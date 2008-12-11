;; vc-clearcase.el --- support for ClearCase version control system
;; Emacs23 version, search for TODO (Emacs23) for problems
;;
;; Copyright (C) 2006, 2007, 2008 Alexandru Harsanyi
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
;;

;; Author: Alexandru Harsanyi (haral@users.sourceforge.net)
;; Created: 28 July 2004
;; Keywords: version-control, clearcase
;; Homepage: http://sourceforge.net/projects/vc-clearcase/

;;; Commentary:
;;
;; vc-clearcase.el is a ClearCase integration package that works as a
;; client to the Emacs VC package.  In addition to the standard VC
;; functionality, this package also allows you to update static views,
;; edit a view's configspec, list checkouts and start the version tree
;; browser GUI on a file.  Once the package is loaded, these
;; additional commands will show up in the Tools/Version Control menu.
;;

;;;; Documentation Notes

;;;;; Installation:
;;
;;
;; 1/ Copy this directory under the site-lisp directory of your GNU/Emacs
;; installation.
;;
;; 2/ Byte compile vc-clearcase.el and ucm.el.  You can do it from inside
;; Emacs, or from the command line:
;;
;;   % emacs -batch -f batch-byte-compile vc-clearcase.el
;;   % emacs -batch -f batch-byte-compile ucm.el
;;
;; 3/ Add the following line to your initialisation file (~/.emacs.el):
;;
;;   (load "vc-clearcase-auto")
;;


;;; Code:

;;;; Initial requires and setup

(require 'tq)
(require 'vc-hooks)
(require 'vc)
(require 'log-view)

;; Bug 1608947: This is needed at runtime for the `find' call.
(require 'cl)

;; This is present in Emacs 22.  If it is available, we provide
;; 'hyperlinks' in the *label-diff-report* buffer.
(unless (featurep 'button)
  (load "button" 'nomessage 'noerror))


(eval-when-compile
  (require 'cc-defs)                    ; for c-point
  (require 'trace) ; avoid compiler complaint w.r.t undefined untrace-function
  )

(defconst vc-clearcase-version "3.0")

(defconst vc-clearcase-maintainer-address "haral@users.sourceforge.net")

;;;###autoload
(defgroup vc-clearcase nil
  "Support for the ClearCase version control system."
  :group 'tools)

;;;###autoload
(defcustom cleartool-program "cleartool"
  "The name of the cleartool executable."
  :type 'string
  :group 'vc-clearcase)

(defcustom clearcase-vtree-program
  (if (eq system-type 'windows-nt)
      "clearvtree"
      "xlsvtree")
  "The name of the Version Tree Browser program."
  :type 'string
  :group 'vc-clearcase)

;;;; Cleartool transaction queue interface

;; ClearCase is slow.  Two techniques are used to improve the
;; responsiveness of Emacs when doing version-control operations.
;; First, cleartool is only started once (via a transaction queue),
;; and all VC commands use it for all the work.  Second, the VC
;; commands try to anticipate what information will be needed and ask
;; cleartool in advance for it.  By the time it is actually needed, it
;; will already be available, or at least the time to wait for it will
;; be shorter.  For example vc-clearcase-registered will also ask for
;; the files version (but does not wait for the answer), by the time
;; vc-clearcase-state-heuristic is called, the information will
;; hopefully be available.
;;
;; The 'user' functions from this section are `cleartool-ask' and
;; `cleartool-wait-for'.


(defvar cleartool-tq nil
  "The transaction queue to cleartool.")

(defvar cleartool-next-command 1
  "Command counter for cleartool commands.

Used to track if someone else is sending commands to cleartool,
or if two commands were sent in one go (e.g. \"cd\nls\n\")")

(defconst cleartool-status-rx
  (concat "Command \\([0-9]+\\) returned status \\([0-9]+\\)[ \t\r\n]+"
	  ;; under Windows NT, we communicate with cleartool using
	  ;; pipes (instead of PTY's, and cleartool won't output a
	  ;; prompt...
	  (unless (eq system-type 'windows-nt)
	    "cleartool \\([0-9]+\\)+>[ \t\r\n]+"))
  "Regexp to match the end of each cleartool result.

If it does not match properly, tq will never pass back the answer
to us." )

(defconst cleartool-question-rx
  "\\(\\[yes\\]\\|\\[no\\]\\)[ \t\r\n]*$"
  "A regexp that matches a question asked by cleartool.")

(defconst cleartool-tq-rx
  (concat "\\(" cleartool-status-rx
	  "\\)\\|\\(" cleartool-question-rx "\\)"))

(defcustom cleartool-timeout 20
  "Timeout (in seconds) for cleartool commands.
This is actually the amount of time cleartool has to be inactive
when receiving data from it, not the total transaction time."
  :type 'integer
  :group 'vc-clearcase)

(defcustom cleartool-idle-timeout 900   ; 15 minutes
  "Kill the cleartool command if idle for this many seconds.
The reason for this variable is that cleartool seems unresponsive
after long periods of inactivity.  Note that cleartool will only
be killed when we try to use it for a new command and the idle
timer has expired."
  :type 'integer
  :group 'vc-clearcase)


(defvar cleartool-last-command-timestamp (float-time)
  "Time-stamp when the last cleartool command was issued.
Used by `cleartool-ask' to know when to restart cleartool.")

(defvar cleartool-ctid 0
  "The ID of the last completed transaction.
This is an incrementing number, any transaction ID that is less
than this value is considered completed.")

(defvar cleartool-ntid 1
  "The next transaction id.
Whenever `cleartool-ask' en-queues a transaction, it increments
this value.")

(defvar cleartool-terr nil
  "Assoc list of (tid . error-message).
Transactions that have errors will have their tid's and error
messages stored in this list.  `cleartool-wait-for' will check
this list and signal an error with the error message." )

(defvar cleartool-tresults nil
  "Assoc list of (tid . answer).
Transactions that don't have a callback function attached, will
have their answer stored here for retrieval by
'cleartool-wait-for'.")

;; Create an error that will be signalled when Cleartool reports an
;; error.  We need it so we can filter errors that come from cleartool
;; itself (which we might want to ignore) and other errors.
(put 'cleartool-error-not-a-vob-object
     'error-conditions
     '(error cleartool-error cleartool-error-not-a-vob-object))
(put 'cleartool-error-not-a-vob-object 'error-message "ClearCase error")

(put 'cleartool-error-label-not-found
     'error-conditions
     '(error cleartool-error cleartool-error-label-not-found))
(put 'cleartool-error-label-not-found 'error-message "ClearCase error")

(put 'cleartool-error
     'error-conditions
     '(error cleartool-error))
(put 'cleartool-error 'error-message "ClearCase error")

(defun cleartool-signal-error (message)
  "Signal a cleartool-error with MESSAGE as an argument."
  ;; Remove the "cleartool: Error: " message as it is annoying and just takes
  ;; up space.
  (while t
    (signal
     (cond ((or (string-match "^cleartool: Error: Not a vob object: " message)
		(string-match "^cleartool: Error: Unable to access " message))
	    'cleartool-error-not-a-vob-object)
	   ((string-match "^cleartool: Error: Label type not found: " message)
	    'cleartool-error-label-not-found)
	   (t
	    'cleartool-error))
     (list
      ;; make the error mesasge look nicer
      (replace-regexp-in-string
       "\n$" ""
       (replace-regexp-in-string
	"\"" "'"
	(replace-regexp-in-string "cleartool: Error: " "" message)))))))

(defun cleartool-tq-sentinel (process event)
  "Sentinel for the cleartool PROCESS.
Cleans up properly if cleartool exits.  EVENT is not used."
  (let ((status  (process-status process))
	(exit-status (process-exit-status process))
	(pbuffer (process-buffer process)))
    (when (memq status '(signal exit))
      (when (null (buffer-name pbuffer))
	(message "Cleartool process buffer was killed"))
      (cleartool-tq-stop)
      (kill-buffer pbuffer))))

(defun cleartool-tq-start ()
  "Start the transaction queue to cleartool."
  (let ((default-directory (expand-file-name "~")))
    (let ((process
	   (start-process "cleartool" " *cleartool*"
			  cleartool-program "-status")))
      (when (not (eq system-type 'windows-nt))
	;; on systems other than windows-nt, cleartool will print a prompt
	;; when it starts up and tq will complain about it.  In these cases,
	;; we wait until the prompt is printed, and start the tq after that.
	(with-timeout (5 (error "Timeout waiting for cleartool to start"))
	  (with-current-buffer (get-buffer " *cleartool*")
	    (goto-char (point-min))
	    (while (not (looking-at "cleartool 1> $"))
	      (accept-process-output process 2)
	      (goto-char (point-min)))
	    (erase-buffer))))
      (set-process-sentinel process 'cleartool-tq-sentinel)
      (set-process-query-on-exit-flag process nil)
      (setq cleartool-tq (tq-create process)
	    cleartool-next-command 1
	    cleartool-last-command-timestamp (float-time)))))


(defcustom cleartool-save-stop-data nil
  "When t, print a report each time cleartool is stopped.
The report is appended to the *cleartool-aborts* buffer."
  :type 'boolean
  :group 'vc-clearcase)

(defun cleartool-tq-stop ()
  "Stop the transaction queue to cleartool and kill cleartool."
  (when cleartool-tq
    (unwind-protect
	 (when cleartool-save-stop-data
	   (with-current-buffer (get-buffer-create "*cleartool-aborts*")
	     (insert "\n\f\n" (current-time-string) "\n"
		     (format "cleartool-ctid: %d\n" cleartool-ctid)
		     (format "cleartool-ntid: %d\n" cleartool-ntid)
		     (format "cleartool-next-command: %d\n"
			     cleartool-next-command))
	     (insert "cleartool-terr:\n")
	     (dolist (x cleartool-terr)
	       (insert (format "    %d    %s\n" (car x) (cdr x))))
	     (insert "\ncleartool-tresults:\n")
	     (dolist (x cleartool-tresults)
	       (insert (format "    %d    %s\n" (car x) (cdr x))))
	     (insert "\ntq-buffer:")
	     (let ((b (tq-buffer cleartool-tq)))
	       (if (and b (buffer-name b))
		   (insert
		    (with-current-buffer (tq-buffer cleartool-tq)
		      (buffer-substring-no-properties (point-min) (point-max))))
		   (insert "*** tq-buffer was killed ***")))))

      ;; Bug 1564792: make sure we run this part even if the code
      ;; above fails.

      (tq-close cleartool-tq)
      (setq cleartool-next-command 0)
      (setq cleartool-tq nil)

      ;; mark all pending transactions as aborted
      (while (< cleartool-ctid (1- cleartool-ntid))
	(incf cleartool-ctid)
	(push (cons cleartool-ctid "cleartool command was aborted")
	      cleartool-terr)))))

(defsubst cleartool-tq-maybe-start ()
  "Start the transaction queue to cleartool, if not already started."
  (unless (and cleartool-tq
	       ;; Bug 1564792: check if someone killed the tq buffer.
	       (let ((b (tq-buffer cleartool-tq)))
		 (if (and b (buffer-name b))
		     t
		     (message "cleartool tq-buffer was killed")
		     nil)))
    (setq cleartool-tq nil)
    (cleartool-tq-start))
  cleartool-tq)

(defun cleartool-tq-handler (closure answer)
  "Handle responses from cleartool-tq.

CLOSURE the closure that was en-queued with `cleartool-ask', it
is a vector containing the transaction id plus the closure and
function that were passed to `cleartool-ask' (the last two
might be null).

ANSWER is the string that was received from cleartool.

The function checks the command index and status received from
cleartool, updates the completed transaction
id ('cleartool-ctid') and either stores the answer in
`cleartool-terr' or `cleartool-tresults' for later
retrieval by `cleartool-wait-for', or calls the function
callback with the answer."

  ;; NOTE: emacs will save the match data, so we can do regexps
  ;; without the need of a save-match-data form.
  (let ((tid (aref closure 0))
	(cb-closure (aref closure 1))
	(cb (aref closure 2)))
    (cond ((string-match cleartool-status-rx answer)
	   (let ((cmd (string-to-number (match-string 1 answer)))
		 (status (string-to-number (match-string 2 answer))))
	     (unless (= cleartool-next-command cmd)
	       ;; transaction queue is out of sync, stop it
	       (cleartool-tq-stop)
	       (error "Unexpected command index received"))
	     ;; it's the command we're expecting
	     (incf cleartool-next-command)
	     (let ((result (replace-match "" t t answer)))
	       (setq cleartool-ctid tid) ; assume tid's always grow
	       (cond ((> status 0)
		      (push (cons tid result) cleartool-terr))
		     (cb                ; do we have a callback function?
		      (funcall cb cb-closure result))
		     (t
		      (push (cons tid result) cleartool-tresults))))))
	  ((string-match cleartool-question-rx answer)
	   (push (cons tid answer) cleartool-terr))
	  (t
	   (error
	    "Answer does not have a status in cleartool-tq-handler")))))

(defun cleartool-wait-for (tid &optional timeout)
  "Wait for TID to complete, return the result or signal an error.

Wait in TIMEOUT seconds intervals, or, if TIMEOUT is nil, wait
`cleartool-timeout' seconds.  If during this time, cleartool
has written something to the output, we wait another interval.
That is, if a transaction takes a very long time to complete, but
cleartool appears to be working, we don't stop it.

If transaction-id has completed, search `cleartool-terr' for
an error message associated with that transaction, and if found,
signals an error.  Otherwise look in `cleartool-tresults' for
a result for the transaction and returns that.  Else return t.

NOTE: a successful transaction might not have a result
associated, as `cleartool-tq-handler' passes the result to the
callback function if that is available."

  ;; (assert tid nil "nil `tid' passed to cleartool-wait-for")
  (unless tid (setq tid -1))

  ;; we use an external loop so that if the with-timeout form exits
  ;; but the process has sent some data we can start the wait again.
  ;; We don't want to abort a cleartool command that is sending us
  ;; lots of data and takes longer than our timeout.
  (while (< cleartool-ctid tid)
    (let (received-some-data
	  (cleartool-process (tq-process cleartool-tq)))
      (with-timeout ((or timeout cleartool-timeout))
	(while (< cleartool-ctid tid)
	  (setq received-some-data
		(or received-some-data
		    ;; will return t if some data was received
		    (accept-process-output cleartool-process 2 0 t)))

	  ;; Hmm, sometimes the input from cleartool is not processed
	  ;; in accept-process-output and we need to call sit-for with
	  ;; a non zero argument.  This occurs when we 'uncheckout' a
	  ;; revision.  It will loop forever (outer while) since
	  ;; `cleartool-tq-handler' is not called to increment
	  ;; `cleartool-ctid'.  There's some race condition here,
	  ;; but I'm not sure what it is.  Do not remove the sit-for
	  ;; call without understanding what it does.  If you remove
	  ;; it, it will work in 99% of the cases and fail
	  ;; mysteriously in 1%.
	  (when (< cleartool-ctid tid)
	    (sit-for 0.5))))

      (when (and (not received-some-data)
		 (< cleartool-ctid tid))
	;; so our transaction is not yet complete and cleartool
	;; hasn't written anything for us.  Assume that cleartool
	;; is hung and kill it.
	(cleartool-tq-stop)
	(cleartool-tq-start)
	(error "Cleartool timed out"))))

  ;; if we are here, the transaction is complete.

  ;; see if we have an error and signal it
  (let ((err (assq tid cleartool-terr)))
    (when err
      (setq cleartool-terr (assq-delete-all tid cleartool-terr))
      (cleartool-signal-error (cdr err))))

  ;; else search for a result
  (let ((result (assq tid cleartool-tresults)))
    (if result
	;; if we have a result, delete it from the list and return it
	(progn
	  (setq cleartool-tresults
		(assq-delete-all tid cleartool-tresults))
	  (cdr result))
	;; else return t, meaning that the transaction is complete but
	;; it returned no data.
	t)))

(defun cleartool-ask (question &optional wait closure fn)
  "En-queue QUESTION to the cleartool-tq.

If WAIT is different than 'nowait, the transaction is waited for
with `cleartool-wait-for' and returns whatever
`cleartool-wait-for' returns.  Otherwise the the transaction
id is returned (you will have to wait for it yourself).  If
CLOSURE and FN are specified, fn will be called when the
transaction is complete as funcall(fn closure answer)."
  (when (> (- (float-time) cleartool-last-command-timestamp)
	   cleartool-idle-timeout)
    (message "Cleartool is idle for too long, restarting...")
    (let ((cleartool-save-stop-data nil))
      ;; stop cleartool without dumping state to the
      ;; *cleartool-aborts* buffer.
      (cleartool-tq-stop))
    (message nil))
  (setq cleartool-last-command-timestamp (float-time))
  (cleartool-tq-maybe-start)
  (let ((tid cleartool-ntid)
	(command (concat question "\n")))
    (incf cleartool-ntid)
    (tq-enqueue cleartool-tq command cleartool-tq-rx
		(vector tid closure fn) 'cleartool-tq-handler)
    (if (eq wait 'nowait)
	tid
	(cleartool-wait-for tid))))

(defsubst cleartool (string &rest objects)
  "Shorthand for (clartool-ask (format STRING OBJECTS))."
  (cleartool-ask (apply 'format string objects)))

(eval-when-compile
  (put 'cleartool 'byte-compile-format-like t))

;;;; Cleartool sub-process interface

;; For cleartool commands that take longer to complete that it takes
;; cleartool to start, we use a sub-process interface.  This will start
;; a cleartool process and put the command output in a buffer.
;;
;; The 'user' function in this section is cleartool-do.

(defvar cleartool-mode-line nil
  "Modeline argument for cleartool commands.")

(defvar cleartool-finished-function nil
  "Function to be called when the cleartool process finishes.")

(defvar cleartool-kill-buffer-when-done nil
  "When t, kill process buffer when cleartool exits.")

(defvar cleartool-last-command nil
  "The last command given to cleartool to create this output buffer.")

(dolist (var '(cleartool-mode-line
	       cleartool-finished-function
	       cleartool-kill-buffer-when-done
	       cleartool-last-command))
  (make-variable-buffer-local var)
  (put var 'permanent-local t))

(defun cleartool-sentinel (process event)
  "Process sentinel for cleartool PROCESS commands.
Updates the modeline when the cleartool command finishes, calls
`cleartool-finished-callback' and kills the process buffer when
`cleartool-kill-buffer-when-done' is set.  EVENT is not used."
  (let ((status  (process-status process))
	(exit-status (process-exit-status process))
	(pbuffer (process-buffer process)))
    (when (memq status '(signal exit))
      (if (null (buffer-name pbuffer))
	  (message "Cleartool process buffer was killed")
	  (with-current-buffer pbuffer
	    (setq cleartool-mode-line
		  (format "%s [%d]" (symbol-name status) exit-status))
	    (force-mode-line-update)
	    (when cleartool-finished-function
	      (funcall cleartool-finished-function))
	    (delete-process process)
	    (when cleartool-kill-buffer-when-done
	      (kill-buffer nil)))))))


(defun cleartool-do (cmd args buffer)
  "Run the cleartool CMD with ARGS and put the result in BUFFER.

Command is a cleartool command, that is the actual command run is
\"cleartool cmd args\".

The arguments need to be a list of strings as in execv(2) call.
This is different from the arguments to `cleartool-ask'.

This command only starts the process and returns it.  The process
will continue to run and fill buffer.  If you want to be notified
when the process is finished, setup a callback function in
`cleartool-finished-function' (see below.)  See also
`clearcase-version' for a command that waits for the result of
the cleartool command.

The sentinel for the resulting process inspects the following
buffer local variables in the process buffer:

`cleartool-finished-function' -- function to call when the
cleartool command has finished.

`cleartool-kill-buffer-when-done' -- when t, the buffer will
be killed when the cleartool command has finished.

In addition, the buffer local variable
`cleartool-last-command' is set to the command and arguments
that were run to create this buffer."
  (let ((name (format "cleartool-%s" cmd))
	(args1 (cons cmd args)))
    (let ((process (apply 'start-process name buffer
			  cleartool-program args1)))
      (with-current-buffer (process-buffer process)
	(set-process-sentinel process 'cleartool-sentinel)
	(setq cleartool-mode-line "run")
	(setq mode-line-process '(" cleartool: " cleartool-mode-line))
	(force-mode-line-update)
	(setq cleartool-last-command (copy-sequence args1))
	process))))

;;;; Some helper macros

(defun clearcase-register-path (path)
  "Register PATH and all its parents with ClearCase.
If PATH is already registered, do nothing.  Return a list of all
the directories that were checked out (the caller is responsible
for checking them back in)."
  (let ((dir (directory-file-name path))
	(elements nil)
	(need-checkin? nil))

    (while (equal "" (cleartool "desc -fmt \"%%Vn\" \"%s\"" dir))
      ;; dir is not registered
      (push dir elements)
      (let ((parent (file-name-directory dir)))
	(when (equal parent dir)        ; reached toplevel
	  (error "%s is not inside a ClearCase view" dir))
	(setq dir (directory-file-name parent))))

    ;; DIR now contains the element we neeed to checkout, ELEMENTS contain all
    ;; the elements we need to register

    (when elements         ; if DIR was already registered, don't check it out
      (when (equal (cleartool "desc -fmt \"%%Rf\" \"%s\"" dir) "")
	(cleartool "co -nquery -ptime -nwarn -reserved -nc \"%s\"" dir)
	(setq need-checkin? t))
      (dolist (e elements)
	(cleartool "mkelem -nc \"%s\"" e)))

    ;; Return the elements in the order they can be uncheckedout without
    ;; errors (checking them in works in any order).
    (nreverse
     (if need-checkin?
	 (cons dir elements)
	 elements))))

(defmacro with-clearcase-checkout (elem &rest body)
  "Ensure that ELEM is checked out, than execute BODY.
If ELEM is not registered, register it and all its parents.  If
BODY succeeds, we checkin all the checkouts we made, if BODY
fails, we undo them. ELEM can be either a file or a directory."
  (declare (debug (form &rest form)) (indent 1))
  (let ((checkouts (make-symbol "checkouts"))
	(path (make-symbol "path"))
	(succeded? (make-symbol "succeded?")))
    `(let* ((,path (expand-file-name ,elem))
	    (,checkouts nil)    ; a list of all directories we had to checkout
	    (,succeded? nil))
       (unwind-protect
	    (progn
	      (condition-case error
		  (when (equal (cleartool "desc -fmt \"%%Rf\" \"%s\"" ,path) "")
		    (cleartool "co -nquery -ptime -nwarn -reserved -nc \"%s\"" ,path)
		    (setq ,checkouts (list ,path)))
		(cleartool-error-not-a-vob-object
		 ;; path is not registered with clearcase
		 (setq ,checkouts (clearcase-register-path ,path))))
	      ,@body
	      (setq ,succeded? t))
	 (if ,succeded?
	     (dolist (e ,checkouts)
	       (cleartool "checkin -ptime -nwarn -nc \"%s\"" e))
	     (dolist (e ,checkouts)
	       (cleartool "uncheckout \"%s\"" e)))))))

(defmacro with-clearcase-cfile (comment-vars &rest body)
  "Save a comment in a temporary file, than execute BODY.
COMMENT-VARS is a list of (comment-file comment-text),
comment-file will be bound to a temporary file name and
comment-text will be saved into it.  When all is finished, the
comment file is removed."
  (declare (debug ((symbolp form) &rest form)) (indent 1))
  ;; NOTE: we could have used
  ;; (defmacro* with-cleacase-cfile ((comment-file comment-text) &body forms)
  ;;    ;; blah blah)
  ;; but we didn't
  (destructuring-bind (cfile ctext) comment-vars
    (assert (symbolp cfile) 'show-args "Expecting a symbol %S")
    `(let ((,cfile
	    (make-temp-name (concat temporary-file-directory "vc-clearcase-"))))
       (unwind-protect
	    (progn
	      (with-temp-file ,cfile
		(insert ,ctext))        ; ctext evaluated once, here
	      ,@body)
	 (delete-file ,cfile)))))

(defmacro with-cleartool-directory (dir &rest body)
  "Change the cleartool directory to DIR, than execute BODY.
The original cleartool directory is restored after BODY is
executed."
  (declare (debug (form &rest form)) (indent 1))
  (let ((old-dir (make-symbol "old-dir"))
	(new-dir (make-symbol "new-dir")))
    `(let ((,old-dir (replace-regexp-in-string
		      "[\n\r]+" "" (cleartool-ask "pwd")))
	   (,new-dir ,dir))             ; dir is evaluated once, here
       (unless (file-directory-p ,new-dir)
	 (error "with-cleartool-directory: not a directory: %s" ,new-dir))
       (unwind-protect
	    (progn
	      (cleartool "cd \"%s\"" (expand-file-name ,new-dir))
	      ,@body)
	 (cleartool "cd \"%s\"" ,old-dir)))))

(defmacro ignore-cleartool-errors (&rest body)
  "Execute BODY, ignoring any cleartool error.
The form returns nil if a cleartool error was signalled,
otherwise it returns the value of the last form in BODY."
  (declare (debug (&rest form)) (indent 0))
  `(condition-case nil
       (progn ,@body)
     (cleartool-error nil)))


;;;; Clearcase file properties

;; Rather than keeping all the version information as properties
;; accessible via vc-file-{get/set}prop, we define a structure to hold
;; all the information and store it in as the 'vc-clearcase-prop
;; property of a file.  We also define some pseudo structure members
;; in the form of functions starting with clearcase-fprop- -- they
;; return derived information from a fprop.

(defstruct (clearcase-fprop
	     (:constructor clearcase-make-fprop)
	     (:copier clearcase-copy-fprop))

  file-name                             ; the file name this fprop belongs to

  version-tid
  version               ; current file revision
  parent                ; parent revision
  status                ; nil, 'reserved, 'unreserved, 'hijacked, 'broken-view
  what-rule             ; confispec rule for the file

  ;; the checkout comment (when checked out).  Use `clearcase-fprop-comment'
  ;; to access it.  NOTE: the comment will only be available right before a
  ;; checkin when `vc-clearcase-state' is called.
  comment-tid^
  comment^

  ;; the activity attached to the file (when checked out, only in UCM views).
  ;; Use `clearcase-fprop-activity' to access it.  NOTE: the activity will
  ;; only be available right before a checkin when `vc-clearcase-state' is
  ;; called.
  activity-tid^
  activity^

  view-tag                              ; the view for the file

  ;; a list of all the revisions of this file, starting from \main\0
  ;; all the way to latest.  Used by vc-clearcase-next-version to
  ;; speed up the search.
  revision-list
  )

(defsubst clearcase-file-fprop (file)
  "Return the fprop structure associated with FILE."
  (vc-file-getprop (expand-file-name file) 'vc-clearcase-fprop))

(defsubst clearcase-fprop-initialized-p (fprop)
  "Return true if FPROP is initialized.
FPROP can be nil, meaning it is not initialized."
  ;; we use an if form to return t or nil instead of the version
  ;; string or tid.
  (if (and fprop
	   (or (clearcase-fprop-version fprop)
	       (clearcase-fprop-version-tid fprop)))
      t
      nil))

(defsubst clearcase-fprop-hijacked-p (fprop)
  "Return true if FPROP is hijacked."
  (eq (clearcase-fprop-status fprop) 'hijacked))

(defsubst clearcase-fprop-checkedout-p (fprop)
  "Return the checked out mode for FPROP or nil."
  (memq (clearcase-fprop-status fprop) '(reserved unreserved)))

(defsubst clearcase-fprop-broken-view-p (fprop)
  "Return true if the there's a problem with this FPROP in the view.
This can happen in snapshot views, occasionally cleartool reports
that another process does an update and refuses to operate on the
files.  The solution to the problem is to run an update on the
whole view, but it is beyond the scope of this FPROP."
  (eq (clearcase-fprop-status fprop) 'broken-view))

(defsubst clearcase-fprop-checkout-will-branch-p (fprop)
  "Return true if a checkout will create a branch on this FPROP.
The branch creation might still fail if the branch already exists
somewhere in the version-tree of this element.  So what we really
check is whether ClearCase will try to branch this file at
checkout."
  (string-match "-mkbranch\\>" (clearcase-fprop-what-rule fprop)))

(defsubst clearcase-fprop-checkout-denied-p (fprop)
  "Return true if checkouts are not permited on this FPROP.
This is indicated by a -nocheckout directive in the configspec
rule for the file."
  (string-match "-nocheckout\\>" (clearcase-fprop-what-rule fprop)))

(defun clearcase-fprop-branch (fprop)
  "Return the branch part of FPROP.
This is the second last element in version path."
  (let ((version (clearcase-fprop-version fprop)))
    (nth 1 (nreverse (split-string version "[\\\\/]")))))

(defun clearcase-fprop-version-base (fprop)
  "Return the version of FPROP minus the last element."
  (let ((version (copy-sequence (clearcase-fprop-version fprop))))
    (when (string-match "[\\\\/][^\\\\/]*$" version)
      (replace-match "" t t version))))

(defun clearcase-fprop-version-number (fprop)
  "Return the version number of FPROP (last element in version path).
If the file is checked out, the version is '.../CHECKEDOUT', in
that case, we return the version of the parent."
  (let ((version (clearcase-fprop-version fprop)))
    (when (string-match "[\\\\/]\\([^\\\\/]*\\)$" version)
      (match-string 1 version))))

(defsubst clearcase-fprop-latest-sel (fprop)
  "Return a version selector for the latest version of FPROP."
  (format "%s/LATEST" (clearcase-fprop-version-base fprop)))

(defun clearcase-fprop-latest (fprop)
  "Return the latest version of FPROP on the current branch."
  (cleartool "desc -fmt \"%%Vn\" \"%s@@%s\""
	     (clearcase-fprop-file-name fprop)
	     (clearcase-fprop-latest-sel fprop)))

(defun clearcase-reset-fprop (fprop)
  "Clear the version fields in FPROP.
This will mark fprop as not initialized for the functions that
care about this.  This function accepts a nil fprop (in which
case it does nothing), to the user can reset a file's fprop
without having to check first that it exists."
  (when fprop
    (setf (clearcase-fprop-version fprop) nil)
    (setf (clearcase-fprop-version-tid fprop) nil)
    (setf (clearcase-fprop-comment-tid^ fprop) nil)
    (setf (clearcase-fprop-comment^ fprop) nil)
    (setf (clearcase-fprop-activity-tid^ fprop) nil)
    (setf (clearcase-fprop-activity^ fprop) nil)
    (setf (clearcase-fprop-revision-list fprop) nil)))

(defsubst clearcase-fprop-comment (fprop)
  "Return the checkout comment of this file."
  (cleartool-wait-for (clearcase-fprop-comment-tid^ fprop))
  (clearcase-fprop-comment^ fprop))

(defsubst clearcase-fprop-activity (fprop)
  "Return the activtiy of a checked out file."
  (cleartool-wait-for (clearcase-fprop-activity-tid^ fprop))
  (clearcase-fprop-activity^ fprop))

(defun clearcase-set-fprop-version-stage-1 (fprop ls-string)
  "Set version information in FPROP from LS-STRING.
Ls-string is returned by a 'cleartool ls file' command.  From it,
we determine the configspec rule, the initial version of the file
and whether the file is hijacked or in a broken view.

Note that if the file is checked out, the revision will end in
/CHECKEDOUT, which is not a valid revision for vc.el
semantics (for example it cannot be used for diffing purposes).
In that case, the version will be adjusted in
`clearcase-set-fprop-version-stage-2'"
  (when (string-match "Rule: \\(.*\\)$" ls-string)
    (setf (clearcase-fprop-what-rule fprop) (match-string 1 ls-string)))
  (when (string-match "@@\\([^ \t]+\\)" ls-string)
    (let ((fver (match-string 1 ls-string)))
      (setf (clearcase-fprop-version fprop) fver)))
  ;; The ls string will also tell us when something is wrong with the
  ;; file.
  (setf (clearcase-fprop-status fprop)
	(cond ((string-match "hijacked" ls-string) 'hijacked)
	      ((string-match "rule info unavailable" ls-string) 'broken-view)
	      (t nil))))

(defun clearcase-set-fprop-version-stage-2 (fprop version-string)
  "Set version information in FPROP from VERSION-STRING.
Version string is returned by the following command:

    cleartool desc -fmt \"%Vn %PVn %Rf\" file

From it we determine the parent revision and the checkout status
of the file.  If the file is checked out, we set its version to
the parent version, to conform to vc.el semantics."
  (let ((fver-raw (split-string version-string)))
    (let ((fver (nth 0 fver-raw))       ; file version
	  (pver (nth 1 fver-raw))       ; parent version
	  (co-mode (let ((co-mode-raw (nth 2 fver-raw)))
		     (cond ((null co-mode-raw) nil)
			   ((string= co-mode-raw "reserved") 'reserved)
			   ((string= co-mode-raw "unreserved") 'unreserved)
			   (t 'unknown)))))
      (when co-mode
	;; The semantics of vc.el requires that the workfile version
	;; be the parent version if the file is checked out.
	(setf (clearcase-fprop-version fprop) pver))
      (setf (clearcase-fprop-parent fprop) pver)
      ;; a hijacked file should keep its existing checkout status and
      ;; modeline (set by clearcase-set-fprop-version-stage-1)
      (unless (memq (clearcase-fprop-status fprop) '(hijacked broken-view))
	(setf (clearcase-fprop-status fprop) co-mode)))))

(defun clearcase-refresh-files (files)
  "Refresh the status of FILES.
This is used to update files when ClearCase commands change them
in bulk."
  (dolist (file files)
    (let ((fprop (clearcase-file-fprop file)))
      (when fprop
	(with-temp-message (format "Refreshing ClearCase status for %s" file)
	  (with-current-buffer (get-file-buffer file)
	    (revert-buffer nil 'noconfirm nil)))))))

;;;; Clearcase view-tag properties

(defstruct (clearcase-vprop
	     (:constructor clearcase-make-vprop)
	     (:copier clearcase-copy-vprop))
  name
  root-path                             ; for snapshot views only
  stream                                ; the UCM stream or nil
  properties                            ; list of 'snapshot 'dynamic 'ucmview
  (activities '("*NONE*" "*NEW-ACTIVITY*")) ; list of UCM activities in this stream
  (activities-tid -1)                  ; transaction id for activity retrieval
  )

(defvar clearcase-all-vprops '())

(defsubst clearcase-snapshot-view-p (view)
  "Return t if VIEW is a snapshot view.
VIEW can be either a view name (a string) a vprop or a fprop"
  (let ((vprop (clearcase-get-vprop view)))
    (not (null (memq 'snapshot (clearcase-vprop-properties vprop))))))

(defsubst clearcase-dynamic-view-p (view)
  "Return t if VIEW is a dynamic view.
VIEW can be either a view name (a string) a vprop or a fprop"
  (let ((vprop (clearcase-get-vprop view)))
    (not (null (memq 'dynamic (clearcase-vprop-properties vprop))))))

(defsubst clearcase-ucm-view-p (view)
  "Return t if VIEW is a ucm view.
VIEW can be either a view name (a string) a vprop or a fprop."
  (let ((vprop (clearcase-get-vprop view)))
    (not (null (memq 'ucmview (clearcase-vprop-properties vprop))))))

(defun clearcase-setup-vprop (vprop dir)
  "Setup some properties in VPROP, if they are not already set.
We set the view properties plus the stream name for UCM views.
If DIR is not nil it is a directory inside the view, we use it to
determine the root path for a snapshot view."

  ;; WARNING: we assume that DIR is a directory inside the view VPROP, but we
  ;; don't check.  This can lead to problems when DIR is inside a different
  ;; view...

  (with-cleartool-directory dir
    (unless (clearcase-vprop-properties vprop)
      (let* ((view-tag (clearcase-vprop-name vprop))
	     (vdata (cleartool "lsview -properties -full %s" view-tag))
	     (case-fold-search t))
	(if (string-match "^\\s-*properties:\\s-*\\(.*\\)\\s-*$" vdata)
	    (setf (clearcase-vprop-properties vprop)
		  (mapcar 'intern (split-string (downcase (match-string 1 vdata)))))
	    ;; should it be an error?
	    (message "clearcase-get-vprop: no props for %s" view-tag))

	(when (clearcase-ucm-view-p vprop)
	  (setf (clearcase-vprop-stream vprop)
		(cleartool "lsstream -obsolete -fmt \"%%n\" -view %s" view-tag))))

      (when (and dir
		 (null (clearcase-vprop-root-path vprop))
		 (clearcase-snapshot-view-p vprop))
	(setf (clearcase-vprop-root-path vprop)
	      (replace-regexp-in-string "[\n\r]+" "" (cleartool "pwv -root"))))))
  vprop)


(defun clearcase-get-vprop (view-tag &optional dir)
  "Return the vprop struct associated with VIEW-TAG.
The vprop structure is created if needed.  DIR, if not nil is a
directory inside the view.  It is used to set the view root path
in snapshot views.

VIEW-TAG can be (1) a vprop, in which case it is returned; (2) a
string in which case a vprop with that name is looked up or
created; (3) a fprop, in which case its view-tag looked up
using (2).

If the VPROP has to be created, some properties will be set by
asking cleartool for information.  See
`clearcase-setup-vprop'."

  ;; Guard against the case where the result of a "pwv -short" in a non
  ;; ClearCase directory is passed to us.
  (when (equal view-tag "** NONE **")
    (error "not a valid view-tag: %s" view-tag))

  (if (clearcase-vprop-p view-tag)
      view-tag                          ; case 1/
      (let* ((vtag (cond ((stringp view-tag) view-tag)
			 ((clearcase-fprop-p view-tag)
			  (clearcase-fprop-view-tag view-tag))
			 (t (error "Unknown type for VIEW-TAG"))))
	     (vprop (find vtag clearcase-all-vprops
			  :key 'clearcase-vprop-name :test 'equal)))
	(unless vprop
	  (assert dir)
	  (setq vprop (clearcase-setup-vprop
		       (clearcase-make-vprop :name vtag)
		       dir))
	  (push vprop clearcase-all-vprops))
	vprop)))

(defun clearcase-refresh-files-in-view (&optional view-tag)
  "Refresh ClearCase info for all visited files from VIEW-TAG.
VIEW-TAG can be a VPROP, a view name or nil.  When nil ClearCase
info all visited files is refreshed.

This function useful when the view changes, such as by a setcs or
update command or when an UCM activity is checked in."
  (when (and view-tag (clearcase-vprop-p view-tag))
    (setq view-tag (clearcase-vprop-name view-tag)))
  (dolist (buffer (buffer-list))
    (let ((file (buffer-file-name buffer)))
      ;; ignore modified buffers, don't rob the user from the joy of figuring
      ;; out that he just changed the view and he had modified files in it...
      ;; also ignore buffers for which the file no longer exists (yes it can
      ;; happen.  See `vc-delete-file')
      (when (and file
		 (file-exists-p file)
		 (not (buffer-modified-p buffer)))
	(let ((fprop (clearcase-file-fprop file)))
	  (when fprop
	    (let ((vtag (clearcase-fprop-view-tag fprop)))
	      (when (or (null view-tag) (string= vtag view-tag))
		;; clear any properties vc.el might have cached.
		(vc-file-clearprops file)
		(vc-file-setprop file 'vc-clearcase-fprop fprop)
		(clearcase-maybe-set-vc-state file 'force)
		(vc-resynch-buffer file t t)))))))))

;;;; Read a view-tag from the minibuffer with completion

;;; Since the number of accessible views can be quite large (2607 of
;;; them on my site, according to 'cleartool lsview -short | wc -l'),
;;; we ask cleartool for the list of views in the background and
;;; prompt the user for the view name.  Completion will become
;;; available when the command completes, but the user can start
;;; typing immediately.  In addition, we don't remove the old view
;;; obarray so, on a second read, the user can use the old list for
;;; completion, until the new list becomes available -- this is ok
;;; considering that the list of views does not change very often.

(defvar clearcase-edcs-all-view-tags nil
  "An obarray of all known view-tags (stored as symbols).")

(defvar clearcase-edcs-all-view-tags-tid -1
  "Transaction ID to wait for fetching all view-tags.")

(defun clearcase-read-view-tag (prompt &optional initial)
  "Read a view tag from the minibuffer and return it.
PROMPT is displayed to the user; INITIAL, when non-nil is the
initial view tag name presented to the user.

This function will provide a `completing-read' with the list of
available view tags in the system.  It does however read the view
tags asynchronously so they might not be available immediately as
the user hits the TAB key.

This implementation was chosen to improve responsiveness, if the
user wants to accept INITIAL or wants to type in the name of the
view, he can do so without waiting for the full list of view tags
to be read from cleartool."

  ;; wait for the previous read view-tags transaction
  (cleartool-wait-for clearcase-edcs-all-view-tags-tid)

  ;; Start reading the view-tags asynchronously. By the time the user decides
  ;; what view-tag it wants, we may have the answer already.  Note that the
  ;; previous view tag list still exists and the user can perform completions
  ;; form that one.
  (setq clearcase-edcs-all-view-tags-tid
	(cleartool-ask
	 "lsview -short" 'nowait nil
	 '(lambda (x view-tags)
	   (setq clearcase-edcs-all-view-tags (make-vector 31 0))
	   (dolist (vtag (split-string view-tags "[\n\r]+"))
	     (intern vtag clearcase-edcs-all-view-tags)))))

  (completing-read
   prompt
   '(lambda (string predicate flag)
     (let ((fn (cond ((eq flag t) 'all-completions)
		     ((eq flag 'lambda) 'test-completion
		     (t 'try-completion))))
       (funcall fn string clearcase-edcs-all-view-tags predicate))))
   nil
   nil
   initial))

;;;; Read a label form the minibuffer with completion.

;;; Since the number of labels in a VOB can be quite large (17076 of them
;;; according to 'cleartool lstype -kind lbtype | wc -l') We use the same idea
;;; as for reading the view-tags, with the following exceptions:
;;;
;;; 1/ We cannot use the cleartool lstype command as it is too slow.  We must
;;; use a cleartool dump command and parse its output.
;;;
;;; 2/ Since the output of the dump command is large, we don't use the
;;; transaction queue to cleartool or the sub-process interface, instead we
;;; start the cleartool process ourselves and filter its output.
;;;
;;; The list of labels is reset at each read, and populated on the fly.
;;; During the first few seconds of the read, not all the labels will be
;;; available.

(defun clearcase-vob-tag-for-path (path)
  "Return the vob tag in which PATH resides.
This can be used to obtain the vob-tag required for the
`clearcase-read-label' function."
  (setq path (expand-file-name path))
  (with-cleartool-directory
      (if (file-directory-p path) path (file-name-directory path))
    (let ((view-root (cleartool "pwv -root")))
      (let ((path-elements (split-string path "[\\\\\\/]"))
	    (prefix-length (length (split-string view-root "[\\\\\\/]"))))
	;; On Windows, the vob tag looks like "/Vob_Name", on Solaris, it is
	;; "/vobs/Vob_Name".
	(concat "/" (nth prefix-length path-elements)
		(unless (eq system-type 'windows-nt)
		  (concat "/" (nth (1+ prefix-length) path-elements))))))))

(defvar clearcase-all-labels nil
  "An obarray containing all labels (stored as symbols).")

(defvar clearcase-collect-labels-point nil
  "The position in the buffer where we left the processing.")

(defvar clearcase-collect-labels-finished nil
  "Becomes t when we finished processing the cleartool dump output.
Used by `clearcase-collect-labels-sentinel' and
`clearcase-collect-labels-filter' to synchronise themselves.")

(defun clearcase-collect-labels-sentinel (process event)
  "Sentinel for the cleartool dump command.
Cleans up after cleartool exits."
  (when (memq (process-status process) '(signal exit))
    (let ((buffer (process-buffer process)))
      (unless (= (process-exit-status process) 0)
	(message "non-zero exit code form cleartool while reading labels"))
      (unless (null (buffer-name buffer))
	(with-current-buffer buffer
	  (when clearcase-collect-labels-finished
	    ;; if we finished collecting labels from the cleartool
	    ;; output, kill the process buffer.
	    (kill-buffer buffer)))))))

(defun clearcase-collect-labels-filter (process string)
  "The filter function for the cleartool dump command.
Parses the output and populates clearcase-all-labels with the
labels it finds."
  (with-current-buffer (process-buffer process)
    ;; We should be the only ones controlling the buffer, we don't
    ;; save-excursion.

    (goto-char (point-max))
    (insert string)

    ;; if we haven't found the start of the label section yet, look
    ;; for it.
    (when (null clearcase-collect-labels-point)
      (goto-char (point-min))
      (when (re-search-forward "^label type objects:" nil 'noerror)
	(forward-line 1)
	(setq clearcase-collect-labels-point (point))))

    (unless (null clearcase-collect-labels-point)
      (goto-char clearcase-collect-labels-point)
      (let ((limit
	     (re-search-forward "^[a-zA-Z]\\(:?.*\\):\\s *$" nil 'noerror)))
	(goto-char clearcase-collect-labels-point)
	(catch 'finished
	  (while t
	    (forward-line 1)
	    (let ((next-line (point)))
	      (when (or (eq next-line clearcase-collect-labels-point)
			(and limit (>= next-line limit)))
		(throw 'finished t))
	      ;; if we are here, we have a complete line
	      (forward-line -1)
	      (when (looking-at "^\\s +[0-9]+\\s +\\([-_.a-zA-Z0-9]+\\)")
		(intern (match-string 1) clearcase-all-labels))
	      (goto-char (setq clearcase-collect-labels-point next-line)))))

	(if limit
	    ;; Cleartool finished the labels section.  If cleartool
	    ;; terminated, we kill the buffer, otherwise we set the
	    ;; process property 'clearcase-collect-labels-done to t
	    (if (eq (process-status process) 'run)
		(setq clearcase-collect-labels-finished t)
		;; the process has finished, kill it
		(progn
		  (unless (= (process-exit-status process) 0)
		    (message
		     "non-zero exit code form cleartool while reading labels"))
		  (kill-buffer (current-buffer))))
	    ;; cleartool is still sending us data...
	    (progn
	      (delete-region (point-min) clearcase-collect-labels-point)
	      (setq clearcase-collect-labels-point (point-min))))))))

(defun clearcase-collect-labels-for-vob (vob)
  "Start the process of collecting the labels in VOB.
The labels will become available as
`clearcase-collect-labels-filter' parses them."
  (let* ((buffer (generate-new-buffer "*cleartool-dump-vob*"))
	 (process (start-process "cleartool-dump-vob" buffer
				 cleartool-program
				 "dump" "-long" (concat "vob:" vob))))
    (with-current-buffer buffer
      (buffer-disable-undo)
      (setq clearcase-all-labels (make-vector 63 0))
      (set (make-local-variable 'clearcase-collect-labels-point) nil)
      (set (make-local-variable 'clearcase-collect-labels-finished) nil)
      (set-process-filter process 'clearcase-collect-labels-filter)
      (set-process-sentinel process 'clearcase-collect-labels-sentinel))))

(defun clearcase-read-label (prompt vob &optional initial reuse-labels)
  "Read a label from the minibuffer and return it.

Display PROMPT to the user and read a ClearCase label using the labels
of VOB as possible completions.  When non-nil, INITIAL, is the initial
label name presented to the user.

Before prompting the user, an asynchronous cleartool dump command will
be started to fetch the list of labels.  The list of labels is
populated incrementally, so completion is provided from an incomplete
list for the first few seconds.  This implementation has been chosen
to improve responsiveness, but it can be quite annoying.

When REUSE-LABELS is non-nil, the previous list of labels will be used
for completion, without starting a cleartool dump command.  This
option should be used when a function needs to read several labels
from the user, in which case starting several cleartool commands is a
waste of resources."
  (unless reuse-labels
    (clearcase-collect-labels-for-vob vob))
  (completing-read
   prompt
   '(lambda (string predicate flag)
     (let ((fn (cond ((eq flag t) 'all-completions)
		     ((eq flag 'lambda) 'test-completion)
		     (t 'try-completion))))
       (funcall fn string clearcase-all-labels predicate)))
   nil
   nil
   initial))

;;;; Some helpers functions

(defun clearcase-get-keep-file-name (file-name)
  "Return a file name which can be used as a 'keep' file for FILE-NAME.
ClearCase creates backup files with the string .keep plus a
number appended to the original file name.  We keep the same
convention when we need to create a backup.

We try to append .keep, .keep.1, .keep.2 to FILE-NAME until we
find a file which does not exist and return that one.  This
method is open to race conditions, but it seems to be what
ClearCase uses."
  ;; first try appending .keep
  (let ((keep-file (format "%s.keep" file-name))
	(n 0))
    (while (file-exists-p keep-file)
      (incf n)
      (setq keep-file (format "%s.keep.%d" file-name n)))
    keep-file))

(defun clearcase-revision-contributors (file revision)
  "Return the revisions which were merged into FILE's REVISION."
  (assert (vc-clearcase-registered file))
  (let ((merge-links (cleartool "desc -short -ahlink Merge \"%s@@%s\"" file revision))
	result)
    (dolist (merge-link (split-string merge-links "[\n\r]"))
      (when (string-match "^<- " merge-link) ; "Merge From" arrow
	(when (string-match "@@\\(.*\\)" merge-link)
	  (let ((merged-revision (match-string 1 merge-link)))
	    (push merged-revision result)))))
    (nreverse result)))

(defun clearcase-get-attributes (object)
  "Return the attributes attached to a ClearCase object as an ALIST."
  (let ((data (cleartool "desc -fmt \"%%a\" %s" object))
	(pos 0)
	(case-fold-search t)
	(attributes '()))
    (while (string-match "(?\\([a-z0-9_-]+\\)=\\(\"?\\)\\(.*?\\)\\2\\(:?)\\|, \\)" data pos)
      (push (cons (intern (match-string 1 data)) (match-string 3 data)) attributes)
      (setq pos (match-end 0)))
    attributes))

(defun clearcase-maybe-set-vc-state (file &optional force)
  "Lazily set the clearcase specific properties of FILE.
If FORCE is not nil, always read the properties."
  (let ((fprop (clearcase-file-fprop file)))
    (when force (clearcase-reset-fprop fprop))
    (when (or (clearcase-fprop-initialized-p fprop)
	      (vc-clearcase-registered file))
      (unless fprop (setq fprop (clearcase-file-fprop file)))
      (cleartool-wait-for (clearcase-fprop-version-tid fprop))

      ;; get the view tag for this fprop.  Ignore the FORCE option, as
      ;; we don't expect the view tag to ever change.
      (unless (clearcase-fprop-view-tag fprop)
	(with-cleartool-directory (file-name-directory file)
	  (setf (clearcase-fprop-view-tag fprop)
		(replace-regexp-in-string
		 "[\n\r]+" "" (cleartool "pwv -short"))))
	;; this will create the proper vprop structure (unless already
	;; created).
	(clearcase-get-vprop fprop (file-name-directory file)))

      ;; When the file is checked out, we need some additional info
      (when (clearcase-fprop-checkedout-p fprop)
        ;; We anticipate that the file's checkout comment might be needed
        ;; shortly so ask for it before we return the state
        (unless (clearcase-fprop-comment-tid^ fprop)
          (setf (clearcase-fprop-comment-tid^ fprop)
                (cleartool-ask
                 (format "desc -fmt \"%%c\" \"%s\"" file)
                 'nowait fprop
                 (lambda (fprop comment)
                   (setf (clearcase-fprop-comment^ fprop) comment)))))

        ;; In UCM views also ask for the files activity.  This is not used by
        ;; vc-clearcase.el for now, but it enables some checkin-hooks to be
        ;; more responsive.
        (when (and (clearcase-ucm-view-p fprop)
                   (null (clearcase-fprop-activity-tid^ fprop)))
          (setf (clearcase-fprop-activity-tid^ fprop)
                (cleartool-ask
                 (format "desc -fmt \"%%[activity]p\" \"%s\""
                         (clearcase-fprop-file-name fprop))
                 'nowait fprop
                 (lambda (fprop activity)
                   (setf (clearcase-fprop-activity^ fprop) activity)))))))))

(defadvice vc-version-backup-file-name
    (after clearcase-cleanup-version (file &optional rev manual regexp))
  "Cleanup rev of \\ and / so it can be stored as a file name."
  (when (string-match "~.*~" ad-return-value)
    (let ((start (match-beginning 0))
	  (data (match-string 0 ad-return-value)))
      (setq data (replace-regexp-in-string "[\\\\/]" "_" data))
      (setq ad-return-value
	    (concat (substring ad-return-value 0 start) data))))
  ad-return-value)

(defadvice vc-start-logentry
    (before clearcase-prepare-checkin-comment
	    (files extra comment initial-contents msg logbuf action &optional after-hook))
  "Insert the checkout comment when checking-in FILE."

  ;; When `vc-start-logentry' wants to collect a log message from the user and
  ;; checkin FILE, we hook in to provide the checkout comment as the default.
  ;;
  ;; We must NOT touch COMMENT in these cases:
  ;;
  ;; - When FILE is nil (called from vc-dired to provide a checkin comment for
  ;;   all files)
  ;;
  ;; - When COMMENT is NOT nil (it can be a string or t), this means
  ;;   `vc-start-logentry' does not intend to prompt the user.
  ;;
  ;; Of course, we also check that the file is a ClearCase file and that it is
  ;; checked out (therefore it will be checked in)

  (when (and files (= (length files) 1) (null comment))
    (let ((fprop (clearcase-file-fprop (car files))))
      (when (and fprop (clearcase-fprop-checkedout-p fprop))
	(setf comment (clearcase-fprop-comment fprop))
	(setf initial-contents t)))))

(defadvice vc-create-snapshot
    (before clearcase-provide-label-completion first
	    (dir name branchp))
  "Override the interactive form so that we have label completion."
  (interactive
   (let* ((d (read-file-name "Directory: "
			     default-directory default-directory t))
	  (vob (ignore-cleartool-errors
		 (clearcase-vob-tag-for-path
		  (if (file-directory-p d) d (file-name-directory d))))))
     (list d (if vob
		 (clearcase-read-label "Label: " vob)
		 (read-string "New snapshot name: "))
	   current-prefix-arg))))

;;;; vc.el interface
;;;;; BACKEND PROPERTIES
;;;;;; revision granularity
(defun vc-clearcase-revision-granularity ()
  "Return the revision granularity of ClearCase.
This is always 'file -- ClearCase has per-file revision
numbering."
  'file)

;;;;; STATE-QUERYING FUNCTIONS
;;;;;; registered

;;;###autoload(defun vc-clearcase-registered (file)
;;;###autoload  (let (wdview
;;;###autoload        retcode
;;;###autoload        (program cleartool-program))
;;;###autoload    (setq wdview
;;;###autoload          (with-output-to-string
;;;###autoload            (with-current-buffer standard-output
;;;###autoload              (setq retcode
;;;###autoload                    (call-process
;;;###autoload                     program nil t nil "pwv" "-short" "-wdview")))))
;;;###autoload    ;;(message "Wdview for %s is %S" file wdview)
;;;###autoload    (if (or (not (eq retcode 0))
;;;###autoload            (eq (compare-strings "** NONE **" 0 10 wdview 0 10) t))
;;;###autoload        nil
;;;###autoload      (load "vc-clearcase")
;;;###autoload      (vc-clearcase-registered file))))

(defun vc-clearcase-registered (file)
  "Return non nil if FILE is registered in ClearCase.
We consider it to be registered, if cleartool can tell us its
version."
  (setq file (expand-file-name file))
  (catch 'done
    ;; if the file already has a version set, or we asked for it already,
    ;; return t
    (let ((fprop (clearcase-file-fprop file)))

      (when (clearcase-fprop-initialized-p fprop)
	(throw 'done t))

      ;; we need to ask ClearCase if the file is registered or not.

      (unless fprop
	(setq fprop (clearcase-make-fprop :file-name file)))

      (ignore-cleartool-errors
	(let ((ls-result (cleartool "ls \"%s\"" file)))
	  (unless (string-match "Rule: \\(.*\\)$" ls-result)
	    (throw 'done nil))          ; file is not registered

	  (clearcase-set-fprop-version-stage-1 fprop ls-result)
	  ;; anticipate that the version will be needed shortly, so ask for
	  ;; it.  When a file is hijacked, do the desc command on the version
	  ;; extended name of the file, as cleartool will return nothing for
	  ;; the hijacked version...
	  (let ((pname (if (clearcase-fprop-hijacked-p fprop)
			   (concat file "@@" (clearcase-fprop-version fprop))
			   file)))
	    (setf (clearcase-fprop-version-tid fprop)
		  (cleartool-ask
		   (format "desc -fmt \"%%Vn %%PVn %%Rf\" \"%s\"" pname)
		   'nowait
		   fprop
		   'clearcase-set-fprop-version-stage-2))))
	(vc-file-setprop file 'vc-clearcase-fprop fprop)
	(throw 'done t)))))

;;;;;; state

(defun vc-clearcase-state (file)
  "Return the current version control state of FILE.

How we map clearcase states to vc states:

'up-to-date -- file is not checked out and the current version is
the one selected by our config-spec.

'edited -- file is checked out by us and no newer version exists
on the branch.

\"USER\" -- this is never returned, we handle this by asking for
an unreserved checkout.

'needs-patch -- the file is not latest on the branch and the
configspec rule does not branch.  (we should check that an update
-print command indicates that it would update the file.)

'needs-merge -- file is not the latest on our branch and we
checked it out.

'unlocked-changes -- file is hijacked."
  (setq file (expand-file-name file))

  ;; we are asked for a reliable computation of state, so refresh all the
  ;; properties.
  (clearcase-maybe-set-vc-state file 'force)

  (let ((fprop (clearcase-file-fprop file)))

    ;; we are about to operate on the file, so check if the view is
    ;; consistent.  Clearcase operations will occasionally fail saying that an
    ;; update is already in progress for this view.  We can anticipate that,
    ;; because the rule that selects this version will be "Rule: <rule info
    ;; unavailable>".  In that case, we exit with an error telling the user to
    ;; update his view.

    (when (and (clearcase-snapshot-view-p fprop)
	       (clearcase-fprop-broken-view-p fprop))
      (error "Snapshot view is inconsistent, run an update"))

    ;; return the state.  The heuristic already gives all the information we
    ;; need.
    (vc-clearcase-state-heuristic file)))

;;;;;; state-heuristic
(defun vc-clearcase-state-heuristic (file)
  "Determine the state of FILE.
Use whatever `clearcase-maybe-set-vc-state' gave us.  See
`vc-clearcase-state' for how states are mapped to ClearCase
information."
  (setq file (expand-file-name file))
  (clearcase-maybe-set-vc-state file)
  (let ((fprop (clearcase-file-fprop file)))
    (cond
      ((clearcase-fprop-hijacked-p fprop)
       'unlocked-changes)

      ((clearcase-fprop-checkedout-p fprop)
       (if (string= (clearcase-fprop-latest fprop)
		    (clearcase-fprop-parent fprop))
	   'edited
	   'needs-merge))

      ((or (clearcase-fprop-checkout-denied-p fprop)
	   (clearcase-fprop-checkout-will-branch-p fprop)
	   (string= (clearcase-fprop-latest fprop)
		    (clearcase-fprop-version fprop)))
       'up-to-date)

      ;; revision is not latest on its branch and a checkout will not
      ;; branch...
      (t 'needs-update))))

;;;;;; dir-status

(defcustom clearcase-dir-status-ignored-files nil
  "A list of regexps mathing files to be ignored by `vc-clearcase-dir-status'.
ClearCase does not provide a mechanism for ignoring unregistered
files from status listings and they can clutter the display.  We
implement a simple mechanism for filtering out unwanted file: if
the file name maches any regexp in this set (and it is not
registered with ClearCase), it will not be displayed in the
*vc-dir* buffer."
  :type '(repeat string)
  :group 'vc-clearcase)

(defun clearcase-dir-status-parse-line ()
  "Process one line from a \"cleartool ls\" output and return a
list of (FILE VC-STATE nil), we always return nil for the EXTRA
part.  Return nil if the line has no file information or the file
is ignored (see `clearcase-dir-status-ignored-files')"
  (beginning-of-line)
  (cond
    ((looking-at "^\\(.*\\)@@")         ; version controlled item
     (let* ((file (match-string 1))
	    (limit (save-excursion
		     (re-search-forward "\\s-+Rule: " (c-point 'eol))
		     (point)))
	    (state (cond
		     ((progn
			(beginning-of-line)
			(re-search-forward "\\[hijacked\\]" limit 'noerror))
		      'unlocked-changes)
		     ((progn
			(beginning-of-line)
			(re-search-forward "CHECKEDOUT" limit 'noerror))
		      'edited)
		     (t 'up-to-date))))
       (list file state nil)))

    ((looking-at "^.+$")                ; not an empty line
     ;; file is not managed by ClearCase
     (let ((file (match-string 0)))
       ;; unregistered directories and files matching a regexp in
       ;; clearcase-dir-status-ignored-files will not be displayed.
       (unless (or (file-directory-p file)
		   (some (lambda (rx) (string-match rx file))
			 clearcase-dir-status-ignored-files))
	 (list file 'unregistered nil))))

    (t nil)))

(defun clearcase-dir-status-collect (process string)
  "Collect information from a cleartool ls output and give it to
vc-dir via the update function."
  (let ((data nil))
    (with-current-buffer (process-get process 'output-buffer)
      (save-match-data
	(goto-char (point-max))
	(insert string)
	(goto-char (point-min))
	(let ((kill-whole-line t))
	  (while (and (not (= (point-min) (point-max)))
		      (progn (re-search-forward "$")
			     (looking-at "\n"))) ; do we have a full line?
	    (goto-char (point-min))
	    (let ((d (clearcase-dir-status-parse-line)))
	      (when d (push d data)))
	    (goto-char (point-min))
	    (kill-line)))))
    (funcall (process-get process 'update-function) (nreverse data) t)))

(defun clearcase-dir-status-sentinel (process event)
  "Kill the output buffer for the cleartool process and signal
`vc-dir' that we are done."
  (let ((status  (process-status process))
	(output-buffer (process-get process 'output-buffer))
	(update-function (process-get process 'update-function)))
    (when (memq status '(signal exit))
      (funcall update-function nil nil) ; signal vc-dir that we are done.
      (kill-buffer output-buffer))))

(defun vc-clearcase-dir-status (dir update-function)
  "Backend function for implementing `vc-dir' functionality for ClearCase.
We run a cleartool ls command in the background and parse its
output.  Note that we will only be able to detect checked out and
hijacked files, not files which need to be updated."
  (let* ((default-directory dir)
	 (args (list cleartool-program "ls" "-recurse" "-visible"))
	 (process (apply 'start-process "cleartool-ls" (current-buffer) args))
	 (output-buffer (generate-new-buffer "*cleartool-ls*")))

    ;; the DIR-STATUS spec requires to use (current-buffer) as the process
    ;; buffer, but we also need a buffer to store results received from the
    ;; cleartool command.

    (set-process-filter process 'clearcase-dir-status-collect)
    (set-process-sentinel process 'clearcase-dir-status-sentinel)
    (process-put process 'output-buffer output-buffer)
    (process-put process 'update-function update-function)

    (with-current-buffer output-buffer
      (buffer-disable-undo)
      (setq default-directory dir))

    ;; we will compute results asynchronously
    nil))

;;;;;; working-revision
(defun vc-clearcase-working-revision (file)
  "Return the workfile version of FILE.
If the file is checked out, In ClearCase, the version is always
\"CHECKEDOUT\", but the vc.el assumes that checked out is not a
separate version, so we return the parent version in that case."
  (clearcase-maybe-set-vc-state file)
  (clearcase-fprop-version (clearcase-file-fprop file)))

;;;;;; latest-on-branch-p
(defun vc-clearcase-latest-on-branch-p (file)
  "Return true if FILE is the latest version on the branch."
  (clearcase-maybe-set-vc-state file)
  (let ((fprop (clearcase-file-fprop file)))
    (string= (clearcase-fprop-version fprop)
	     (clearcase-fprop-latest fprop))))

;;;;;; checkout-model
(defun vc-clearcase-checkout-model (files)
  "Checkout model for ClearCase is always locking for every file."
  'locking)

;;;;;; workfile-unchanged-p
(defun vc-clearcase-workfile-unchanged-p (file)
  "Return true if FILE is unchanged.
We consider it unchanged if it is checked in, or checked out but
no modifications are made.  If it is hijacked, we consider it
modified even if no modifications were made."
  ;; NOTE: apparently, the -status_only option does not work: it returns
  ;; success all the time in the interactive cleartool process.
  (setq file (expand-file-name file))
  (let ((fprop (clearcase-file-fprop file)))
    (cond
      ((null (clearcase-fprop-status fprop)) t)
      ((clearcase-fprop-checkedout-p fprop)
       (string= (cleartool "diff -pre -opt -headers_only \"%s\"" file)
		"Files are identical\n"))
      (t nil))))

;;;;;; mode-line-string
(defcustom clearcase-wash-mode-line-function nil
  "Function to call to post-process the mode line string.
This is a function which receives a string representing the
version control mode line and must return a new string which will
be used as the mode line.

The VS modeline for a ClearCase file can be quite long because
the branch or stream name is included.  This can be agravated by
site conventions which insist on using strings as 'release',
'branch' or 'iteration' as part of the branch name.  UCM streams
have the user name as part of the stream name.

This variable allows the user to implement a mechanism for
abbreviating these strings based on site-specific information."
  :type '(choice (const nil) function)
  :group 'vc-clearcase)

(defun vc-clearcase-mode-line-string (file)
  "Return the mode line string for FILE."
  (clearcase-maybe-set-vc-state file)
  (let ((fprop (clearcase-file-fprop file))
	tag mode-line)
    (setq tag
	  (if (clearcase-ucm-view-p fprop)
	      (let ((vprop (clearcase-get-vprop fprop)))
		(concat "<" (clearcase-vprop-stream vprop) ">"))
	      (let ((branch (clearcase-fprop-branch fprop))
		    (version-number (clearcase-fprop-version-number fprop)))
		(concat branch "/" version-number))))
    (setq mode-line (case (clearcase-fprop-status fprop)
		      ('hijacked "Cc:HIJACKED")
		      ('broken-view "Cc:BROKEN-VIEW")
		      ('reserved (concat "Cc:(R)" tag))
		      ('unreserved (concat "Cc:(U)" tag))
		      (t (concat "Cc:" tag))))
    (when clearcase-wash-mode-line-function
      (setq mode-line (funcall clearcase-wash-mode-line-function mode-line)))
    mode-line))

;;;;; STATE-CHANGING FUNCTIONS
;;;;;; register
(defun vc-clearcase-register (file &optional rev comment)
  "Register FILE with clearcase.  REV and COMMENT are ignored.
ClearCase requires the directory in which file resides to be
checked out for the insertion to work.  If the directory is
checked out, we leave it checked out, otherwise we do a checkout
for the file insertion than a checkin.

NOTE: if dir is not under clearcase, this code will fail.  We
don't attempt to register a directory in clearcase even if one of
it's parents is registered."

  (when (consp file)
    (assert (= (length file) 1) "Only one file is accepted")
    (setf file (car file)))
  (setq file (expand-file-name file))
  (with-clearcase-checkout (file-name-directory file)
    (let ((cleartool-timeout (* 2 cleartool-timeout)))
      (cleartool "mkelem -nc \"%s\"" file))
    (let ((fprop (clearcase-make-fprop :file-name file)))
      (vc-file-setprop file 'vc-clearcase-fprop fprop)
      (clearcase-maybe-set-vc-state file 'force))))

;;;;;; responsible-p

(defvar clearcase-known-vobs ()
  "A list of the VOBS we know to exist.
This is used by `vc-clearcase-responsible-p' to avoid doing a
lsvob.")

(defun vc-clearcase-responsible-p (file)
  "Return t if we responsible for FILE.
We consider ourselves responsible if FILE is inside a ClearCase
view under a VOB directory."

  (ignore-cleartool-errors
    (let ((vob (clearcase-vob-tag-for-path file)))
      (if (member vob clearcase-known-vobs)
	  t
	  ;; else
	  (progn
	    ;; lsvob will signal an error if VOB is not valid.
	    (cleartool "lsvob -short \"%s\"" vob)
	    (push vob clearcase-known-vobs)
	    t
	    )))))

;;;;;; checkin
(defun vc-clearcase-checkin (files rev comment)
  "Checkin FILE.
REV should be nil, COMMENT is the checkin comment.
`vc-checkin-switches' is ignored."

  ;; Implementation note: We pass -identical to checkin which means we
  ;; will create a new version even if some file is unchanged -- this
  ;; is needed since an "file is identical" error will abort the
  ;; checkin mid-way.  vc.el is responsible for not calling checkin if
  ;; the file has not changed, so we should not create identical
  ;; copies anyway...

  (setq files (mapcar 'expand-file-name files))

  (when rev
    (error "Revision specification not supported: %s" rev))
  (let ((pnames (mapconcat (lambda (f) (concat "\"" f "\"")) files " ")))
    (if (or (null comment) (equal comment ""))
	;; use the checkout comment, note that we can be called from vc-dired,
	;; so the checkout comment might not be known to us.
	(cleartool "checkin -ptime -nwarn -identical -nc %s" pnames)
	(with-clearcase-cfile (cfile comment)
	  (cleartool "checkin -ptime -nwarn -identical -cfile %s %s" cfile pnames))))
  ;; we might not have a fprop if called from vc-dired
  (dolist (file files)
    (when (clearcase-file-fprop file)
      (clearcase-maybe-set-vc-state file 'force))))

;;;;;; find-version
(defun clearcase-find-version-helper (file rev destfile)
  "Get the FILE revision REV into DESTFILE.
This is a helper function user by both
`vc-clearcase-find-version' and `vc-clearcase-checkout' (since we
want to preserve the Emacs 21.3 `vc-clearcase-checkout'
behaviour."
  (setq file (expand-file-name file))
  (when (string= rev "")
    (error "Refusing to checkout head of trunk"))
  (let ((fprop (clearcase-file-fprop file)))
    (unless rev
      (setq rev (clearcase-fprop-latest-sel fprop)))
    ;; Handle the case when we are asked by a checked out file by its version
    ;; extended pathname.
    (if (and (string-match "\\(.+\\)[\\/]CHECKEDOUT\\(?:\.[0-9]+\\)?$" rev)
	     (clearcase-fprop-checkedout-p fprop)
	     (string= (match-string 1 rev) (clearcase-fprop-version-base fprop)))
	(copy-file file destfile)
	(progn
	  (when (file-exists-p destfile)
	    (delete-file destfile))
	  (cleartool "get -to \"%s\" \"%s@@%s\"" destfile file rev)))))

(defun vc-clearcase-find-version (file rev buffer)
  "Fetch FILE revision REV and place it into BUFFER.
If REV nil, it will get the latest on the branch, if REV is the
empty string, we signal an error, since head of trunk has no
meaning in ClearCase."
  ;; make-temp-file creates a file, and clearcase will refuse to get the
  ;; version into an existing file...
  (let ((tmp (make-temp-name (concat temporary-file-directory "vc-clearcase-"))))
    (unwind-protect
	 (progn
	   (clearcase-find-version-helper file rev tmp)
	   (with-current-buffer buffer
	     (insert-file-contents-literally tmp)))
      (when (file-exists-p tmp)
	(delete-file tmp)))))

;;;;;; checkout
(defun clearcase-finish-checkout (file rev comment mode)
  "Finish a checkout started by `vc-clearcase-checkout'.
FILE, REV and COMMENT are the same as the one from
`vc-clearcase-checkout', MODE selects the checkout mode and can
be 'reserved or 'unreserved."

  ;; NOTE: we pass the -ptime to checkout to preserve the modification
  ;; time of the file in a dynamic view (cleartool preserves it
  ;; automatically in a static view).  If we don't do that, vc.el will
  ;; be confused and will try to ckeck-in an unmodified file (without
  ;; bothering to do a diff) instead of reverting the checkout.

  (with-clearcase-cfile (comment-file comment)
    (let ((pname (if rev (concat file "@@" rev) file))
	  (options (concat "-ptime -nwarn -nquery "
			   "-cfile " comment-file
			   " " (when rev "-version ")))
	  (co-mode (if (eq mode 'reserved) "-reserved " "-unreserved "))
	  ;; increase the cleartool timeout for the checkout operation
	  (cleartool-timeout (* 1.5 cleartool-timeout)))
      ;; NOTE: if this fails, we should prompt the user to checkout
      ;; unreserved.
      (cleartool "checkout %s %s \"%s\"" co-mode options pname))
    (clearcase-maybe-set-vc-state file 'force)
    (vc-resynch-buffer file t t)))

;; This would be so much easier if vc-start-logentry would accept a
;; closure to pass it to us...

(defun clearcase-finish-checkout-reserved (file rev comment)
  "Do a reserved checkout on FILE with REV and COMMENT.
NOTE: This function will be called from `vc-start-logentry' which
works with filesets and it must expect several files."
  (clearcase-finish-checkout file rev comment 'reserved))

(defun clearcase-finish-checkout-unreserved (file rev comment)
  "Do an unreserved checkout on FILE with REV and COMMENT.
NOTE: This function will be called from `vc-start-logentry' which
works with filesets it must expect several files."
  (clearcase-finish-checkout file rev comment 'unreserved))

(defun clearcase-revision-reserved-p (file)
  "Return t if FILE is checked out reserved.
If yes, return the user and view that has the reserved checkout,
otherwise return nil."
  (let ((fprop (clearcase-file-fprop file))
	(checkouts
	 (split-string
	  (cleartool "lsco -fmt \"%%PVn %%Rf %%Tf %%u\\n\" \"%s\"" file)
	  "[\n\r]+")))
    (let* ((match (concat (clearcase-fprop-version fprop) " reserved"))
	   (len (length match))
	   (rev (catch 'found
		  (dolist (c checkouts)
		    (when (eq (compare-strings match 0 len c 0 len) t)
		      (throw 'found c))))))
      (if rev
	  (let ((elements (split-string rev)))
	    (cons (nth 3 elements) (nth 2 elements)))
	  nil))))

(defcustom clearcase-checkout-comment-type 'normal
  "The type of comments expected from the user on checkout.
The value of this variable should be one of the three symbols:

normal -- a buffer will be used to enter a comment, just like for
	  file checkin,

brief -- the comment will be read from the minibuffer,

none -- no comment will be used on checkout."
  :type '(choice
	  (const :tag "Normal" normal)
	  (const :tag "Brief" brief)
	  (const :tag "None" none))
  :group 'vc-clearcase)

(defcustom clearcase-checkout-policy 'heuristic
  "The type of checkout to perform in `vc-clearcase-checkout'.
The value of this variable should be one of the three symbols:

heuristic -- a heuristic is used to determine the checkout model.
	 By default it tries to do a reserved checkout, but if
	 `vc-clearcase-checkout' determines that the unreserverd
	 checkout will fail it will do an unreserved checkout.

reserved -- always attempt to do a reserved checkout.  This might
	 fail if someone else has the file checked out reserved
	 or we don't checkout the latest revision on the branch.

unreserved -- always do an unreserved checkout."
  :type '(choice
	  (const :tag "Heuristic" heuristic)
	  (const :tag "Always Reserved" reserved)
	  (const :tag "Always Unreserved" unreserved))
  :group 'vc-clearcase)

(defun vc-clearcase-checkout (file &optional editable rev destfile)
  "Checkout FILE as per the checkout specification in vc.el.
See the vc.el `vc-checkout' documentation for the meaning of
EDITABLE, REV and DESTFILE.

The function is asynchronous (like checkin), it pops up a buffer
for the checkout comment and finishes the checkout later.

This method does three completely different things:

  1/ Checkout a version of the file.  The real checkout.

  2/ Get a version of the file in a separate file (this is for
     backwards compatibility with Emacs 21)

  3/ Update the file (in snapshot views)."

  (setq file (expand-file-name file))
  (cond
    ((and editable destfile)
     (error "Cannot checkout to a specific file"))
    (editable
     ;; this is the real checkout operation
     (let* ((fprop (clearcase-file-fprop file))
	    checkout)
       (when (clearcase-fprop-checkout-denied-p fprop)
	 (error "Configspec rule forbids checkout of this file."))
       ;; need to find out if we have to checkout reserved or
       ;; unreserved.
       (ecase clearcase-checkout-policy
	 ('reserved (setq checkout 'clearcase-finish-checkout-reserved))
	 ('unreserved (setq checkout 'clearcase-finish-checkout-unreserved))
	 ('heuristic
	  (cond
	    ;; if the checkout will create a branch, checkout reserved
	    ((clearcase-fprop-checkout-will-branch-p fprop)
	     (setq checkout 'clearcase-finish-checkout-reserved))

	    ;; if we are not latest on branch and we are asked to
	    ;; checkout this version (eq rev nil), we checkout
	    ;; unreserved.
	    ((and (null rev)
		  (not (string= (clearcase-fprop-latest fprop)
				(clearcase-fprop-version fprop))))
	     ;; patch rev first
	     (setq rev (clearcase-fprop-version fprop))
	     (setq checkout 'clearcase-finish-checkout-unreserved))

	    ;; if someone else has checked out this revision in
	    ;; reserved mode, ask the user if he wants an unreserved
	    ;; checkout.
	    (t (let ((user-and-view (clearcase-revision-reserved-p file)))
		 (if user-and-view
		     (when (yes-or-no-p
			    (concat
			     "This revision is checked out reserved by "
			     (car user-and-view) "in" (cdr user-and-view)
			     ".  Checkout unreserved? "))
		       (setq checkout 'clearcase-finish-checkout-unreserved))
		     ;; no one has this version checked out, checkout
		     ;; reserved.
		     (setq checkout 'clearcase-finish-checkout-reserved)))))))
       (if checkout
	   (ecase clearcase-checkout-comment-type
	     ('normal (vc-start-logentry
		       (list file) rev nil nil
		       "Enter a checkout comment" "*VC-log*" checkout))
	     ('brief (let ((comment (read-string "Enter a checkout comment: ")))
		       (funcall checkout file rev comment)))
	     ('none (funcall checkout file rev "")))
	   (message "Aborted."))))
    ((and (not editable) destfile)
     ;; Check out an arbitrary version to the specified file
     (clearcase-find-version-helper file rev destfile))
    ((and (not editable) (or (null rev) (eq rev t)))
     ;; Update the file in the view (no-op in dynamic views)
     (let ((update-result (cleartool "update -force -rename \"%s\"" file)))
       (when (string-match
	      "^Update log has been written to .*$" update-result)
	 (message (match-string 0 update-result)))
       (clearcase-maybe-set-vc-state file 'force)
       (vc-resynch-buffer file t t)))
    ((not editable)                     ; last case left for not editable
     (error "Cannot to update to a specific revision"))
    (t
     (error "Bad param combinations in vc-clearcase-checkout: %S %S %S"
	    editable rev destfile))))

;;;;;; revert
(defcustom clearcase-rmbranch-on-revert-flag t
  "Non-nil means remove a branch when a revert leaves no versions on it.
When a checkout operation creates a new branch, the uncheckout
will leave the element on that branch with a version of 0 which
is identical to the parent version.  If you use lots of branches,
the version tree will become littered with useless branches.
When this flag is true, the empty branches will be removed (a new
checkout in the same view will recreate the branch.)"
  :type 'boolean
  :group 'vc-clearcase)

(defun vc-clearcase-revert (file &optional contents-done)
  "Cancel a checkout or a hijacking on FILE.
CONTENTS-DONE is ignored.  The
`clearcase-rmbranch-on-revert-flag' is honoured."
  (setq file (expand-file-name file))
  (let* ((fprop (clearcase-file-fprop file))
	 (empty-branch-p (equal "0" (clearcase-fprop-version-number fprop))))
    (if (clearcase-fprop-hijacked-p fprop)
	(cleartool "update -overwrite -force \"%s\"" file)
	(progn
	  (cleartool "uncheckout -keep \"%s\"" file)
	  (when (and empty-branch-p clearcase-rmbranch-on-revert-flag)
	    (let ((base (clearcase-fprop-version-base fprop)))
	      (cleartool "rmbranch -force -nc \"%s@@%s\"" file base)
	      ;; in snapshot views, the file seems to be listed as "[special
	      ;; selection, deleted version]", after removing the branch, so
	      ;; we need an update.
	      (when (clearcase-snapshot-view-p fprop)
		(cleartool "update -overwrite -force \"%s\"" file))))))
    (clearcase-maybe-set-vc-state file 'force)))

;;;;;; cancel-version

;; No longer a backend function in Emacs 23, but used by
;; `vc-clearcase-rollback'

(defun vc-clearcase-cancel-version (file editable)
  "Remove the current version of FILE.
FILE must be checked in and latest on its branch.  We use
\"rmver\" to remove the version, but a version will not be
removed when a branch begins there or it has labels or attributes
attached.

When EDITABLE is non nil, the file will be checked out and the
contents of the deleted version will be placed in FILE.  A
checkin at this point will create a new version with the same
contents as the deleted version.  This is useful if you checked
the file in by mistake and want to rework some changes.

When EDITABLE is nil, the version is removed completely.  The
current branch might be removed as well if
`clearcase-rmbranch-on-revert-flag' is non nil.

If an error is signalled during the cancel, the original version
is saved as a keep file in the same directory as FILE, so no data
is lost."

  ;; Implementation Notes:
  ;;
  ;; Here is what we need to do in order to cancel a version so that the
  ;; changes are kept ready for a new checkin:
  ;;
  ;; * make a temporary copy of the file and the checkin comment for the
  ;;   file.  Copy-file will create a read-only copy, since the file is
  ;;   checked in.
  ;;
  ;; * remove the version
  ;;
  ;; * on a snapshot view, this will leave the file in place as a view
  ;;   private file.  We need to remove the file and run a clearcase
  ;;   update to get the version controlled file back, otherwise
  ;;   ClearCase commands will not work on the file.
  ;;
  ;; * checkout the file.  We checkout the file unreserved, which we expect to
  ;;   succeed most of the time (unless you are in a UCM view and you don't
  ;;   have an activity set).  We will try to reserve the checkout, but do
  ;;   nothing if it fails.
  ;;
  ;; * we need to replace the checked out file with the saved copy, make
  ;;   the file writable and load it back into emacs.
  ;;
  ;;
  ;; To cancel a version so that it is completely removed we simply need
  ;; to:
  ;;
  ;; * remove the version
  ;;
  ;; * on a snapshot view, update the file
  ;;
  ;; * if removing the version left an empty branch, we honour the
  ;;   `clearcase-rmbranch-on-revert-flag' (remove the branch if the
  ;;   flag is set.
  ;;
  ;; If something goes wrong in this routine, we leave the keep file
  ;; in place.  This is consistent with ClearCase behaviour.

  (setq file (expand-file-name file))
  (let ((fprop (clearcase-file-fprop file))
	(keep-file (clearcase-get-keep-file-name file))
	(comment-text nil))
    (when editable
      (copy-file file keep-file)
      (setq comment-text
	    (cleartool "desc -fmt \"%%c\" \"%s@@%s\""
		       file (clearcase-fprop-version fprop))))

    ;; -xhlink, enables us to cancel versions which have activities or merge
    ;; arrows attached.  Without it we cannot cancel any version in UCM views.

    (cleartool "rmver -force -xhlink -nc \"%s@@%s\""
	       file (clearcase-fprop-version fprop))

    (when (clearcase-snapshot-view-p fprop)
      (cleartool "update -overwrite -force \"%s\"" file))

    (when editable
      (with-clearcase-cfile (comment-file comment-text)
	(cleartool
	 "checkout -nquery -cfile \"%s\" -nwarn -ndata -unreserved \"%s\""
	 comment-file file))
      (copy-file keep-file file 'overwrite)
      (set-file-modes file (logior (file-modes file) #o220))
      (ignore-cleartool-errors
	(cleartool "reserve -ncomment \"%s\"" file))
      (revert-buffer 'ignore-auto 'noconfirm))

    (when (and (not editable)
	       clearcase-rmbranch-on-revert-flag
	       (string-match "[/\\]0$" (clearcase-fprop-parent fprop)))
      ;; we were left with an empty branch, remove that as well
      (cleartool "rmbranch -force -nc \"%s@@%s\""
		 file (clearcase-fprop-version-base fprop))
      ;; see vc-clearcase-revert on why we do this...
      (when (clearcase-snapshot-view-p fprop)
	(cleartool "update -overwrite -force \"%s\"" file)))

    (when editable
      (delete-file keep-file))

    (clearcase-maybe-set-vc-state file 'force)))

;;;;;; rollback
;; New in Emacs 23
(defun vc-clearcase-rollback (files)
  "Remove the current version of every file in FILES.
This function simply calls `vc-clearcase-cancel-version' for
each file.

NOTE: this function replaces `vc-clearcase-cancel-version' from
Emacs 22 and it is less functional (EDITABLE cannot be set)"
  (dolist (file files)
    (vc-clearcase-cancel-version file nil)))

;;;;;; merge
(defun vc-clearcase-merge (file rev1 rev2)
  "Merge into FILE REV1 up to REV2.
The operation will throw an error if the merge cannot be done
automatically.  vc.el assumes there are conflict markers in the
buffer, but ClearCase does not use them.

NOTE: when trying to merge revisions (vc-merge) on a file that is
not checked-out, vc asks for a checkout, but that the comment
window pops up, and `vc-merge' assumes the file was already
checked out.  We need to do an automatic checkout in this case,
but we don't do that."
  (setq file (expand-file-name file))
  (let ((merge-status
	 (cleartool
	  "merge -abort -insert -to \"%s\" -ver %s %s" file rev1 rev2)))
    (with-current-buffer (get-buffer-create "*vc-merge-result*")
      (let ((inhibit-read-only t))
	(insert merge-status)
	(switch-to-buffer-other-window (current-buffer) 'norecord)
	(shrink-window-if-larger-than-buffer)))
    0))                                 ; return success

;;;;;; merge-news
(defun vc-clearcase-merge-news (file)
  "Merge the new versions in FILE."
  (setq file (expand-file-name file))
  (let ((latest (concat file "@@"
			(clearcase-fprop-latest-sel
			 (clearcase-file-fprop file)))))
    (message "Merging LATEST into this version")
    ;; NOTE: we abort if anything goes wrong with the merge.  Let the
    ;; error propagate to the vc package.  If we just return 1, it
    ;; will try to invoke smerge-mode or ediff, expecting CVS-like
    ;; conflict markers.
    (let ((merge-status
	   (cleartool "merge -abort -to \"%s\" \"%s\"" file latest)))
      (with-current-buffer (get-buffer-create "*vc-merge-result*")
	(let ((inhibit-read-only t))
	  (insert merge-status)
	  (switch-to-buffer-other-window (current-buffer) 'norecord)
	  (shrink-window-if-larger-than-buffer)))
      0)))                              ; return success

;;;;;; steal-lock
(defun vc-clearcase-steal-lock (file &optional version)
  "Checkout a hijacked FILE and keep its current contents.
VERSION is not used, and we signal an error if it is not nil.

We save the current contents of the file, perform an unreserved
checkout, put the contents of the file back in, than try to
reserve the checkout.  At the end of the process, FILE will be
checked out and the contents will be the one of the hijacked
file.  File might be checked out unreserved, if someone already
has a reserved checkout of the file."
  (when version
    (error "vc-clearcase-steal-lock: cannot steal a specific version"))
  (setq file (expand-file-name file))
  (let ((keep-file (clearcase-get-keep-file-name file)))
    ;; if something goes wrong in this routine, we leave the keep file
    ;; in place.  This is consistent with ClearCase behaviour.
    (rename-file file keep-file)
    (condition-case err

	(progn
	  (cleartool "checkout -nquery -ncomment -nwarn -ndata -unreserved \"%s\"" file)
	  (copy-file keep-file file 'overwrite)
	  ;; make file writable, in case it wasn't
	  (set-file-modes file (logior (file-modes file) #o220))
	  (delete-file keep-file)
	  (ignore-cleartool-errors
	    (cleartool "reserve -ncomment \"%s\"" file))
	  (clearcase-maybe-set-vc-state file 'force))

      (cleartool-error
       ;; if we failed above, and we don't have a file, put the original file
       ;; back
       (unless (file-exists-p file)
	 (rename-file keep-file file))
       (error (error-message-string err))))))

;;;;;; modify-change-comment

;; TODO: needs to be tested
(defun vc-clearcase-modify-change-comment (files rev comment)
  "Change the comment for the revision REV of FILES to COMMENT.
We accept any number of files to conform to the vc interface, but
for clearcase this operation only makes sense for one file."
  (with-clearcase-cfile (cfile comment)
    (dolist (file files)
      (cleartool "chevent -replace -cfile %s \"%s\"@@%s" cfile file))))

;;;;; HISTORY FUNCTIONS
;;;;;; print-log
(defcustom clearcase-print-log-show-labels 'some
  "How to display the labels in the log (lshistory) output.
The value of this variable should be one of the three symbols:

none -- no labels are displayed, this setting will result in the
    fastest print-log output

some -- the most recent labels for each version are displayed.

all -- all the labels for each version are displayed.  This
    setting can make `vc-print-log' (\\[vc-print-log]) command
    very slow."
  :type '(choice
	  (const :tag "None" none)
	  (const :tag "Some" some)
	  (const :tag "All" all))
  :group 'vc-clearcase)

(defvar clearcase-lshistory-fmt
  (concat "----------------------------\n"
	  "revision %Vn (%e)\n"
	  "date: %d; author: %u\n"
	  "%c")
  "Format string to use when listing file history.")

(defvar clearcase-lshistory-fmt-ucm
  (concat "----------------------------\n"
	  "revision %Vn (%e)\n"
	  "date: %d; author: %u\n"
	  "activity: %[activity]p\n"
	  "%c")
  "Format string so use when listing file history.
This is used when the file is in a UCM project.")

(defun vc-clearcase-print-log (file &optional buffer)
  "Insert the history of FILE into the *clearcase-lshistory* buffer.
By default only the history of the current branch of the file is
printed.  With a previx argument (C-u) entire file history is
printed."

  ;; TODO (Emacs23): vc-clearcase-print-log
  ;;
  ;; We currently support one file even though vc will pass a fileset
  ;; (list of files) in FILE.  Problems with filesets containing
  ;; multiple files:
  ;;
  ;; * cleartool supports printing the history of several files at
  ;;   once.  The evet records will be sorted by date so events for
  ;;   one file will be mixed with events for the other files
  ;;
  ;; * We have a label section at the top of the file, but this only
  ;;   provides a mapping from the label name to the version which
  ;;   will be ambiguous when multiple files are involved.
  ;;
  ;; * The format records used by lshistory (see
  ;;  `clearcase-lshistory-fmt' and `clearcase-lshistory-fmt-ucm' will
  ;;  not print a file name for each record to avoid cluttering the
  ;;  display (there is a single "Working file: " record at the top of
  ;;  the log).  However this will make the display ambiguous if more
  ;;  than one file is involved.
  ;;
  ;; For now, we accept only one file in the fileset.

  (when (consp file)
    (assert (= (length file) 1) "Only one file is accepted")
    (setf file (car file)))
  (setq file (expand-file-name file))
  (let ((inhibit-read-only t)
	(buf (if buffer
		 (get-buffer-create buffer)
		 (get-buffer-create "*vc*")))
	(fprop (clearcase-file-fprop file))
	(label-revisions nil)
	(max-label-length 0))

    (when (memq clearcase-print-log-show-labels '(some all))
      (with-temp-message "Collecting label information..."
	(dolist (v (split-string
		    (cleartool "lsvtree -nco %s \"%s\""
			       (if (eq clearcase-print-log-show-labels 'all)
				   "-all"
				   "")
			       file)
		    "[\n\r]+"))
	  (when (string-match "@@\\([^ ]+\\) (\\(.*\\))" v)
	    (let ((revision (match-string 1 v))
		  (labels (match-string 2 v)))
	      (dolist (label (split-string labels ", "))
		(unless (string= label "...")
		  (setq max-label-length (max max-label-length (length label)))
		  (push (cons label revision) label-revisions))))))))

    (with-current-buffer buf
      (erase-buffer)
      (insert (format "Working file: %s\n" file))

      (when (memq clearcase-print-log-show-labels '(some all))
	(insert "Labels:\n")

	(let ((fmtstr (format "\t%%-%ds %%s\n" max-label-length)))
	  (dolist (label label-revisions)
	    (insert (format fmtstr (car label) (cdr label))))))

      (let ((args (list "-fmt" (if (clearcase-ucm-view-p fprop)
				   clearcase-lshistory-fmt-ucm
				   clearcase-lshistory-fmt)
			file)))
	(unless current-prefix-arg
	  (setq args (append (list "-branch" (format "brtype:%s"
						     (clearcase-fprop-branch fprop)))
			     args)))
	(apply 'start-process
	       "cleartool-lshistory" buffer
	       cleartool-program "lshistory"
	       args)))))

;;;;;; log-view-mode
(defconst clearcase-log-view-file-re
  "^Working file: \\(.+\\)$"
  "Regexp to match the filename in a lshistory listing
The actual filename is the first match subexpression")

(defconst clearcase-log-view-message-re
  "^revision \\(\\S-+\\) (\\(?:create\\|checkout\\) version)"
  "Regexp to match the start of a lshistory record
The revision is the first match subexpression.")

(defconst clearcase-log-view-font-lock-keywords
  `(("-+" . font-lock-comment-face)
    (,clearcase-log-view-file-re . log-view-file-face)
    (,clearcase-log-view-message-re . log-view-message-face)
    ("(reserved)" . font-lock-variable-name-face)
    ("<No-tag-in-region>" . font-lock-warning-face)))
(defconst clearcase-log-view-font-lock-defaults
  '(clearcase-log-view-font-lock-keywords t nil nil nil))

(define-derived-mode vc-clearcase-log-view-mode log-view-mode
  "Cc-Log-View"
  "Mode to view clearcase log listings."
  (set (make-local-variable 'font-lock-defaults)
       clearcase-log-view-font-lock-defaults)
  (set (make-local-variable 'log-view-message-re)
       clearcase-log-view-message-re)
  (set (make-local-variable 'log-view-file-re)
       clearcase-log-view-file-re))

;;;;;; show-log-entry
(defun vc-clearcase-show-log-entry (version)
  "Search for VERSION in the current buffer.
Only works for the clearcase log format defined in
`clearcase-lshistory-fmt'."
  (let ((regexp
	 (concat "^revision "
		 (replace-regexp-in-string "[\\\\/]" "[\\\\/]" version)
		 "\\>"))
	pos)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward regexp (point-max) 'noerror)
	(setq pos (match-beginning 0))))
    (when pos (goto-char pos))))

;;;;;; diff
(defcustom vc-clearcase-diff-switches nil
  "*Extra switches for clearcase diff under VC.
This is either a string or a list of strings.  Useful options
are \"-diff_format\" or \"-serial_format\".

To ignore the extra white space characters, you need the option
\"-option \"-blank_ignore\"\".

See the cleartool diff manual page for possible options.

NOTE: this option is ignored when
`clearcase-use-external-diff' is t."
  :type '(choice (const :tag "None" nil)
	  (string :tag "Argument String")
	  (repeat :tag "Argument List"
	   :value ("")
	   string))
  :version "21.1"
  :group 'vc
  :group 'vc-clearcase)

(defcustom clearcase-diff-cleanup-flag t
  "Non-nil means remove ^M characters from the diff output."
  :type 'boolean
  :group 'vc-clearcase)

(defcustom clearcase-use-external-diff nil
  "Non-nil means use the `diff' function to compare revisions.
When nil, the ClearCase internal diff is used instead."
  :type 'boolean
  :group 'vc-clearcase)

(defun clearcase-diff-with-diff (file rev1 rev2)
  "Do a diff on FILE revisions REV1 and REV2 using the diff package.
The diff is stored in the current buffer.  The function returns t
if the revisions are identical and nil otherwise.
This is a helper function for `vc-clearcase-diff'"

  (setq file (file-relative-name file default-directory))

  ;; The `diff' function likes to display the diff buffer, but within vc, the
  ;; choice to display it or not is left to `vc-version-diff'.
  (save-window-excursion
    (let ((diff-start-pos (point))
	  (old (vc-version-backup-file-name file rev1 'manual))
	  (new (if rev2
		   (vc-version-backup-file-name file rev2 'manual)
		   file)))

      (when (file-exists-p old)
	(delete-file old))
      (clearcase-find-version-helper file rev1 old)

      (when rev2
	(when (file-exists-p new)
	  (delete-file new))
	(clearcase-find-version-helper file rev2 new))

      (let ((resize-mini-windows nil))
	(shell-command
	 (format "%s %s --label \"%s\" --label \"%s\" \"%s\" \"%s\""
		 diff-command diff-switches
		 (concat file " " (or rev1 ""))
		 (concat file " " (or rev2 ""))
		 old new)
	 (current-buffer)))

      ;; delete the temporary files we created
      (delete-file old)
      (when rev2 (delete-file new))

      (goto-char diff-start-pos)
      (and (re-search-forward "(no differences)" (point-max) 'noerror) t))))

(defun clearcase-diff-with-cleartool (file rev1 rev2)
  "Compare FILE's revisions REV1 and REV2 using the cleartool diff.
If REV1 and REV2 are nil, compare the current version of FILE
against its predecessor.

The diff is stored in the current buffer.  The function returns t
if the revisions are identical and nil otherwise.

This is a helper function for `vc-clearcase-diff'"

  (setq file (file-relative-name file default-directory))
  (let ((diff-start-pos (point))
	(fver1 (if rev1 (concat file "@@" rev1) file))
	(fver2 (if rev2 (concat file "@@" rev2) file))
	(opts (mapconcat 'identity
			 (if (listp vc-clearcase-diff-switches)
			     vc-clearcase-diff-switches
			     (list vc-clearcase-diff-switches))
			 " ")))
    (with-cleartool-directory default-directory
      (insert
       (if (and (null rev1) (null rev2))
	   (cleartool "diff %s -pre \"%s\"" opts file)
	   (cleartool "diff %s \"%s\" \"%s\"" opts fver1 fver2)))
      (goto-char diff-start-pos)
      (when clearcase-diff-cleanup-flag
	(while (re-search-forward "\r$" nil t)
	  (replace-match "" nil nil))
	(goto-char diff-start-pos))
      ;; the way we determine whether the files are identical depends
      ;; on the diff format we use.
      (or
       ;; diff format has an empty buffer
       (equal diff-start-pos (point-max))
       ;; serial format prints "Files are identical", so we look for that.
       (looking-at "\\(Files\\|Directories\\) are identical")))))

(defun vc-clearcase-diff (files &optional rev1 rev2 buffer)
  "Put the FILES diff between REV1 and REV2 in BUFFER.
Return t if the revisions are identical, nil otherwise.

When a file in FILES is a directory, the directory revisions are
compared and not the directory contents.
When BUFFER is nil, *vc-diff* is used instead.
When REV1 is nil, the files latest (checked-in) revision is used,
when REV2 is nil, the current contents of the file are used."

  ;; `vc-clearcase-diff' is called by both vc-diff and vc-diff-tree, in the
  ;; first case, we need to erase the buffer but in the second we should not,
  ;; as several diffs will accumulate in it.  The only way to tell the two
  ;; cases appart is that in vc-diff-tree case BUFFER is already the current
  ;; buffer.  Also, if the files are identical, vc.el expects us not to put
  ;; anything in the buffer.

  (unless buffer
    (setq buffer (get-buffer-create "*vc-diff*")))
  (unless (eq buffer (current-buffer))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
	(erase-buffer)
	(diff-mode))))

  (with-current-buffer buffer
    (goto-char (point-max))
    (let ((inhibit-read-only t)
	  (all-identical? t)
	  (external-diff? nil))
      (dolist (file files)
	(setq file (expand-file-name file))
	;; always use cleartool diff when comparing directory
	;; revisions.
	(setq external-diff? (and clearcase-use-external-diff
				  (not (file-directory-p file))))
	(with-cleartool-directory (file-name-directory file)
	  (let ((diff-start-pos (point))
		(fprop (clearcase-file-fprop file))
		(default-directory (file-name-directory file)))
	    (unless rev1
	      (setq rev1 (clearcase-fprop-version fprop)))
	    (setq file (file-name-nondirectory file))

	    (let ((identical? (if external-diff?
				  (clearcase-diff-with-diff file rev1 rev2)
				  (clearcase-diff-with-cleartool file rev1 rev2))))
	      (when identical?
		(kill-region diff-start-pos (point-max)))
	      (goto-char diff-start-pos)
	      (setq all-identical? (and all-identical? identical?))))))
      all-identical?)))

;;;;;; annotate-command
(defun vc-clearcase-annotate-command (file buf rev)
  "Get the annotations for FILE and put them in BUF.
REV is the revision we want to annotate.  With prefix argument,
will ask if you want to display the deleted sections as well."
  (setq file (expand-file-name file))
  (let ((pname (concat file (when rev (concat "@@" rev)))))
    (vc-setup-buffer buf)
    (with-current-buffer buf
      (let ((fmt-args '("-fmt" "%-9.9Sd %-8.8u %Sn |"))
	    (rm-args (when (and current-prefix-arg
				(y-or-n-p "Show deleted sections? "))
		       '("-rm" "-rmfmt" "D %-9.9Sd %-8.8u |"))))
	(cleartool-do
	 "annotate"
	 (append fmt-args rm-args `("-out" "-" "-nheader" ,pname))
	 buf)
	(setq cleartool-finished-function
	      '(lambda ()
		(let ((buffer (current-buffer)))
		  (clearcase-annotate-post-process buffer))))))))

(defconst clearcase-annotate-months
  '(("Jan" . 1) ("Feb" . 2) ("Mar" . 3) ("Apr" . 4)
    ("May" . 5) ("Jun" . 6) ("Jul" . 7) ("Aug" . 8)
    ("Sep" . 9) ("Oct" . 10) ("Nov" . 11) ("Dec" . 12)))

(defconst clearcase-annotate-date-rx
  "\\([0-9]+\\)-\\([A-Za-z]+\\)-\\([0-9]+\\)")

(defun clearcase-annotate-mktime (time-str)
  "Convert TIME-STR into a fractional number of days.
NOTE: we don't use `vc-annotate-convert-time' since it is not
available in Emacs 21."
  (when (and (stringp time-str)
	     (string-match clearcase-annotate-date-rx time-str))
    (let ((day (string-to-number (match-string 1 time-str)))
	  (month
	   (cdr (assoc (match-string 2 time-str)
		       clearcase-annotate-months)))
	  (year (string-to-number (match-string 3 time-str))))
      (incf year (if (< year 70) 2000 1900))
      (/ (float-time (encode-time 0 0 0 day month year)) 24 3600))))

(defun clearcase-annotate-post-process (buffer)
  "Compute the age, time and revision of each line in BUFFER.
These will be stored as properties, so
`vc-clearcase-annotate-difference', `vc-clearcase-annotate-time'
and `vc-clearcase-annotate-revision-atline' work fast."
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
	  (date-rx "^[0-9]+-[A-Za-z]+-[0-9]+")
	  (version-rx " \\([\\/][-a-zA-Z0-9._\\/]+\\) +|"))
      ;; Step 1: parse the buffer and annotate the text with the time
      ;; and revision number of each line
      (goto-char (point-max))
      (let ((now (/ (float-time) 24 3600))
	    (beg (point))
	    (end (point))
	    time-str revision-str time age)
	(while (re-search-backward date-rx nil 'noerror)
	  (setq time-str (match-string-no-properties 0))
	  (setq time (clearcase-annotate-mktime time-str))
	  (setq age (- now time))

	  (when (re-search-forward version-rx (c-point 'eol) 'noerror)
	    (setq revision-str (match-string-no-properties 1)))

	  (beginning-of-line)
	  (setq beg (point))
	  (put-text-property beg end 'vc-clearcase-time time)
	  (put-text-property beg end 'vc-clearcase-age age)
	  (put-text-property beg end 'vc-clearcase-revision revision-str)
	  (setq end (1- beg))))
      ;; Step 2: all the '|' markers in continuation lines
      (goto-char (point-min))
      (while (re-search-forward "^ +\\. +|" nil t)
	(let* ((bol (c-point 'bol))
	       (time (get-text-property bol 'vc-clearcase-time))
	       (age (get-text-property bol 'vc-clearcase-age))
	       (revision (get-text-property bol 'vc-clearcase-revision))
	       (str "                   .                    |")
	       (max (1- (length str))))
	  (put-text-property 0 max 'vc-clearcase-time time str)
	  (put-text-property 0 max 'vc-clearcase-age age str)
	  (put-text-property 0 max 'vc-clearcase-revision revision str)
	  (replace-match str nil nil)))
      ;; Step 3: truncate or expand all the version numbers
      (goto-char (point-min))
      (while (re-search-forward version-rx nil t)
	(let* ((str (match-string-no-properties 1))
	       max
	       (bol (c-point 'bol))
	       (time (get-text-property bol 'vc-clearcase-time))
	       (age (get-text-property bol 'vc-clearcase-age))
	       (revision (get-text-property bol 'vc-clearcase-revision)))
	  (when (> (length str) 20)
	    (setq str (substring str -20 (length str))))
	  (setq str (format " %20s |" str))
	  (setq max (length str))
	  (put-text-property 0 max 'vc-clearcase-time time str)
	  (put-text-property 0 max 'vc-clearcase-age age str)
	  (put-text-property 0 max 'vc-clearcase-revision revision str)
	  (replace-match str nil t))))))

;;;;;; annotate-difference
(defun vc-clearcase-annotate-difference (point)
  "Return the age in days of POINT."
  (get-text-property point 'vc-clearcase-age))

;;;;;; annotate-time
(defun vc-clearcase-annotate-time ()
  "Return the time in days of (point)."
  (get-text-property (point) 'vc-clearcase-time))

;;;;;; annotate-exact-revision-at-line
(defun vc-clearcase-annotate-extract-revision-at-line ()
  "Return the version of (point)."
  (get-text-property (point) 'vc-clearcase-revision))

;;;;; SNAPSHOT SYSTEM
;;;;;; create-tag
(defcustom clearcase-no-label-action 'ask
  "What to do when we are asked to apply a label that does not exist.
There are three possible values for this variable:

'error -- an error will be signalled when we are asked to apply a
	  non existent label.

'create -- the label will be created if it does not exist.

'ask -- ask the user whether she wants to create the label or
not.

NOTE: in ClearCase a label exists independently from the files
it is applied to.  A label must be created first before it can be
applied."
  :type '(choice (const :tag "Signal error" error)
	  (const :tag "Automatically create it" create)
	  (const :tag "Ask the user" ask))
  :group 'vc-clearcase)

(defcustom clearcase-confirm-label-move t
  "When not nil, ask the user to confirm a label move.
The vc-clearcase-create-snapshot function will not move the label
unless the snapshot is initiated with an universal
argument (C-u).  When this variable is not nil,
vc-clearcase-create-snapshot will ask the user to confirm the
label move.

The reason for this variable is to remind the CVS user that C-u
C-x v s will not create a branch in ClearCase."
  :type 'boolean
  :group 'vc-clearcase)

(defun vc-clearcase-create-tag (dir name branchp)
  "Label all files under DIR using NAME as the label.

BRANCHP is used to move an existing label.  This is not the
default behaviour, but the default behaviour is useless for
Clearcase.

First, if the label NAME does not exist, if is created with
mklbtype as an ordinary, non shared label.  Than the label is
applied recursively on DIR (but not moved if it already exists).
Than, for each parent directory of DIR the label is applied only
to that directory.  This means that you can select this version
of the sources with this single line in the configspec:

element * NAME -nocheckout"
  (when (and branchp
	     clearcase-confirm-label-move
	     (not (yes-or-no-p "Move existing label? ")))
    (error "Aborted"))
  (setq dir (expand-file-name dir))
  (with-cleartool-directory (file-name-directory dir)
    ;; let's see if the label exists
    (condition-case nil
	(cleartool "desc -fmt \"ok\" lbtype:%s" name)
      (cleartool-error-label-not-found
       (let ((should-create
	      (ecase clearcase-no-label-action
		('create t)
		('error nil)
		('ask (yes-or-no-p
		       (format "Label %s does not exist.  Create it? " name))))))
	 (if should-create
	     (progn
	       (message "Creating label %s" name)
	       (cleartool "mklbtype -ordinary -nc lbtype:%s" name)
	       (message nil))
	     (error "Label %s does not exist and will not create it" name)))))
    (let ((dir? (file-directory-p dir)))
      (message "Applying label...")
      ;; NOTE: the mklabel command might fail if some files are
      ;; hijacked... The rest of the files will be labelled though...
      (cleartool
       "mklabel -nc %s %s lbtype:%s \"%s\""
       (if branchp "-replace" "") (if dir? "-recurse" "") name dir)
      (when dir?                        ; apply label to parent directories
	(message "Applying label to parent directories...")
	(ignore-cleartool-errors
	  (while t                      ; until cleartool will throw an error
	    (setq dir (replace-regexp-in-string "[\\\\/]$" "" dir))
	    (setq dir (file-name-directory dir))
	    (cleartool
	     "mklabel -nc %s lbtype:%s \"%s\""
	     (if branchp "-replace" "") name dir)))))
    (message "Finished applying label")))

;;;;; MISCELLANEOUS
;;;;;; previous-version
(defun vc-clearcase-previous-revision (file rev)
  "Return the FILE revision that precedes the revision REV.
Return nil if no such revision exists."
  (setq file (expand-file-name file))
  ;; We simply ask clearcase to tell us the previous version name
  ;; (%PVn).  If we came across a version number of 0, we ask for the
  ;; previous version again, since the initial version on a branch (0)
  ;; is identical to the original version in the parent branch...
  (let ((prev (cleartool "desc -fmt \"%%PVn\" \"%s@@%s\"" file rev)))
    (if (string= prev "")
	nil
	(if (string-match "[\\/]0$" prev)
	    (vc-clearcase-previous-revision file prev)
	    prev))))

;;;;;; next-version
(defun vc-clearcase-next-revision (file rev)
  "Return the FILE revision that follows the revision REV.
Return nil if no such revision exists."
  (setq file (expand-file-name file))
  ;; Unfortunately, there's no easy (and correct) way of finding the
  ;; next version.  We start with the LATEST for FILE and walk
  ;; backwards until the parent is REV. If we came across a version
  ;; number of 0, skip it, since the initial version on a branch (0)
  ;; is identical to the original version in the parent branch...
  ;;
  ;; To speed up the search, we use the revision-list in fprop.
  (let* ((fprop (clearcase-file-fprop file))
	 (revision-list (clearcase-fprop-revision-list fprop)))
    (unless revision-list
      ;; If we don't have a revision list, build one now
      (message "Building revision list...")
      (let* ((prev (clearcase-fprop-latest fprop)))
	(setq revision-list (list prev))
	(while (not (string= prev ""))
	  (setq prev (cleartool "desc -fmt \"%%PVn\" \"%s@@%s\"" file prev))
	  (unless (or (string= prev "") (string-match "[\\/]0$" prev))
	    (setq revision-list (cons prev revision-list))))
	(setf (clearcase-fprop-revision-list fprop) revision-list)
	(message "Building revision list...done.")))
    (car (cdr-safe (member rev revision-list)))))

;;;;;; delete-file
(defun vc-clearcase-delete-file (file)
  "Remove FILE from ClearCase.
Previous versions of the directory will still contain FILE."
  (setq file (expand-file-name file))
  (with-clearcase-checkout (file-name-directory file)
    (cleartool "rmname -nc \"%s\"" file)))

;;;;;; rename-file
(defun vc-clearcase-rename-file (old new)
  "Rename file from OLD to NEW.
When both OLD and NEW are in the same VOB, a ClearCase rename is
used, this will preserve the file's history.  When OLD and NEW
are in different VOBs, the file is copied and registered in the
new location and removed from the old."
  ;; Unfortunately vc-rename-file will not expand these for us
  (setq old (expand-file-name old))
  (setq new (expand-file-name new))
  (with-clearcase-checkout (file-name-directory old)
    (with-clearcase-checkout (file-name-directory new)
      (with-clearcase-cfile (comment (format "*renamed from %s to %s*" old new))
	(let ((old-vob (clearcase-vob-tag-for-path old))
	      (new-vob (clearcase-vob-tag-for-path new)))
	  (if (equal old-vob new-vob)
	      (cleartool "mv -cfile %s \"%s\" \"%s\"" comment old new)
	      (progn
		;; renaming a file accross different vobs is not supported by
		;; ClearCase.
		(copy-file old new)
		(cleartool "mkelem -cfile %s \"%s\"" comment new)
		(cleartool "rmname -cfile %s \"%s\"" comment old)
		(cleartool "checkin -nc \"%s\"" new))))))))

;;;;;; extra-menu

(defvar vc-clearcase-extra-menu)

(easy-menu-define vc-clearcase-extra-menu vc-clearcase-extra-menu
  "Menu for the ClearCase specific commands"
  '("Clearcase"
    ["Show file version" vc-clearcase-what-version t]
    ["Show configspec rule" vc-clearcase-what-rule t]
    ["Show view tag" vc-clearcase-what-view-tag t]
    ["Browse version tree (GUI)" vc-clearcase-gui-vtree-browser t]))

(defun vc-clearcase-extra-menu ()
  vc-clearcase-extra-menu)

;;;; CLEARCASE SPECIFIC COMMANDS
;;;;;
;;;;;; vc-clearcase-get-label-differences
(defun vc-clearcase-get-label-differences (dir label-1 label-2)
  "Return the changed files in DIR between LABEL-1 and LABEL-2.
A list is returned, each element is another list of

  (file version-1 version-2)

Where version-1 is the version attached to LABEL-1 (or the string
*no version* if there is no version for that label.  version-2 is
the same for LABEL-2.

The list of files is not returned in any particular order."
  (setq dir (expand-file-name dir))
  ;; make sure both labels exist
  (with-cleartool-directory dir
    (cleartool "desc -fmt \"ok\" lbtype:%s" label-1)
    (cleartool "desc -fmt \"ok\" lbtype:%s" label-2))

  (let ((buf1 (get-buffer-create " *clearcase-label-1*"))
	(buf2 (get-buffer-create " *clearcase-label-2*"))

	;; Will hold the file report.  KEY is file-name, VALUE is (cons
	;; label-1-version label-2-version).  If one of the versions does not
	;; exist, the string "*no version*" is used instead.
	(report (make-hash-table :test 'equal))

	;; We don't want the full path name in front of each file so we remove
	;; it by skipping SKIP chars.
	(skip (length dir)))

    ;; Start a cleartool find for label-1
    (with-current-buffer buf1
      (buffer-disable-undo)
      (erase-buffer)
      (cleartool-do
       "find" (list dir "-version" (format "lbtype(%s)" label-1) "-print")
       buf1))

    ;; Start a cleartool find for label-2
    (with-current-buffer buf2
      (buffer-disable-undo)
      (erase-buffer)
      (cleartool-do
       "find" (list dir "-version" (format "lbtype(%s)" label-2) "-print")
       buf2))

    ;; Wait for both processes to complete
    (with-timeout (30 (error "Cleartool takes too long to complete"))
      ;; we use ignore-errors because the cleartool sentinel deletes the
      ;; process on exit.
      (while (or (ignore-errors (eq (process-status buf1) 'run))
		 (ignore-errors (eq (process-status buf2) 'run)))
	(accept-process-output)
	(sit-for 1)))

    ;; Process the listed files for label-1.  For each line in the buffer,
    ;; find the file and version and add it to the report.
    (with-current-buffer buf1
      (goto-char (point-min))
      (while (not (= (point) (point-max)))
	(beginning-of-line)
	(forward-char skip)
	(let* ((start (point))
	       (end (progn (end-of-line) (point)))
	       (str (buffer-substring-no-properties start end))
	       (file-and-version (split-string str "@@")))
	  (when (= (length file-and-version) 2)
	    (let ((file (car file-and-version))
		  (version (cadr file-and-version)))
	      (puthash file (cons version "*no version*") report))))
	(forward-line 1)))

    ;; Process the listed files for label-2.  For each line in the buffer,
    ;; find the file and version than update the report with the second
    ;; label's version.  We remove entries that have the same version for both
    ;; labels and add new entries for files that only have label 2.
    (with-current-buffer buf2
      (goto-char (point-min))
      (while (not (= (point) (point-max)))
	(beginning-of-line)
	(forward-char skip)
	(let* ((start (point))
	       (end (progn (end-of-line) (point)))
	       (str (buffer-substring-no-properties start end))
	       (file-and-version (split-string str "@@")))
	  (when (= (length file-and-version) 2)
	    (let ((file (car file-and-version))
		  (version (cadr file-and-version)))
	      (let ((ver1 (car (gethash file report))))
		(cond
		  ;; no version 1
		  ((null ver1)
		   (puthash file (cons "*no version*" version) report))
		  ;; the two versions are identical
		  ((string= ver1 version)
		   (remhash file report))
		  ;; version 1 exists, add version 2
		  (t (puthash file (cons ver1 version) report)))))))
	(forward-line 1)))

    ;; cleanup after us.  maybe these should be in an unwind-protect
    (kill-buffer buf1)
    (kill-buffer buf2)

    ;; collect all files from the hash.  We also remove a leading
    ;; slash or backslash -- cleartool likes to print it, but it
    ;; causes confusion. Also make sure that the current directory is
    ;; "." not "/" as cleartool prints it.
    (loop for k being the hash-keys of report using (hash-values v)
       collect (list
		(let ((file (replace-regexp-in-string "\\`[\\\\/]" "" k)))
		  (if (equal file "") "." file))
		(car v) (cdr v)))))

;;;;;; what-version
(defun vc-clearcase-what-version (file)
  "Show the version of FILE and save the version in the kill ring.

HINT: You can use this function to get the current version of the
file when the version string is too long to type (and in
ClearCase it usually is).  For example, when comparing files
using \\[ediff-revision] you can the current revision of the file
in the kill ring using \\[vc-clearcase-what-version], than when
\\[ediff-revision] asks for a version number you can use \\[yank]
to to put it into the minibuffer and edit it rather than typing
the whole revision string."
  (interactive (list (buffer-file-name (current-buffer))))
  (if (and (stringp file) (vc-clearcase-registered file))
      (progn
	(clearcase-maybe-set-vc-state file)
	(let* ((fprop (clearcase-file-fprop file))
	       (version (clearcase-fprop-version fprop))
	       (co-status (clearcase-fprop-status fprop)))
	  (kill-new version)
	  (message "File version: %s%s" version
		   (case co-status
		     ('reserved ", checkedout reserved")
		     ('unreserved ", checkedout unreserved")
		     ('hijacked ", hijacked")
		     ('broken-view ", broken view")
		     (t "")))))
      (message "Not a clearcase file")))

;;;;;; what-rule
(defun vc-clearcase-what-rule (file)
  "Show the configspec rule for FILE."
  (interactive (list (buffer-file-name (current-buffer))))
  (if (and (stringp file) (vc-clearcase-registered file))
      (progn
	(clearcase-maybe-set-vc-state file)
	(let ((rule (clearcase-fprop-what-rule
		     (clearcase-file-fprop file))))
	  (if rule
	      (message "Configspec rule: %s" rule)
	      (message "No configspec rule"))))
      (message "Not a clearcase file")))

;;;;;; what-view-tag
(defun vc-clearcase-what-view-tag (file)
  "Show view in which FILE resides.
For UCM views, show the current activity as well.  If FILE is
null, the file visited in the current buffer is used."
  (interactive (list (buffer-file-name (current-buffer))))
  (when (stringp file)
    (setq file (expand-file-name file)))
  (unless (and (stringp file) (vc-clearcase-registered file))
    (error "Not a clearcase file: %S" file))
  (clearcase-maybe-set-vc-state file)
  (let ((vprop (clearcase-get-vprop
		(clearcase-file-fprop file))))
    (if (clearcase-ucm-view-p vprop)
	(let ((cact (with-cleartool-directory (file-name-directory file)
		      (cleartool "lsact -cact -fmt \"%%n\""))))
	  (when (string= cact "")
	    (setq cact "NO ACTIVITY"))
	  (message "View: %s (%s)"
		   (clearcase-vprop-name vprop)
		   cact))
	;; else
	(message "View tag: %s" (clearcase-vprop-name vprop)))))

;;;;;; gui-vtree-browser
(defun vc-clearcase-gui-vtree-browser (ask-for-file)
  "Start the version tree browser GUI on a file or directory.
When ASK-FOR-FILE is nil, the file in the current buffer is used.
Otherwise, it will ask for a file (you can also specify a
directory, in this case the versions of the directory itself will
be browsed)"
  (interactive "P")
  (let ((file (buffer-file-name (current-buffer))))
    (when ask-for-file
      (setq file
	    (expand-file-name
	     (read-file-name "Browse vtree for: " file file t))))
    (if (and file (vc-clearcase-registered file))
	(progn
	  (message "Starting Vtree browser...")
	  (start-process-shell-command
	   "Vtree_browser" nil clearcase-vtree-program file))
	(message "Not a clearcase file"))))

;;;;;; clearcase-file-not-found-handler

;;; If we don't autoload this function, the functionality will not be
;;; available until the first ClearCase file is opened.  If we do autoload it,
;;; however, the vc-clearcase.el library will be loaded the first time the
;;; user tries to open a non-existent file.

;;;###autoload
(defun clearcase-file-not-found-handler ()
  "Handle opening of version-extended ClearCase files.
This function should be added to `find-file-not-found-functions'
to handle opening ClearCase files in the format
file.txt@@/main/0.  The function will visit the file first, than
will open the specified version in another window, using
`vc-revision-other-window'"
  (let ((file-name (buffer-file-name))
	(b (current-buffer)))
    (when (string-match "\\(.*\\)@@\\(.*\\)" file-name)
      (let ((file (match-string 1 file-name))
	    (version (match-string 2 file-name)))
	(when (file-exists-p file)
	  (find-file file)
	  (kill-buffer b)
	  (vc-revision-other-window version)
	  t                             ; return t, so no other handler works
	  )))))

;;;;;; clearcase-hijack-file-handler
(defun clearcase-hijack-file-handler ()
  "Detect the hijacking of a file and update the FPROP accordingly."
  (let ((file (buffer-file-name)))
    (when file
      (let ((fprop (clearcase-file-fprop file)))
	(when fprop
	  (unless (clearcase-fprop-checkedout-p fprop)
	    ;; we are hijacking a file
	    (vc-file-setprop file 'vc-state 'unlocked-changes)
	    (setf (clearcase-fprop-status fprop) 'hijacked)
	    (vc-resynch-buffer file t t))))))
  nil)

;;;;;; checkout-directory
;;;###autoload
(defun vc-clearcase-checkout-directory (dir)
  "Checkout directory DIR, or do nothing if already checked out.
To register, rename or remove files, the directory needs to be
checked out.  vc-clearcase will checkout a directory when needed
and check it in again, but if you need to register (or rename or
delete) several files, a new directory revision will be created
for each operation.  In such a case, it might be useful to
checkout/checkin the directory explicitely, so that a single
directory revision is created instead."
  (interactive "D")
  (setq dir (expand-file-name dir))
  (if (member (cleartool "desc -fmt \"%%Rf\" \"%s\"" dir)
	      '("reserved" "unreserved"))
      (message "%s already checked out" dir)
      (with-temp-message (format "Checking out %s" dir)
	(cleartool "checkout -nc \"%s\"" dir))))

;;;;;; checkin-directory
;;;###autoload
(defun vc-clearcase-checkin-directory (dir)
  "Checkin directory DIR, or do nothing if already checked in.
See `vc-clearcase-checkout-directory' for why this function might
be usefull."
  (interactive "D")
  (setq dir (expand-file-name dir))
  (if (equal "" (cleartool "desc -fmt \"%%Rf\" \"%s\"" dir))
      (message "%s is not checked out" dir)
      (with-temp-message (format "Checking in %s" dir)
	(cleartool "checkin -nc \"%s\"" dir))))

;;;;;; list-checkouts
(defconst cleartool-lsco-fmt
  (concat "Working file: %n\n"
	  "revision %PVn (%Rf)\n"
	  "user: %u; view: %Tf; date: %Sd (%Ad days ago)\n\n"
	  "%c")
  "Format string to use when listing checkouts.")

;;;###autoload
(defun vc-clearcase-list-checkouts (dir &optional prefix-arg)
  "List the checkouts of the current user in DIR.
If PREFIX-ARG is present, an user name can be entered, and all
the views are searched for checkouts of the specified user.  If
the entered user name is empty, checkouts from all the users on
all the views are listed."
  (interactive "DList checkouts in directory: \nP")
  (when (string-match "\\(\\\\\\|/\\)$" dir)
    (setq dir (replace-match "" nil nil dir)))
  (setq dir (expand-file-name dir))
  (let ((user-selection
	 (if prefix-arg
	     (let ((u (read-from-minibuffer "User: ")))
	       (unless (string= u "")
		 (list "-user" u)))
	     (list "-me" "-cview")))
	(other-options (list "-recurse" "-fmt" cleartool-lsco-fmt dir)))
    (with-current-buffer (get-buffer-create "*clearcase-lsco*")
      (let ((inhibit-read-only t))
	(erase-buffer)
	(setq default-directory dir)
	(insert "Listing checkouts in " dir "\n")
	(insert "Cleartool command: "
		(format "%S" (append user-selection other-options))
		"\n")
	(vc-clearcase-log-view-mode)
	(let ((process
	       (cleartool-do
		"lsco"
		(append user-selection other-options) (current-buffer))))
	  (switch-to-buffer-other-window (process-buffer process)))))))

;;;;;; update-view
;;;###autoload
(defun vc-clearcase-update-view (dir prefix-arg)
  "Run a cleartool update command in DIR and display the results.
With PREFIX-ARG, run update in preview mode (no actual changes
are made to the views)."
  (interactive "DUpdate directory: \nP")
  (when (string-match "[\\\/]+$" dir)
    (setq dir (substring dir 0 (match-beginning 0))))
  (setq dir (expand-file-name dir))
  (with-current-buffer (get-buffer-create "*clearcase-update*")
    (setq buffer-read-only t)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (when prefix-arg (insert "*PREVIEW* "))
      (insert "Updating directory " dir "\n")
      (let ((options (list "-force" "-rename" dir)))
	(when prefix-arg (setq options (cons "-print" options)))
	(let ((process (cleartool-do "update" options (current-buffer))))
	  (switch-to-buffer-other-window (process-buffer process))
	  ;; TODO: how do we refresh all the files that were loaded
	  ;; from this view?
	  )))))

;;;;;; label-diff-report
(when (fboundp 'define-button-type)
  (define-button-type 'vc-clearcase-start-ediff-button
      'face 'default
      'help-echo "mouse-2, RET: Compare the two file revisions with Ediff"
      'follow-link t
      'action (lambda (button)
		(require 'ediff)
		(declare (special ediff-version-control-package))
		(let ((file-name (button-get button 'file-name))
		      (revision-1 (button-get button 'revision-1))
		      (revision-2 (button-get button 'revision-2)))
		  (with-current-buffer (find-file-noselect file-name)
		    (funcall
		     (intern (format "ediff-%S-internal"
				     ediff-version-control-package))
		     revision-1 revision-2 nil))))
      'skip t))

;;;###autoload
(defun vc-clearcase-label-diff-report (dir label-1 label-2)
  "Report the changed file revisions between labels.
A report is prepared in the *label-diff-report* buffer for the
files in DIR that have different revisions between LABEL-1
and LABEL-2'."
  (interactive
   (let ((d (read-file-name "Report on directory: "
			    default-directory default-directory t nil)))
     (assert (file-directory-p d))
     (let* ((vob (clearcase-vob-tag-for-path d))
	    (l1 (clearcase-read-label "Label 1 (newer): " vob))
	    (l2 (clearcase-read-label "Label 2 (older): " vob nil t)))
       (list d l1 l2))))
  (setq dir (expand-file-name dir))
  (message "Fetching label differences...")
  (let ((diff (vc-clearcase-get-label-differences dir label-1 label-2))
	;; the format string for a line in the report
	line-fmt)
    (setq diff (sort* diff 'string< :key 'car))
    (loop for (file rev-1 rev-2) in diff
       maximize (length file) into file-len
       maximize (length rev-1) into lb1-len
       maximize (length rev-2) into lb2-len
       finally do
       (setq line-fmt (format "%% 3d    %%-%ds    %%-%ds    %%-%ds"
			      file-len lb1-len lb2-len)))

    (with-current-buffer (get-buffer-create "*label-diff-report*")
      ;; these are declared in ps-print.el, but I want to avoid an
      ;; (eval-when-compile (require 'ps-print))
      (declare
       (special ps-landscape-mode ps-number-of-columns ps-zebra-stripes))
      (set (make-local-variable 'ps-landscape-mode) 'landscape)
      (set (make-local-variable 'ps-number-of-columns) 1)
      (set (make-local-variable 'ps-zebra-stripes) t)

      (setq default-directory dir)
      (erase-buffer)
      (buffer-disable-undo)
      (insert (format "Directory: %s\nLabel 1: %s\nLabel 2: %s\n\n"
		      dir label-1 label-2))
      (let ((header (format line-fmt 0 "File" label-1 label-2)))
	(insert header)
	(insert "\n")
	(insert (make-string (length header) ?=)))
      (insert "\n")

      (loop for (file rev-1 rev-2) in diff
	 count 1 into pos
	 do (progn
	      (if (fboundp 'insert-text-button)
		  (insert-text-button
		   (format line-fmt pos file rev-1 rev-2)
		   'type 'vc-clearcase-start-ediff-button
		   'file-name file 'revision-1 rev-1 'revision-2 rev-2)
		  (insert (format line-fmt pos file rev-1 rev-2)))
	      (insert "\n")))

      (goto-char (point-min))
      (buffer-enable-undo)
      (pop-to-buffer (current-buffer))))
  (message "Fetching label differencess...done."))

;;;;;; list-view-private-files
;;;###autoload
(defun vc-clearcase-list-view-private-files (dir)
  "List the view private files in DIR.
You can edit the files using 'find-file-at-point'"
  (interactive "DReport on directory: ")
  (setq dir (expand-file-name dir))
  (let ((buf (get-buffer-create "*clearcase-view-private-files*")))
    (with-current-buffer buf
      (buffer-disable-undo)
      (erase-buffer)
      (cd dir)
      (insert (format "View private files in %s:\n\n" dir))
      (cleartool-do "ls" (list "-recurse" "-short" "-view_only") buf))
    (pop-to-buffer buf)))


;;;;;; configspec editor

(defvar clearcase-edcs-view-tag nil
  "The name of the view whose configspec we are editing.")

;; NOTE: for some configspecs cleartool will want to ask questions, I
;; didn't find a way to turn that off.

(defun clearcase-setcs (&optional buffer view-tag)
  "Set the configspec found in BUFFER to the VIEW-TAG.

If buffer is nil, the current buffer is used.  If view-tag is
nil, the value of clearcase-edcs-view-tag is used (local to
buffer).  This function should be invoked on a buffer setup by
`vc-clearcase-edcs'.

NOTE: we can only set the configspec if view-tag has an
associated vprop to tell us if it's a dynamic or static view.
This usually means you have to visit a file in that view before
you can set the configspec.  This note is here, because
`vc-clearcase-edcs' allows you to inspect the configspec of ANY
view accessible from this machine."
  (interactive (list (current-buffer)
		     (with-current-buffer (current-buffer)
		       clearcase-edcs-view-tag)))
  (let ((vprop (clearcase-get-vprop view-tag))
	(configspec (buffer-file-name buffer)))
    (unless configspec (error "Buffer has no file associated with it"))
    (unless (or (clearcase-dynamic-view-p vprop)
		(clearcase-snapshot-view-p vprop))
      (error "Nothing known about %s.  Visit a file in the view first"
	     view-tag))
    (when (buffer-modified-p buffer)
      (if (yes-or-no-p (format "Save %s? " configspec))
	  (save-buffer buffer)
	  (error "Aborted")))
    (cond
      ((clearcase-dynamic-view-p vprop)
       ;; in a dynamic view, we simply set the configspec, than
       ;; trigger a re-sync on all the visited files from that view.
       (cleartool "setcs -tag %s \"%s\"" view-tag configspec)
       (message "%s's confispec updated." view-tag)
       (clearcase-refresh-files-in-view view-tag))

      ((clearcase-snapshot-view-p vprop)
       ;; in a snapshot view, a update will be triggered, so we set
       ;; the configspec with a cleartool-do command, and trigger
       ;; the re-sync in its finished callback.
       (with-current-buffer (get-buffer-create "*clearcase-setcs*")
	 (let ((inhibit-read-only t))
	   (erase-buffer)
	   (cd (clearcase-vprop-root-path vprop))
	   (let ((process
		  (cleartool-do "setcs"
				(list "-tag" view-tag configspec)
				(current-buffer))))
	     (switch-to-buffer-other-window (process-buffer process))
	     ;; reuse this variable to hold the view tag in the update
	     ;; buffer.
	     (set (make-local-variable 'clearcase-edcs-view-tag) view-tag)
	     (setq cleartool-finished-function
		   '(lambda ()
		     (clearcase-refresh-files-in-view
		      clearcase-edcs-view-tag))))))))))

(defun clearcase-setcs-and-kill-buffer (&optional buffer view-tag)
  "Set the configspec found in BUFFER to the VIEW-TAG than kill the buffer."
  (interactive (list (current-buffer)
		     (with-current-buffer (current-buffer)
		       clearcase-edcs-view-tag)))
  (clearcase-setcs buffer view-tag)
  (let ((window (get-buffer-window buffer t)))
    (when (and window (not (window-dedicated-p window)))
      (ignore-errors (delete-window window))))
  (kill-buffer buffer))

;;; TODO: the highlighted keyword list is incomplete.
(defconst clearcase-edcs-font-lock-keywords
  `(("^\\s-*\\(element\\|load\\|include\\)" 1 font-lock-keyword-face)
    ("^\\s-*element\\s-+\\(-file\\|-directory\\|-eltype\\s-+\\sw+\\)\\s-" 1 font-lock-type-face)
    ("\\s-\\(-mkbranch\\)\\s-+\\(\\sw+\\)" (1 font-lock-type-face) (2 font-lock-variable-name-face))
    ("\\s-\\(-time\\)\\s-+\\(\\sw+\\)" (1 font-lock-type-face) (2 font-lock-variable-name-face))
    ("\\s-\\(-nocheckout\\b\\)" 1 font-lock-type-face))
  "Font lock keywords for highlighting config spec files.")

(defvar clearcase-edcs-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; comment starter
    (modify-syntax-entry ?# "<" table)
    ;; newline and formfeed end coments
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\f ">" table)
    ;; underscore and minus sign are word constituents
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?- "w" table)
    table)
  "Syntax table used in `clearcase-edcs-mode'.")

(define-derived-mode clearcase-edcs-mode fundamental-mode
  "Configspec"
  "Generic mode to edit clearcase configspecs."
  (make-local-variable 'clearcase-edcs-view-tag)
  ;; 'adapted' from values in emacs-lisp-mode
  (setq comment-start "#"
	comment-start-skip "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)#+ *"
	comment-end ""
	comment-end-skip nil)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(clearcase-edcs-font-lock-keywords nil t))
  (font-lock-mode t))

;; Provide a shorter alias for the edcs mode.  This is useful if you
;; want to keep configspecs separately and have mode tags in them.
(defalias 'edcs-mode 'clearcase-edcs-mode)


(easy-mmode-defmap clearcase-edcs-mode-map
		   '(("\C-c\C-s" . clearcase-setcs)
		     ("\C-c\C-c" . clearcase-setcs-and-kill-buffer))
		   "Keymap for Clearcase Edit Configspec mode")


;;;###autoload
(defun vc-clearcase-edcs (view-tag)
  "Fetch the config spec for VIEW-TAG and pop up a buffer with it.
In interactive mode, prompts for a view-tag name with the default
of the current file's view-tag."
  (interactive
   (list
    (clearcase-read-view-tag
     "Edit configspec for view: "
     ;; get an initial view-tag if possible.
     (let ((file (buffer-file-name (current-buffer))))
       (when (and file (vc-clearcase-registered file))
	 (clearcase-fprop-view-tag (clearcase-file-fprop file)))))))

  (message "Fetching configspec for %s" view-tag)
  (let ((tid (cleartool-ask (concat "catcs -tag " view-tag) 'nowait))
	(csfile (format "%s%s.configspec" temporary-file-directory view-tag)))
    (with-current-buffer (find-file-noselect csfile)
      (clearcase-edcs-mode)
      (setq clearcase-edcs-view-tag view-tag)
      (buffer-disable-undo)
      (erase-buffer)
      (insert (cleartool-wait-for tid))
      (buffer-enable-undo)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer))
      (message "Edit your configspec.  Type C-c C-c when done."))))

;;;;;; start-view
;;;###autoload
(defun vc-clearcase-start-view (view-tag)
  "Start the dynamic view for VIEW-TAG.
In interactive mode, prompts for a view-tag name."
  (interactive (list (clearcase-read-view-tag "Start dynamic view: ")))
  (message "Starting %s dynamic view..." view-tag)
  (message (cleartool "startview %s" view-tag))
  (message "Starting %s dynamic view...done." view-tag))

;;;; Update vc keymap

;;; bind the extra clearcase commands to keys and menus in the vc
;;; keymap

;;;###autoload
(progn
  (define-key vc-prefix-map "e" 'vc-clearcase-edcs)
  (define-key vc-prefix-map "f" 'vc-clearcase-start-view)
  (define-key vc-prefix-map "j" 'vc-clearcase-gui-vtree-browser)
  (define-key vc-prefix-map "o" 'vc-clearcase-list-checkouts)
  (define-key vc-prefix-map "p" 'vc-clearcase-update-view)
  (define-key vc-prefix-map "t" 'vc-clearcase-what-view-tag)
  (define-key vc-prefix-map "w" 'vc-clearcase-what-rule)
  (define-key vc-prefix-map "y" 'vc-clearcase-what-version))

;; 'borrowed' from pcvs-defs.el, Clearcase commands that are not file
;; related will go in a Clearcase menu under Tools.
;;;###autoload
(defvar clearcase-global-menu)

;;;###autoload
(defvar clearcase-global-menu-map (make-sparse-keymap))

;;;###autoload
(easy-menu-define clearcase-global-menu clearcase-global-menu-map
  "Menu for the extra (non-file related) ClearCase functionality"
  '("ClearCase"
    ["Checkout directory..." vc-clearcase-checkout-directory t]
    ["Checkin directory..." vc-clearcase-checkin-directory t]
    "----"
    ["Start dynamic view..." vc-clearcase-start-view t]
    ["Edit Configspec..." vc-clearcase-edcs t]
    ["Update snapshot view..." vc-clearcase-update-view t]
    ["List Checkouts..." vc-clearcase-list-checkouts t]
    ["List View Private Files..." vc-clearcase-list-view-private-files t]
    ["Label diff report..." vc-clearcase-label-diff-report t]
    "----"
    ["Report bug in vc-clearcase..." vc-clearcase-report-bug t]))

;;;###autoload
(easy-menu-add-item
 menu-bar-tools-menu '() clearcase-global-menu "PCL-CVS")

;;;; Debugging aids, reporting bugs

(defun clearcase-version ()
  "Return the clearcase version as a string.
This is the string returned by the cleartool -version command."
  (with-temp-buffer
    (setq cleartool-finished-function (lambda () (throw 'done nil)))
    (cleartool-do "-version" nil (current-buffer))
    (catch 'done (while t (sit-for 0.1)))
    (replace-regexp-in-string "\r\n?" "\n" (buffer-string))))

;; To update vc-clearcase-report-bug, use M-x occur <RET>
;; def\(var\|const\|custom\) <RET>.  `cleartool-last-command-timestamp' is
;; treated specially (see below)

(defconst clearcase-never-report
  '(clearcase-log-view-mode-abbrev-table
    clearcase-log-view-mode-syntax-table
    vc-clearcase-log-view-mode-abbrev-table
    vc-clearcase-log-view-mode-map
    vc-clearcase-log-view-mode-syntax-table
    clearcase-global-menu
    clearcase-global-menu-map
    vc-clearcase-extra-menu
    clearcase-edcs-mode-syntax-table
    clearcase-log-view-mode-map
    clearcase-edcs-mode-map
    clearcase-edcs-mode-abbrev-table
    clearcase-annotate-date-rx
    clearcase-annotate-months
    clearcase-log-view-font-lock-keywords
    cleartool-tq

    ;; this is handled specially in `vc-clearcase-report-bug'
    cleartool-last-command-timestamp)
  "A list of variables which we don't include in the error report.
`vc-clearcase-report-bug' will report all variables in the
vc-clearcase package except the ones listed above.  It is much
easier to keep it up-to-date this way.")

;;;###autoload
(defun vc-clearcase-report-bug ()
  "Submit via mail a bug report on vc-clearcase.el."
  (interactive)
  (require 'reporter)
  (let ((reporter-prompt-for-summary-p t)
	(varlist ))

    ;; collect all variables we want to report on
    (mapatoms
     (lambda (a)
       (when (and (boundp a)
		  (string-match "^\\(ah-\\|vc-\\)?clear\\(case\\|tool\\)-"
				(symbol-name a))
		  (not (memq a clearcase-never-report)))
	 (push a varlist)))
     obarray)

    (push (cons 'cleartool-last-command-timestamp
		(lambda (x buf)
		  (let ((name (symbol-name x))
			(value (symbol-value x)))
		    (insert name " " (format "%.3f" value)
			    "; " (format "%.3f" (- (float-time) value))
			    " seconds ago")
		    (lisp-indent-line)
		    (insert "\n"))))
	  varlist)

    ;; sort varlist, as it is easier to search in the error report.
    (setq varlist
	  (sort varlist
		'(lambda (a b)
		  (let ((a-name (symbol-name (if (consp a) (car a) a)))
			(b-name (symbol-name (if (consp b) (car b) b))))
		    (string< a-name b-name)))))

    (reporter-submit-bug-report
     vc-clearcase-maintainer-address "vc-clearcase.el" varlist

     (lambda ()
       (insert "\n\nClearCase version:\n==================\n\n"
	       (clearcase-version))
       (insert "\n\nContents of *cleartool-aborts*:\n"
	       "===============================\n\n")
       (if cleartool-save-stop-data
	   (ignore-errors (insert-buffer-substring
			   (get-buffer "*cleartool-aborts*")))
	   (insert "cleartool-save-stop-data is nil, "
		   "*cleartool-aborts* is not available"))
       (insert "\n")
       (insert "\n\nContents of *cleartool-tq-trace*:\n"
	       "=================================\n\n")
       (let ((buf (get-buffer "*cleartool-tq-trace*")))
	 (if buf
	     (insert-buffer-substring buf)
	     (insert "buffer *cleartool-tq-trace* is not available")))
       (insert "\n")))))

(defvar clearcase-function-to-trace
  '(cleartool-ask cleartool-wait-for cleartool-tq-handler)
  "List of function to trace.
See `clearcase-trace-cleartool-tq' and
`clearcase-untrace-cleartool-tq'")

(defun clearcase-trace-cleartool-tq ()
  "Trace some of the cleartool commands."
  (interactive)
  (let ((trace-buf (get-buffer-create "*cleartool-tq-trace*")))
    (dolist (f clearcase-function-to-trace)
      (trace-function-background f trace-buf))))

(defun clearcase-untrace-cleartool-tq ()
  "Disable tracing of cleartool commands."
  (interactive)
  (dolist (f clearcase-function-to-trace)
    (untrace-function f)))

;;;; Finish up

;; This does not have to be autoloaded, we only need to detect hijacking of
;; files after we load vc-clearcase
(add-hook 'after-save-hook 'clearcase-hijack-file-handler)

;; Version controlled backups for ClearCase files are in the format:
;; file.c.~_main_some_branch_3~.  Similarly, ClearCase will also create
;; 'backups' by appending a .keep or .contrib to the filename. These are not
;; currently not recognized by Emacs, so we add in an auto-mode-alist that
;; strips off the version part so the file type can be properly identified.

;;;###autoload
(let ((backup-regexp "\\.~[a-zA-Z0-9_-~]+\\'")
      (garbage-regexp "\\.\\(contrib\\|keep\\)\\(\\.[0-9]+\\)?\\'"))
  (unless (assoc backup-regexp auto-mode-alist)
    (push (list backup-regexp nil t) auto-mode-alist))
  (unless (assoc garbage-regexp auto-mode-alist)
    (push (list garbage-regexp nil t) auto-mode-alist)))

(defun vc-clearcase-unload-hook ()
  (remove-hook 'after-save-hook 'clearcase-hijack-file-handler)
  (cond
    ((boundp 'find-file-not-found-functions)
     (remove-hook 'find-file-not-found-functions 'clearcase-file-not-found-handler))
    ((boundp 'find-file-not-found-hooks)
     (remove-hook 'find-file-not-found-hooks 'clearcase-file-not-found-handler)))
  (ad-activate 'vc-dired-hook)
  (ad-disable-advice
   'vc-version-backup-file-name 'after 'clearcase-cleanup-version)
  (ad-activate 'vc-version-backup-file-name)
  (ad-disable-advice
   'vc-start-logentry 'before 'clearcase-prepare-checkin-comment)
  (ad-activate 'vc-start-logentry)
  (ad-disable-advice
   'vc-create-snapshot 'before 'clearcase-provide-label-completion)
  (ad-activate 'vc-create-snapshot)
  ;; unfortunately, I don't know how to restore the autoload for the
  ;; `vc-clearcase-registered' so we remove ourselves completely
  (setq vc-handled-backends (delq 'CLEARCASE vc-handled-backends)))

(add-hook 'vc-clearcase-unload-hook 'vc-clearcase-unload-hook)

;; Activate the advices and start cleartool when we are loaded, we will need
;; it anyway (but only if we find the cleartool program (meaning ClearCase is
;; installed on the system)
(cond ((not (executable-find cleartool-program))
       (message "cleartool executable not found, disabling vc-clearcase"))
      ((< emacs-major-version 23)
       (message "this version of vc-clearcase only works with GNU Emacs 23"))
      (t
       (ad-activate 'vc-version-backup-file-name)
       (ad-activate 'vc-start-logentry)
       (ad-activate 'vc-create-snapshot)
       (cleartool-tq-maybe-start)))

;; Bug 1818879: Only add 'CLEARCASE to `vc-handled-backends' and start the
;; transaction queue when we can find cleartool.

;;;###autoload
(when (and (executable-find cleartool-program)
	   (>= emacs-major-version 23))
  (cond
    ((boundp 'find-file-not-found-functions)
     (add-hook 'find-file-not-found-functions
	       'clearcase-file-not-found-handler))
    ((boundp 'find-file-not-found-hooks)
     (add-hook 'find-file-not-found-hooks
	       'clearcase-file-not-found-handler)))
  (if (boundp 'vc-handled-backends)
      (unless (memq 'CLEARCASE vc-handled-backends)
	(setq vc-handled-backends (nconc vc-handled-backends '(CLEARCASE))))
      (setq vc-handled-backends '(RCS CVS CLEARCASE))))

(provide 'vc-clearcase)

;;; Local Variables:
;;; mode: emacs-lisp
;;; mode: outline-minor
;;; outline-regexp: ";;;;+"
;;; End:

;;; vc-clearcase.el ends here
