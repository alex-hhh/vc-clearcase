;; vc-clearcase.el --- support for ClearCase version control system
;;
;; Copyright (C) 2006 Alexandru Harsanyi
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
;; $Id$

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
;; To use this package you need to put it somewhere in your load-path
;; (for example the site-lisp directory), byte compile it, than add
;; the follwing line to your ~/.emacs file:
;;
;;     (require 'vc-clearcase)
;;
;; You can also generate autoloads for the functions in this file,
;; this will delay loading vc-clearcase.el until it is actually
;; needed.

;; The code below will byte-compile vc-clearcase.el and generate the
;; autoloads in a file named vc-clearcase-auto.el in the same
;; directory as vc-clearcase.el. If you don't have vc-clearcase.el
;; opened, or multiple copies of it are opened, we ask for the file
;; name explicitely.
;;
;; After the code below runs, the compilation results will be in a
;; buffer named "*Compile-Log* (which you can switch to).  The
;; vc-clearcase-auto.el file will be saved.
;;
;; After you generated the vc-clearcase-auto.el file, you need to add
;; the follwing line to ~/.emacs:
;;
;;    (load "vc-clearcase-auto")
;;
;; To evaluate the code below, copy it into the *scratch* buffer,
;; remove the comments, move the cursor at the end of it and type C-x
;; C-e
;;
;; There are two versions of the code, this is the Emacs 22 version:
;;
;; (save-window-excursion
;;   (require 'autoload)
;;   (with-current-buffer
;;       (or (get-buffer "vc-clearcase.el")
;;           (find-file
;;            (read-file-name "Find vc-clearcase.el: " nil nil 'must-match)))
;;     (let* ((file (buffer-file-name (current-buffer)))
;;            (base (file-name-nondirectory file))
;;            (dir (file-name-directory file))
;;            (generated-autoload-file
;;             (expand-file-name "vc-clearcase-auto.el" dir)))
;;       (unless (equal base "vc-clearcase.el")
;;         (error "Expecting a file named vc-clearcase.el, got %s" base))
;;       ;; byte-compile-file returns nil if there were errors
;;       (unless (byte-compile-file file)
;;         (error
;;          "Failed to compile %s, check the *Compile-Log* buffer for errors"
;;          file))
;;       ;; update-file-autoloads returns nil if no autoloads were found.
;;       (when (update-file-autoloads file 'save-after)
;;         (error "Failed to find autoloads in %s" file)))))
;;
;; This is the Emacs 21 version:
;;
;; (save-window-excursion
;;   (require 'autoload)
;;   (with-current-buffer
;;       (or (get-buffer "vc-clearcase.el")
;;           (find-file
;;            (read-file-name "Find vc-clearcase.el: " nil nil 'must-match)))
;;     (let* ((file (buffer-file-name (current-buffer)))
;;            (base (file-name-nondirectory file))
;;            (dir (file-name-directory file))
;;            (generated-autoload-file
;;             (expand-file-name "vc-clearcase-auto.el" dir)))
;;       (unless (equal base "vc-clearcase.el")
;;         (error "Expecting a file named vc-clearcase.el, got %s" base))
;;       ;; byte-compile-file returns nil if there were errors
;;       (unless (byte-compile-file file)
;;         (error
;;          "Failed to compile %s, check the *Compile-Log* buffer for errors"
;;          file))
;;       (with-current-buffer (find-file generated-autoload-file)
;;         (erase-buffer)
;;         (insert "\f")
;;         (generate-file-autoloads file)
;;         (save-buffer)))))
;;

;;;;; Implementation notes:
;;
;; This package uses the following prefixes for its functions and
;; variables:
;;
;;  ah-cleartool -- used for all cleartool related functionality
;; (running cleartool commands or communicating with the cleartool
;; transaction queue)
;;
;; ah-clearcase -- used for all clearcase functionality (support
;; functions for the VC package)
;;
;; vc-clearcase -- prefix used by the VC interface functions (required
;; by vc.el) and the additional functions which perform vc operations.
;;
;; the 'ah' prefix stands for Alex Harsanyi and is used to avoid
;; conflicts with the clearcase.el package.
;;
;; In addition, three macros are defined: with-clearcase-checkout,
;; with-clearcase-cfile and ignore-cleartool-errors if you compile
;; this file they will not pollute the Emacs namespace.


;;;;; Implementation status of vc backend functions from vc.el
;;
;;
;; STATE-QUERYING FUNCTIONS (*) is mandatory, (-) is optional
;;
;; * registered (file) -- implemented
;;
;; * state (file) -- implemented, except that we don't return the
;;   "USER" state (meaning that another user has a reserved checkout
;;   on the file).  vc.el will try to steal the lock if we do that and
;;   that is not always the right thing to do.
;;
;; - state-heuristic (file) -- implemented, see `state'
;;
;; - dir-state (dir) -- not implemented
;;
;; * workfile-version (file) -- implemented
;;
;; - latest-on-branch-p (file) -- implemented
;;
;; * checkout-model (file) -- implemented
;;
;; - workfile-unchanged-p (file) -- implemented
;;
;; - mode-line-string (file) -- implemented
;;
;; - dired-state-info (file) -- not implemented (default is fine)
;;
;;
;; STATE-CHANGING FUNCTIONS
;;
;; * register (file &optional rev comment) -- implemented
;;
;; - init-version (file) -- not implemented, you don't choose the
;;   initial version in ClearCase
;;
;; - responsible-p (file) -- implemented
;;
;; - could-register (file) -- not implemented, default implementation
;;   is fine.  Not really clear what it should do...
;;
;; - receive-file (file rev) -- not implemented
;;
;; - unregister (file) -- not implemented
;;
;; * checkin (file rev comment) -- implemented, but `rev' is ignored;
;;   vc-checkin-switches is ignored
;;
;; * find-version (file rev buffer) -- implemented, but will not
;;   check-out the head of the trunk: will sigal an error when
;;   (string= rev "".  Head of trunk has no meaning in ClearCase.
;;
;; * checkout (file &optional editable rev) -- implemented, but not
;;   all parameter combinations are allowed.  Furthermore, the
;;   function is asynchronous (like checkin), it pops up a buffer for
;;   the checkout comment and finishes the checkout later.
;;
;; * revert (file &optional contents-done) -- implemented, but
;;   `contents-done' is ignored.
;;
;; - cancel-version (file editable) -- not implemented, ClearCase does
;;   provide this functionality, but I consider it too dangerous.
;;
;; - merge (file rev1 rev2) -- implemented, but the operation will
;;   throw an error if the merge cannot be done automatically.  vc.el
;;   assumes there are conflict markers in the buffer, but ClearCase
;;   does not use them.
;;
;; - merge-news (file) -- implemented, same restrictions as for
;;   `merge'
;;
;; - steal-lock (file &optional version) -- not implemented.  Maybe it
;;   could be implemented as unreserving someone elses reserved
;;   checkout.  vc.el seems to handle locking by someone else in a
;;   limited way (compared to ClearCase).  vc-clearcase-state will
;;   never return a string (which signifies that the file is locked by
;;   someone else) because vc.el will always want to steal the lock.
;;   Instead, vc-clearcase-checkout will ask the user if he wants to
;;   checkout the file unreserved.
;;
;;
;; HISTORY FUNCTIONS
;;
;; * print-log (file &optional buffer) -- implemented, but as a
;;   separate function (vc-print-log is defadvice'd).  ClearCase logs
;;   look nothing like CVS logs, especially since revision names are
;;   soo different.
;;
;; - show-log-entry (version) -- implemented, but see `print-log'
;;
;; - wash-log (file) -- not implemented
;;
;; - logentry-check () -- not implemented
;;
;; - comment-history (file) -- not implemented
;;
;; - update-changelog (files) -- not implemented
;;
;; * diff (file &optional rev1 rev2 buffer) -- implemented.
;;
;; - diff-tree (dir &optional rev1 rev2) -- not implemented (new in
;;   Emacs 22)
;;
;; - annotate-command (file buf &optional rev) -- implemented, but
;;   runs annotate asynchronously
;;
;; - annotate-time () -- implemented (new in Emacs 22), we also
;;   implement annotate-difference which is Emacs 21.
;;
;; - annotate-current-time () -- not implemented (new in Emacs 22)
;;
;; - annotate-extract-revision-at-line () -- implemented (new in Emacs
;;   22)
;;
;;
;; SNAPSHOT SYSTEM
;;
;; - create-snapshot (dir name branchp) -- implemented, but `branchp'
;;   is used to move a label.  You cannot make branches in ClearCase
;;   in the way vc.el would like
;;
;; - assign-name (file name) -- not implemented
;;
;; - retrieve-snapshot (dir name update) -- not implemented as it is not
;;   applicable for ClearCase.  See `vc-clearcase-edcs' for an
;;   alternative.
;;
;;
;; MISCELLANEOUS
;;
;; - make-version-backups-p (file) -- not implemented
;;
;; - repository-hostname (dirname) -- not implemented
;;
;; - previous-version (file rev) -- implemented
;;
;; - next-version (file rev) -- implemented
;;
;; - check-headers () -- not implemented, version headers don't seem
;;   to exist in ClearCase.
;;
;; - clear-headers () -- not implemented
;;
;; - delete-file (file) -- not implemented
;;
;; - rename-file (old new) -- implemented
;;
;; - find-file-hook () -- not implemented
;;
;; - find-file-not-found-hook () -- not implemented
;;
;; CLEARCASE SPECIFIC FUNCTIONS
;; (see their help text for details)
;;
;; - vc-clearcase-what-version
;; - vc-clearcase-what-rule
;; - vc-clearcase-what-view-tag
;; - vc-clearcase-gui-vtree-browser
;; - vc-clearcase-list-checkouts
;; - vc-clearcase-update-view
;; - vc-clearcase-label-diff-report
;; - vc-clearcase-list-view-private-files
;; - vc-clearcase-edcs
;; - vc-clearcase-start-view

;;; History:
;;


;;; Code:

;;;; Initial requires and setup

(require 'tq)
(require 'vc-hooks)
(require 'vc)

(eval-when-compile
  (require 'cl)
  (require 'cc-defs)                    ; for c-point
  (require 'trace) ; avoid compiler complaint w.r.t undefined untrace-function
  )

(defconst vc-clearcase-cvsid
  "$Id$")

(defconst vc-clearcase-maintainer-address "haral@users.sourceforge.net")

;;;###autoload
(defgroup vc-clearcase nil
  "Support for the ClearCase version control system."
  :group 'tools)

;;;###autoload
(defcustom ah-clearcase-cleartool-program "cleartool"
  "The name of the cleartool executable."
  :type 'string
  :group 'vc-clearcase)

(defcustom ah-clearcase-vtree-program
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
;; The 'user' functions from this section are `ah-cleartool-ask' and
;; `ah-cleartool-wait-for'.


(defvar ah-cleartool-tq nil
  "The transaction queue to cleartool.")

(defvar ah-cleartool-next-command 1
  "Command counter for cleartool commands.

Used to track if someone else is sending commands to cleartool,
or if two commands were sent in one go (e.g. \"cd\nls\n\")")

(defconst ah-cleartool-status-rx
  (concat "Command \\([0-9]+\\) returned status \\([0-9]+\\)[ \t\r\n]+"
          ;; under Windows NT, we communicate with cleartool using
          ;; pipes (instead of PTY's, and cleartool won't output a
          ;; prompt...
          (unless (eq system-type 'windows-nt)
            "cleartool \\([0-9]+\\)+>[ \t\r\n]+"))
  "Regexp to match the end of each cleartool result.

If it does not match properly, tq will never pass back the answer
to us." )

(defconst ah-cleartool-question-rx
  "\\(\\[yes\\]\\|\\[no\\]\\)[ \t\r\n]*$"
  "A regexp that matches a question asked by cleartool.")

(defconst ah-cleartool-tq-rx
  (concat "\\(" ah-cleartool-status-rx
          "\\)\\|\\(" ah-cleartool-question-rx "\\)"))

(defcustom ah-cleartool-timeout 20
  "Timeout (in seconds) for cleartool commands.
This is actually the amount of time cleartool has to be inactive
when receiving data from it, not the total transaction time."
  :type 'integer
  :group 'vc-clearcase)

(defcustom ah-cleartool-idle-timeout 900 ; 15 minutes
  "Kill the cleartool command if idle for this many seconds.
The reason for this variable is that cleartool seems unresponsive
after long periods of inactivity.  Note that cleartool will only
be killed when we try to use it for a new command and the idle
timer has expired."
  :type 'integer
  :group 'vc-clearcase)


(defvar ah-cleartool-last-command-timestamp (float-time)
  "Timestamp when the last cleartool command was issued.
Used by `ah-cleartool-ask' to know when to restart cleartool.")

(defvar ah-cleartool-ctid 0
  "The ID of the last completed transaction.
This is an incrementing number, any transaction ID that is less
than this value is considered completed.")

(defvar ah-cleartool-ntid 1
  "The next transaction id.
Whenever `ah-cleartool-ask' enqueues a transaction, it increments
this value.")

(defvar ah-cleartool-terr nil
  "Assoc list of (tid . error-message).
Transactions that have errors will have their tid's and error
messages stored in this list.  `ah-cleartool-wait-for' will check
this list and signal an error with the error message." )

(defvar ah-cleartool-tresults nil
  "Assoc list of (tid . answer).
Transactions that don't have a callback function attached, will
have their answer stored here for retrieval by
'ah-cleartool-wait-for'.")

;; Create an error that will be signaled when Cleartool reports an
;; error.  We need it so we can filter errors that come from cleartool
;; itself (which we might want to ignore) and other errors.
(put 'ah-cleartool-error 'error-conditions '(error ah-cleartool-error))
(put 'ah-cleartool-error 'error-message "cleartool")

(defun ah-cleartool-signal-error (message)
  "Parse MESSAGE and signal a cleartool-error.
MESSAGE is searched for an error from cleartool and that error is
signaled as an `ah-cleartool-error' error.  If MESSAGE does not
contain a cleartool error, the entire MESSAGE is signaled.  If
multiple cleartool errors are found, the first one is signaled,
but the string \"(multiple)\" is prepended to it."
  (let* ((tag "cleartool: Error: \\(.*\\)")
         (pos (string-match tag message))
         (str (if pos (match-string 1 message) message)))
    (when (and pos (string-match tag message (1+ pos)))
      (setq str (concat "(multiple) " str)))
    (while t (signal 'ah-cleartool-error (list str)))))

(defun ah-cleartool-tq-sentinel (process event)
  "Sentinel for the cleartool PROCESS.
Cleans up properly if cleartool exits.  EVENT is not used."
  (let ((status  (process-status process))
        (exit-status (process-exit-status process))
        (pbuffer (process-buffer process)))
    (when (memq status '(signal exit))
      (when (null (buffer-name pbuffer))
        (message "Cleartool process buffer was killed"))
      (ah-cleartool-tq-stop)
      (kill-buffer pbuffer))))

(defun ah-cleartool-tq-start ()
  "Start the transaction queue to cleartool."
  (let ((process
         (start-process "cleartool" " *cleartool*"
                        ah-clearcase-cleartool-program "-status")))
    (when (not (eq system-type 'windows-nt))
      ;; on systems other than windows-nt, cleartool will print a
      ;; prompt when it starts up and tq will complain about it.  In
      ;; these cases, we wait until the prompt is printed, and start
      ;; the tq afterward.
      (with-timeout (5 (error "Timeout waiting for cleartool to start"))
        (with-current-buffer (get-buffer " *cleartool*")
          (goto-char (point-min))
          (while (not (looking-at "cleartool 1> $"))
            (accept-process-output process 2)
            (goto-char (point-min)))
          (erase-buffer))))
    (set-process-sentinel process 'ah-cleartool-tq-sentinel)
    (process-kill-without-query process nil)
    (setq ah-cleartool-tq (tq-create process)
          ah-cleartool-next-command 1
          ah-cleartool-last-command-timestamp (float-time))))


(defcustom ah-cleartool-save-stop-data nil
  "When t, print a report each time cleartool is stopped.
The report is appended to the *cleartool-aborts* buffer."
  :type 'boolean
  :group 'vc-clearcase)

(defun ah-cleartool-tq-stop ()
  "Stop the transaction queue to cleartool and kill cleartool."
  (when ah-cleartool-tq
    (unwind-protect
         (when ah-cleartool-save-stop-data
           (with-current-buffer (get-buffer-create "*cleartool-aborts*")
             (insert "\n\f\n" (current-time-string) "\n"
                     (format "ah-cleartool-ctid: %d\n" ah-cleartool-ctid)
                     (format "ah-cleartool-ntid: %d\n" ah-cleartool-ntid)
                     (format "ah-cleartool-next-command: %d\n"
                             ah-cleartool-next-command))
             (insert "ah-cleartool-terr:\n")
             (dolist (x ah-cleartool-terr)
               (insert (format "    %d    %s\n" (car x) (cdr x))))
             (insert "\nah-cleartool-tresults:\n")
             (dolist (x ah-cleartool-tresults)
               (insert (format "    %d    %s\n" (car x) (cdr x))))
             (insert "\ntq-buffer:")
             (let ((b (tq-buffer ah-cleartool-tq)))
               (if (and b (buffer-name b))
                   (insert
                    (with-current-buffer (tq-buffer ah-cleartool-tq)
                      (buffer-substring-no-properties (point-min) (point-max))))
                   (insert "*** tq-buffer was killed ***")))))

      ;; Bug 1564792: make sure we run this part even if the code
      ;; above fails.

      (tq-close ah-cleartool-tq)
      (setq ah-cleartool-next-command 0)
      (setq ah-cleartool-tq nil)

      ;; mark all pending transactions as aborted
      (while (< ah-cleartool-ctid (1- ah-cleartool-ntid))
        (incf ah-cleartool-ctid)
        (push (cons ah-cleartool-ctid "cleartool command was aborted")
              ah-cleartool-terr)))))

(defsubst ah-cleartool-tq-maybe-start ()
  "Start the transaction queue to cleartool, if not already started."
  (unless (and ah-cleartool-tq
               ;; Bug 1564792: check if someone killed the tq buffer.
               (let ((b (tq-buffer ah-cleartool-tq)))
                 (if (and b (buffer-name b))
                     t
                     (message "cleartool tq-buffer was killed")
                     nil)))
    (setq ah-cleartool-tq nil)
    (ah-cleartool-tq-start))
  ah-cleartool-tq)

(defun ah-cleartool-tq-handler (closure answer)
  "Handle responses from cleartool-tq.

CLOSURE the closure that was enqueued with `ah-cleartool-ask', it
is a vector containing the transaction id plus the closure and
function that were passed to `ah-cleartool-ask' (the last two
might be null).

ANSWER is the string that was received from cleartool.

The function checks the command index and status received from
cleartool, updates the completed transaction
id ('ah-cleartool-ctid') and either stores the answer in
`ah-cleartool-terr' or `ah-cleartool-tresults' for later
retrieval by `ah-cleartool-wait-for', or calls the function
callback with the answer."

  ;; NOTE: emacs will save the match data, so we can do regexps
  ;; without the need of a save-match-data form.
  (let ((tid (aref closure 0))
        (cb-closure (aref closure 1))
        (cb (aref closure 2)))
    (cond ((string-match ah-cleartool-status-rx answer)
           (let ((cmd (string-to-number (match-string 1 answer)))
                 (status (string-to-number (match-string 2 answer))))
             (unless (= ah-cleartool-next-command cmd)
               ;; transaction queue is out of sync, stop it
               (ah-cleartool-tq-stop)
               (error "Unexpected command index received"))
             ;; it's the command we're expecting
             (incf ah-cleartool-next-command)
             (let ((result (replace-match "" t t answer)))
               (setq ah-cleartool-ctid tid) ; assume tid's always grow
               (cond ((> status 0)
                      (push (cons tid result) ah-cleartool-terr))
                     (cb             ; do we have a callback function?
                      (funcall cb cb-closure result))
                     (t
                      (push (cons tid result) ah-cleartool-tresults))))))
          ((string-match ah-cleartool-question-rx answer)
           (push (cons tid answer) ah-cleartool-terr))
          (t
           (error
            "Answer does not have a status in ah-cleartool-tq-handler")))))

(defun ah-cleartool-wait-for (tid &optional timeout)
  "Wait for TID to complete, return the result or signal an error.

Wait in TIMEOUT seconds intervals, or, if TIMEOUT is nil, wait
`ah-cleartool-timeout' seconds.  If during this time, cleartool
has written something to the output, we wait another interval.
That is, if a transaction takes a very long time to complete, but
cleartool appears to be working, we don't stop it.

If transaction-id has completed, search `ah-cleartool-terr' for
an error message associated with that transaction, and if found,
signals an error.  Otherwise look in `ah-cleartool-tresults' for
a result for the transaction and returns that.  Else return t.

NOTE: a successful transaction might not have a result
associated, as `ah-cleartool-tq-handler' passes the result to the
callback function if that is available."

  (assert tid nil "nil `tid' passed to ah-cleartool-wait-for")

  ;; we use an external loop so that if the with-timeout form exits
  ;; but the process has sent some data we can start the wait again.
  ;; We don't want to abort a cleartool command that is sending us
  ;; lots of data and takes longer than our timeout.
  (while (< ah-cleartool-ctid tid)
    (let (received-some-data
          (cleartool-process (tq-process ah-cleartool-tq)))
      (with-timeout ((or timeout ah-cleartool-timeout))
        (while (< ah-cleartool-ctid tid)
          (setq received-some-data
                (or received-some-data
                    ;; will return t if some data was received
                    (accept-process-output cleartool-process 2)))

          ;; Hmm, sometimes the input from cleartool is not processed
          ;; in accept-process-output and we need to call sit-for with
          ;; a non zero argument.  This occurs when we 'uncheckout' a
          ;; revision.  It will loop forever (outer while) since
          ;; `ah-cleartool-tq-handler' is not called to increment
          ;; `ah-cleartool-ctid'.  There's some race condition here,
          ;; but I'm not sure what it is.  Do not remove the sit-for
          ;; call without understanding what it does.  If you remove
          ;; it, it will work in 99% of the cases and fail
          ;; mysteriously in 1%.
          (when (< ah-cleartool-ctid tid)
            (sit-for 0.1))))

      (when (and (not received-some-data)
                 (< ah-cleartool-ctid tid))
        ;; so our transaction is not yet complete and cleartool
        ;; hasn't written anything for us.  Assume that cleartool
        ;; is hung and kill it.
        (ah-cleartool-tq-stop)
        (ah-cleartool-tq-start)
        (error "Cleartool timed out"))))

  ;; if we are here, the transaction is complete.

  ;; see if we have an error and signal it
  (let ((err (assq tid ah-cleartool-terr)))
    (when err
      (setq ah-cleartool-terr (assq-delete-all tid ah-cleartool-terr))
      (ah-cleartool-signal-error (cdr err))))

  ;; else search for a result
  (let ((result (assq tid ah-cleartool-tresults)))
    (if result
        ;; if we have a result, delete it from the list and return it
        (progn
          (setq ah-cleartool-tresults
                (assq-delete-all tid ah-cleartool-tresults))
          (cdr result))
        ;; else return t, meaning that the transaction is complete but
        ;; it returned no data.
        t)))

(defun ah-cleartool-ask (question &optional wait closure fn)
  "Enqueue QUESTION to the cleartool-tq.

If WAIT is different than 'nowait, the transaction is waited for
with `ah-cleartool-wait-for' and returns whatever
`ah-cleartool-wait-for' returns.  Otherwise the the transaction
id is returned (you will have to wait for it yourself).  If
CLOSURE and FN are specified, fn will be called when the
transaction is complete as funcall(fn closure answer)."
  (when (> (- (float-time) ah-cleartool-last-command-timestamp)
           ah-cleartool-idle-timeout)
    (message "Cleartool is idle for too long, restarting...")
    (let ((ah-cleartool-save-stop-data nil))
      ;; stop cleartool without dumping state to the
      ;; *cleartool-aborts* buffer.
      (ah-cleartool-tq-stop)))
  (setq ah-cleartool-last-command-timestamp (float-time))
  (ah-cleartool-tq-maybe-start)
  (let ((tid ah-cleartool-ntid)
        (command (concat question "\n")))
    (incf ah-cleartool-ntid)
    (tq-enqueue ah-cleartool-tq command ah-cleartool-tq-rx
                (vector tid closure fn) 'ah-cleartool-tq-handler)
    (if (eq wait 'nowait)
        tid
        (ah-cleartool-wait-for tid))))

(defsubst ah-cleartool (string &rest objects)
  "Shorthand for (ah-clartool-ask (format ... ))."
  (ah-cleartool-ask (apply 'format string objects)))

;;;; Cleartool subprocess interface

;; For cleartool commands that take longer to complete that it takes
;; cleartool to start, we use a subprocess interface.  This will start
;; a cleartool process and put the command output in a buffer.
;;
;; The 'user' function in this section is ah-cleartool-do.

(defvar ah-cleartool-mode-line nil
  "Modeline argument for cleartool commands.")

(defvar ah-cleartool-finished-function nil
  "Function to be called when the cleartool process finishes.")

(defvar ah-cleartool-kill-buffer-when-done nil
  "When t, kill process buffer when cleartool exits.")

(defvar ah-cleartool-last-command nil
  "The last command given to cleartool to create this output buffer.")

(progn
  ;; make the above variables buffer local and attach permanent-local
  ;; to them
  (mapcar (lambda (x)
            (make-variable-buffer-local x)
            (put x 'permanent-local t))
          '(ah-cleartool-mode-line
            ah-cleartool-finished-function
            ah-cleartool-kill-buffer-when-done
            ah-cleartool-last-command))
  )

(defun ah-cleartool-sentinel (process event)
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
            (setq ah-cleartool-mode-line
                  (format "%s [%d]" (symbol-name status) exit-status))
            (force-mode-line-update)
            (when ah-cleartool-finished-function
              (funcall ah-cleartool-finished-function))
            (delete-process process)
            (when ah-cleartool-kill-buffer-when-done
              (kill-buffer nil)))))))


(defun ah-cleartool-do (cmd args buffer)
  "Run the cleartool CMD with ARGS and put the result in BUFFER.

Command is a cleartool command, that is the actual command run is
\"cleartool cmd args\".

The arguments need to be a list of strings as in execv(2) call.
This is different from the arguments to `ah-cleartool-ask'.

This command only starts the process and returns it.  The process
will continue to run and fill buffer.  If you want to be notified
when the process is finished, setup a callback function in
`ah-cleartool-finished-function' (see below.)  See also
`ah-clearcase-version' for a command that waits for the result of
the cleartool command.

The sentinel for the resulting process inspects the following
buffer local variables in the process buffer:

`ah-cleartool-finished-function' -- function to call when the
cleartool command has finished.

`ah-cleartool-kill-buffer-when-done' -- when t, the buffer will
be killed when the cleartool command has finished.

In addition, the buffer local variable
`ah-cleartool-last-command' is set to the command and arguments
that were run to create this buffer."
  (let ((name (format "cleartool-%s" cmd))
        (args1 (cons cmd args)))
    (let ((process (apply 'start-process name buffer
                          ah-clearcase-cleartool-program args1)))
      (with-current-buffer (process-buffer process)
        (set-process-sentinel process 'ah-cleartool-sentinel)
        (setq ah-cleartool-mode-line "run")
        (setq mode-line-process '(" cleartool: " ah-cleartool-mode-line))
        (force-mode-line-update)
        (setq ah-cleartool-last-command (copy-sequence args1))
        process))))


;;;; Clearcase Log View mode

;; The existing logview mode in Emacs works only for RCS style logs.
;; We define our own mode for the ClearCase logs (which are called
;; lshistory in ClearCase).

(defvar ah-clearcase-lshistory-fmt
  (concat "----------\n"
          "version: %Vn\n"
          "user: %u; what: %e; date: %Sd\n"
          "labels: %l\n\n"
          "%c")
  "Format string to use when listing file history.")

(defconst ah-clearcase-log-view-font-lock-keywords
  '(("----------" . font-lock-comment-face)
    ("[-A-Za-z0-9]+:" . font-lock-keyword-face)
    ("(reserved)" . font-lock-variable-name-face)
    ("<No-tag-in-region>" . font-lock-warning-face)))

(defconst ah-clearcase-record-separator-rx "^----------"
  "Regexp for a record separator.")

(defun ah-clearcase-log-view-bor ()
  "Move to the beginning of the current log record."
  (interactive)
  (unless (eolp)
    (end-of-line))
  (if (re-search-backward ah-clearcase-record-separator-rx (point-min) 'noerror)
      (progn
        (goto-char (match-beginning 0))
        (forward-line 1)
        (beginning-of-line))
      (goto-char (point-min))))

(defun ah-clearcase-log-view-eor ()
  "Move to the end of the current log record."
  (interactive)
  (unless (eolp)
    (end-of-line))
  (if (re-search-forward ah-clearcase-record-separator-rx (point-max) 'noerror)
      (progn
        (goto-char (match-beginning 0))
        (forward-char -1))
      (goto-char (point-max))))

(defun ah-clearcase-log-view-record-field (field-name)
  "Return the value of FIELD-NAME in the current record.

This method assumes that the record fields look like:
'field-name: value' and returns 'value'.  If the field is not
found, nil is returned."
  (save-match-data
    (save-excursion
      (let ((field-re (format "\\<%s:\\s-+" field-name))
            (limit (progn (ah-clearcase-log-view-eor) (point)))
            start end)
        (ah-clearcase-log-view-bor)
        (when (re-search-forward field-re limit 'noerror)
          (progn
            (setq start (match-end 0))
            (setq end
                  (if (looking-at "(")
                      ;; If the field value is enclosed in
                      ;; parenthesis, the value is the SEXP
                      (progn
                        (setq start (1+ start))
                        (forward-sexp)
                        (forward-char -1)
                        (point))
                      ;; else, a field value ends at the end of line or
                      ;; at the ';' char
                      (when (re-search-forward ";\\|$" limit 'noerror)
                        (match-beginning 0))))
            (buffer-substring-no-properties start end)))))))

(defun ah-clearcase-log-view-again ()
  "Re-run the cleartool command that generated this log."
  (interactive)
  (when (and (boundp 'ah-cleartool-last-command) ah-cleartool-last-command)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (ah-cleartool-do (car ah-cleartool-last-command)
                     (cdr ah-cleartool-last-command)
                     (current-buffer))))

(defun ah-clearcase-log-view-forward-record (num-records)
  "Move forward NUM-RECORDS, if negative, move backward.
Return the number of records actually moved."
  (interactive "p")
  (multiple-value-bind (search-fn limit adjust)
      (if (>= num-records 0)
          (list 're-search-forward (point-max) 1)
          (list 're-search-backward (point-min) -1))
    (if (funcall search-fn ah-clearcase-record-separator-rx limit 'noerror
                 (abs num-records))
        (progn
          (forward-line adjust)
          (ah-clearcase-log-view-bor)
          t)
        nil)))

(defun ah-clearcase-log-view-backward-record (num-records)
  "Move backwards NUM-RECORDS, if positive, move forward."
  (interactive "p")
  (ah-clearcase-log-view-forward-record (- num-records)))

(defun ah-clearcase-log-view-forward-version (num)
  "Visit the log record next NUM versions from the current one.
Will visit previous records if NUM is negative).

NOTE: you can only move forward if the current version is on the
version path of the current file."
  (interactive "p")
  (let ((version (ah-clearcase-log-view-record-field "version"))
        (move-version-fn (if (>= num 0) 'vc-clearcase-next-version
                             'vc-clearcase-previous-version))
        (file (with-current-buffer vc-parent-buffer
                (buffer-file-name))))

    (if (or (null version) (string= version ""))
        (message "No version record in the current field")
        (catch 'exit
          (dotimes (i (abs num))
            (when (null version)
              (throw 'exit nil))
            (setq version (funcall move-version-fn file version))))
        (if (null version)
            (message "End of version chain")
            (vc-clearcase-show-log-entry version)))))

(defun ah-clearcase-log-view-backward-version (num)
  "Visit the log record previous NUM versions from the current record.
Will move to the next record if NUM is negative."
  (interactive "p")
  (ah-clearcase-log-view-forward-version (- num)))

(defun ah-clearcase-log-view-visit-file ()
  "Visit the file specified by this log record."
  (interactive)
  (let ((file-name (ah-clearcase-log-view-record-field "file")))
    (if file-name
        (if (file-exists-p file-name)
            (switch-to-buffer (find-file-noselect file-name))
            (message "File %s does not exist" file-name))
        (message "No file found in record"))))


(defun ah-clearcase-log-view-wash-record ()
  "Fill the comment part of a log record."
  (let ((inhibit-read-only t)
        (start (progn (ah-clearcase-log-view-bor) (point)))
        (end (progn (ah-clearcase-log-view-eor) (point))))
    (goto-char start)
    (when (re-search-forward "^$" end)
      (let ((cstart (match-end 0)))
        (goto-char cstart)
        (fill-region (point) end)))))

(defun ah-clearcase-log-view-wash-log ()
  "Wash the whole log file."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (ah-clearcase-log-view-wash-record)
    (while (ah-clearcase-log-view-forward-record 1)
      (ah-clearcase-log-view-wash-record))))

(define-derived-mode ah-clearcase-log-view-mode fundamental-mode
  "Cc-Log-View"
  "Generic mode to view clearcase log listings."
  ;; this gets reset when we switch modes
  (make-local-variable 'font-lock-defaults)
  (setq case-fold-search nil)
  (setq font-lock-defaults '(ah-clearcase-log-view-font-lock-keywords nil nil))
  (font-lock-mode t)
  (setq buffer-read-only t))

(easy-mmode-defmap ah-clearcase-log-view-mode-map
                   '(("n" . ah-clearcase-log-view-forward-record)
                     ("p" . ah-clearcase-log-view-backward-record)
                     ("N" . ah-clearcase-log-view-forward-version)
                     ("P" . ah-clearcase-log-view-backward-version)
                     ("\M-a" . ah-clearcase-log-view-bor)
                     ("\M-e" . ah-clearcase-log-view-eor)
                     ("g" . ah-clearcase-log-view-again)
                     ("f" . ah-clearcase-log-view-visit-file)
                     ("w" . ah-clearcase-log-view-wash-log))
                   "Mode map for Clearcase Log View mode")


;;;; Clearcase file properties

;; Rather than keeping all the version information as properties
;; accessible via vc-file-{get/set}prop, we define a structure to hold
;; all the information and store it in as the 'vc-clearcase-prop
;; property of a file.  We also define some pseudo structure members
;; in the form of functions starting with ah-clearcase-fprop- -- they
;; return derived information from a fprop.

(defstruct (ah-clearcase-fprop
             (:constructor ah-clearcase-make-fprop)
             (:copier ah-clearcase-copy-fprop))

  file-name                      ; the file name this fprop belongs to

  version-tid
  version                               ; current file revision
  parent                                ; parent revision
  status        ; nil, 'reserved, 'unreserved, 'hijacked, 'broken-view
  what-rule                             ; confispec rule for the file

  comment-tid
  comment                    ; the checkout comment (when checked out)

  view-tag                              ; the view for the file

  ;; a list of all the revisions of this file, starting from \main\0
  ;; all the way to latest.  Used by vc-clearcase-next-version to
  ;; speed up the search.
  revision-list
  )

(defsubst ah-clearcase-file-fprop (file)
  "Return the fprop structure associated with FILE."
  (vc-file-getprop (expand-file-name file) 'vc-clearcase-fprop))

(defsubst ah-clearcase-fprop-initialized-p (fprop)
  "Return true if FPROP is initialized.
FPROP can be nil, meaning it is not initialized."
  ;; we use an if form to return t or nil instead of the version
  ;; string or tid.
  (if (and fprop
           (or (ah-clearcase-fprop-version fprop)
               (ah-clearcase-fprop-version-tid fprop)))
      t
      nil))

(defsubst ah-clearcase-fprop-hijacked-p (fprop)
  "Return true if FPROP is hijacked."
  (eq (ah-clearcase-fprop-status fprop) 'hijacked))

(defsubst ah-clearcase-fprop-checkedout-p (fprop)
  "Return the checked out mode for FPROP or nil."
  (memq (ah-clearcase-fprop-status fprop) '(reserved unreserved)))

(defsubst ah-clearcase-fprop-broken-view-p (fprop)
  "Return true if the there's a problem with this FPROP in the view.
This can happen in snapshot views, occasionally cleartool reports
that another process does an update and refuses to operate on the
files.  The solution to the problem is to run an update on the
whole view, but it is beyond the scope of this FPROP."
  (eq (ah-clearcase-fprop-status fprop) 'broken-view))

(defsubst ah-clearcase-fprop-checkout-will-branch-p (fprop)
  "Return true if a checkout will create a branch on this FPROP.
The branch creation might still fail if the branch already exists
somewhere in the version-tree of this element.  So what we really
check is whether ClearCase will try to branch this file at
checkout."
  (string-match "-mkbranch" (ah-clearcase-fprop-what-rule fprop)))

(defun ah-clearcase-fprop-branch (fprop)
  "Return the branch part of FPROP.
This is the second last element in version path."
  (let ((version (ah-clearcase-fprop-version fprop)))
    (nth 1 (nreverse (split-string version "[\\\\/]")))))

(defun ah-clearcase-fprop-version-base (fprop)
  "Return the version of FPROP minus the last element."
  (let ((version (copy-sequence (ah-clearcase-fprop-version fprop))))
    (when (string-match "[\\\\/][^\\\\/]*$" version)
      (replace-match "" t t version))))

(defun ah-clearcase-fprop-version-number (fprop)
  "Return the version number of FPROP (last element in version path).
If the file is checked out, the version is '.../CHECKEDOUT', in
that case, we return the version of the parent."
  (let ((version (ah-clearcase-fprop-version fprop)))
    (when (string-match "[\\\\/]\\([^\\\\/]*\\)$" version)
      (match-string 1 version))))

(defsubst ah-clearcase-fprop-latest-sel (fprop)
  "Return a version selector for the latest version of FPROP."
  (format "%s/LATEST" (ah-clearcase-fprop-version-base fprop)))

(defun ah-clearcase-fprop-latest (fprop)
  "Return the latest version of FPROP on the current branch."
  (ah-cleartool "desc -fmt \"%%Vn\" \"%s@@%s\""
                (ah-clearcase-fprop-file-name fprop)
                (ah-clearcase-fprop-latest-sel fprop)))

(defun ah-clearcase-wash-mode-line (mode-line)
  "Make the MODE-LINE string shorter.
We do this by replacing some of the words in it with shorter
versions.  This is probably specific to my site, so it should be
made configurable..."
  (replace-regexp-in-string
   "\\<release\\([0-9]*\\)\\>" "rel\\1"
   (replace-regexp-in-string
    "\\<branch\\([0-9]*\\)\\>" "br\\1"
    (replace-regexp-in-string
     "\\<patch\\([0-9]*\\)\\>" "pat\\1"
     (replace-regexp-in-string
      "iteration\\([0-9]+\\)" "I~\\1" mode-line)))))

(defun ah-clearcase-reset-fprop (fprop)
  "Clear the version fields in FPROP.
This will mark fprop as not initialized for the functions that
care about this.  This function accepts a nil fprop (in which
case it does nothing), to the user can reset a file's fprop
without having to check first that it exists."
  (when fprop
    (setf (ah-clearcase-fprop-version fprop) nil)
    (setf (ah-clearcase-fprop-version-tid fprop) nil)
    (setf (ah-clearcase-fprop-revision-list fprop) nil)))

(defun ah-clearcase-set-fprop-version-stage-1 (fprop ls-string)
  "Set version information in FPROP from LS-STRING.
Ls-string is returned by a 'cleartool ls file' command.  From it,
we detemine the configspec rule, the intial version of the file
and whether the file is hijacked or in a broken view.

Note that if the file is checked out, the revision will end in
/CHECKEDOUT, which is not a valid revision for vc.el
semantics (for example it cannot be used for diff'ing purposes).
In that case, the version will be adjusted in
`ah-clearcase-set-fprop-version-stage-2'"
  (when (string-match "Rule: \\(.*\\)$" ls-string)
    (setf (ah-clearcase-fprop-what-rule fprop) (match-string 1 ls-string)))
  (when (string-match "@@\\([^ \t]+\\)" ls-string)
    (let ((fver (match-string 1 ls-string)))
      (setf (ah-clearcase-fprop-version fprop) fver)))
  ;; The ls string will also tell us when something is wrong with the
  ;; file.
  (setf (ah-clearcase-fprop-status fprop)
        (cond ((string-match "hijacked" ls-string) 'hijacked)
              ((string-match "rule info unavailable" ls-string) 'broken-view)
              (t nil))))

(defun ah-clearcase-set-fprop-version-stage-2 (fprop version-string)
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
        (setf (ah-clearcase-fprop-version fprop) pver))
      (setf (ah-clearcase-fprop-parent fprop) pver)
      ;; a hijacked file should keep its existing checkout status and
      ;; modeline (set by ah-clearcase-set-fprop-version-stage-1)
      (unless (memq (ah-clearcase-fprop-status fprop) '(hijacked broken-view))
        (setf (ah-clearcase-fprop-status fprop) co-mode)))))




;;;; Clearcase view-tag properties

(defstruct (ah-clearcase-vprop
             (:constructor ah-clearcase-make-vprop)
             (:copier ah-clearcase-copy-vprop))
  name
  root                                  ; for snapshot views only
  type                                  ; (nil 'snapshot 'dynamic)
  )

(defvar ah-clearcase-all-vprops '())

(defsubst ah-clearcase-snapshot-view-p (view)
  "Return t if VIEW is a snapshot view.
VIEW can be either a view name (a string) a vprop or a fprop"
  (let ((vprop (ah-clearcase-get-vprop view)))
    (eq (ah-clearcase-vprop-type vprop) 'snapshot)))

(defsubst ah-clearcase-dynamic-view-p (view)
  "Return t if VIEW is a dynamic view.
VIEW can be either a view name (a string) a vprop or a fprop"
  (let ((vprop (ah-clearcase-get-vprop view)))
    (eq (ah-clearcase-vprop-type vprop) 'dynamic)))

(defun ah-clearcase-prepare-vprop (file fprop)
  "Find the view in which FILE resides and populate it.
FPROP is used to get the view name.  If the view is not known,
create a new vprop for it."
  ;; first, we switch the current directory in cleartool, as it is the
  ;; only way to get the current view and its root directory
  (ah-cleartool "cd \"%s\"" (file-name-directory file))
  (setf (ah-clearcase-fprop-view-tag fprop)
        (replace-regexp-in-string "[\n\r]+" "" (ah-cleartool "pwv -short")))
  (let ((vprop (ah-clearcase-get-vprop fprop)))
    (unless (ah-clearcase-vprop-type vprop)
      ;; This is the first time we see this view, collect some info on it
      (let ((lsview (ah-cleartool "lsview %s" (ah-clearcase-vprop-name vprop))))
        (setf (ah-clearcase-vprop-type vprop)
              ;; the first char in a lsview listing is '*'.  At least at
              ;; our site...  I think the '*' means the view is started,
              ;; but that's the same thing for us... (since we are
              ;; visiting a file in this view, it must be started)
              (if (char-equal ?* (aref lsview 0)) 'dynamic 'snapshot)))
      (when (ah-clearcase-snapshot-view-p vprop)
        (ah-cleartool-ask
         "pwv -root" 'nowait vprop
         '(lambda (vprop root-dir)
           (let ((root (replace-regexp-in-string "[\n\r]+" "" root-dir)))
             (setf (ah-clearcase-vprop-root vprop) root))))))))

(defun ah-clearcase-get-vprop (view-tag)
  "Return the vprop struct associated with VIEW-TAG.

VIEW-TAG can be:

1/ a vprop, in which case it is returned,

2/ a string in which case a vprop with that name is looked up and
returned (if no such vprop exists, it is created first)

3/ a fprop, in which case its view-tag is searched using 2/."

  (if (ah-clearcase-vprop-p view-tag)
      view-tag                          ; case 1/
      (let* ((vtag (cond ((stringp view-tag) view-tag)
                         ((ah-clearcase-fprop-p view-tag)
                          (ah-clearcase-fprop-view-tag view-tag))
                         (t (error "Unknown type for VIEW-TAG"))))
             ;; find or create a new vprop
             (vprop (car (or (member* vtag ah-clearcase-all-vprops
                                      :key 'ah-clearcase-vprop-name
                                      :test 'equal)
                             (push (ah-clearcase-make-vprop :name vtag)
                                   ah-clearcase-all-vprops)))))

        ;; find out if this is a started dynamic view.  This allows to
        ;; set configspec in dynamic views without having to declare
        ;; them first.  NOTE: maybe we could start it if it is not?
        (let ((vinfo (ignore-errors (ah-cleartool "lsview %s" vtag))))
          (when (and vinfo (char-equal ?* (aref vinfo 0)))
            ;; we have a started dynamic view
            (setf (ah-clearcase-vprop-type vprop) 'dynamic)))
        vprop)))

(defun ah-clearcase-declare-view (view-tag type &optional root)
  "Declare a VIEW-TAG as a view of TYPE 'snapshot or 'dynamic.
For a snapshot view, the view's ROOT directory also has to be
specified.  This function can be used to pre-declare views so
that ah-clearcase-setcs will not complain that you have to visit
a file in that view first."
  (unless (memq type '(snapshot dynamic))
    (error "Bad view type"))
  (when (eq type 'snapshot)
    (unless (and (stringp root) (file-directory-p root))
      (error "Bad view root")))
  (let ((vprop (ah-clearcase-get-vprop view-tag)))
    (setf (ah-clearcase-vprop-type vprop) type)
    (setf (ah-clearcase-vprop-root vprop) root)))

(defun ah-clearcase-refresh-files-in-view (view-tag)
  "Update all visited files from VIEW-TAG.
This is useful when the view changes (by a setcs or update
command).  VIEW can be either a view-tag name or a vprop."
  (when (ah-clearcase-vprop-p view-tag)
    (setq view-tag (ah-clearcase-vprop-name view-tag)))
  (dolist (buffer (buffer-list))
    (ignore-errors
      ;; ignore modified buffers, don't rob the user from the joy of
      ;; figuring out that he just changed the view and he had
      ;; modified files in it...
      (unless (buffer-modified-p buffer)
        (let* ((file (buffer-file-name buffer))
               (fprop (ah-clearcase-file-fprop file))
               (vtag (ah-clearcase-fprop-view-tag fprop)))
          (when (string= vtag view-tag)
            (ah-clearcase-maybe-set-vc-state file 'force)
            (vc-resynch-buffer file t t)))))))

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

(defvar ah-clearcase-edcs-all-view-tags nil
  "An obarray of all known view-tags (stored as symbols).")

(defvar  ah-clearcase-edcs-all-view-tags-tid nil
  "Transaction ID to wait for fetching all view-tags.")

(defun ah-clearcase-complete-view-tag (string predicate flag)
  "Completion function for reading a view-tag name.
See `completing-read' for the meanings of STRING, PREDICATE and
FLAG."
  ;; Note that we cannot pass ah-clearcase-edcs-all-view-tags to
  ;; completing-read because its value will be set asynchronously when
  ;; the cleartool ask command finishes.  Thus, we simply check the
  ;; flag and call the proper function (which completing-read would
  ;; call if we would pass ah-clearcase-edcs-all-view-tags directly to
  ;; it.
  (let ((completion-fn (cond ((eq flag t) 'all-completions)
                             ((eq flag 'lambda)
                              ;; test-completion does not exist in emacs 21.
                              '(lambda (x l &optional p) (intern-soft x l)))
                             ((null flag) 'try-completion)
                             (t (error "unknwn value for flag %S" flag)))))
    (funcall completion-fn string ah-clearcase-edcs-all-view-tags predicate)))


(defun ah-clearcase-read-view-tag (prompt &optional initial)
  "Read a view tag from the minibuffer and return it.
PROMPT is displayed to the user; INITIAL, when non-nil is the
initial view tag name presented to the user.

This function will provide a completing-read with the list of
available view tags in the system.  It does however read the view
tags asynchronously so they might not be available immediately as
the user hits the TAB key.

This implementation was chosen to improve responsiveness, if the
user wants to accept INITIAL or wants to type in the name of the
view, he can do so without waiting for the full list of view tags
to be read from cleartool."

  ;; start reading the view-tags. By the time the user decides what
  ;; view-tag it wants, we may have the answer already.  Note that the
  ;; previous view tag list still exists and the user can perform
  ;; completions form that one.
  (setq ah-clearcase-edcs-all-view-tags-tid
        (ah-cleartool-ask
         "lsview -short" 'nowait nil
         '(lambda (x view-tags)
           (setq ah-clearcase-edcs-all-view-tags (make-vector 31 0))
           (dolist (vtag (split-string view-tags "[\n\r]+"))
             (intern vtag ah-clearcase-edcs-all-view-tags)))))

  (completing-read prompt 'ah-clearcase-complete-view-tag nil nil initial))

;;;; Read a label form the minibuffer with completion.

;;; Since the number of labels in a VOB can be quite large (17076 of
;;; them according to 'cleartool lstype -kind lbtype | wc -l') We use
;;; the same ideea as for reading the view-tags, with the following
;;; exceptions:
;;;
;;; 1/ We cannot use the cleartool lstype command as it is too slow.
;;; We must use a cleartool dump command and parse its output.
;;;
;;; 2/ Since the output of the dump command is large, we don't use the
;;; transaction queue to cleartool or the subprocess interface,
;;; instead we start the cleartool process ourselves and filter its
;;; output.
;;;
;;; The list of labels is reset at each read, and populated on the
;;; fly.  During the first few seconds of the read, not all the labels
;;; will be available.

(defun ah-clearcase-vob-tag-for-path (path)
  "Return the vob tag in which PATH resides.
This can be used to obtain the vob-tag required for the
`ah-clearcase-read-label' function."
  (setq path (expand-file-name path))
  (unless (file-exists-p path)
    (error "%s does not exist" path))
  (let ((dir (if (file-directory-p path)
                 path
                 (file-name-directory path))))
    (ah-cleartool "cd \"%s\"" dir)
    (let ((view-root (ah-cleartool "pwv -root")))
      (let ((path-elements (split-string path "[\\\\\\/]"))
            (prefix-length (length (split-string view-root "[\\\\\\/]"))))
        ;; On Windows, the vob tag looks like "/Vob_Name", on Solaris,
        ;; it is "/vobs/Vob_Name".
        (concat "/" (nth prefix-length path-elements)
                (unless (eq system-type 'windows-nt)
                  (concat "/" (nth (1+ prefix-length) path-elements))))))))

(defvar ah-clearcase-all-labels nil
  "An obarray containing all labels (stored as symbols)")

(defvar ah-clearcase-collect-labels-point nil
  "The position in the buffer where we left the processing")

(defvar ah-clearcase-collect-labels-finished nil
  "Becomes t when we finished processing the cleartool dump output.
Used by `ah-clearcase-collect-labels-sentinel' and
`ah-clearcase-collect-labels-filter' to synchrinize themselves.")

(defun ah-clearcase-collect-labels-sentinel (process event)
  "Sentinel for the cleartool dump command.
Cleans up after cleartool exits."
  (when (memq (process-status process) '(signal exit))
    (let ((buffer (process-buffer process)))
      (unless (= (process-exit-status process) 0)
        (message "non-zero exit code form cleartool while reading labels"))
      (unless (null (buffer-name buffer))
        (with-current-buffer buffer
          (when ah-clearcase-collect-labels-finished
            ;; if we finished collecting labels from the cleartool
            ;; output, kill the process buffer.
            (kill-buffer buffer)))))))

(defun ah-clearcase-collect-labels-filter (process string)
  "The filter function for the cleartool dump command.
Parses the output and populates ah-clearcase-all-labels with the
labels it finds."
  (with-current-buffer (process-buffer process)
    ;; We should be the only ones controlling the buffer, we don't
    ;; save-excursion.

    (goto-char (point-max))
    (insert string)

    ;; if we haven't found the start of the label section yet, look
    ;; for it.
    (when (null ah-clearcase-collect-labels-point)
      (goto-char (point-min))
      (when (re-search-forward "^label type objects:" nil 'noerror)
        (forward-line 1)
        (setq ah-clearcase-collect-labels-point (point))))

    (unless (null ah-clearcase-collect-labels-point)
      (goto-char ah-clearcase-collect-labels-point)
      (let ((limit
             (re-search-forward "^[a-zA-Z]\\(:?.*\\):\\s *$" nil 'noerror)))
        (goto-char ah-clearcase-collect-labels-point)
        (catch 'finished
          (while t
            (forward-line 1)
            (let ((next-line (point)))
              (when (or (eq next-line ah-clearcase-collect-labels-point)
                        (and limit (>= next-line limit)))
                (throw 'finished t))
              ;; if we are here, we have a complete line
              (forward-line -1)
              (when (looking-at "^\\s +[0-9]+\\s +\\([-_.a-zA-Z0-9]+\\)")
                (intern (match-string 1) ah-clearcase-all-labels))
              (goto-char (setq ah-clearcase-collect-labels-point next-line)))))

        (if limit
            ;; Cleartool finished the labels section.  If cleartool
            ;; terminated, we kill the buffer, otherwise we set the
            ;; process property 'ah-clearcase-collect-labels-done to t
            (if (eq (process-status process) 'run)
                (setq ah-clearcase-collect-labels-finished t)
                ;; the process has finished, kill it
                (progn
                  (unless (= (process-exit-status process) 0)
                    (message
                     "non-zero exit code form cleartool while reading labels"))
                  (kill-buffer (current-buffer))))
            ;; cleartool is still sending us data...
            (progn
              (delete-region (point-min) ah-clearcase-collect-labels-point)
              (setq ah-clearcase-collect-labels-point (point-min))))))))

(defun ah-clearcase-collect-labels-for-vob (vob)
  "Start the process of collecting the labels in VOB.
The labels will become available as
`ah-clearcase-collect-labels-filter' parses them."
  (let* ((buffer (generate-new-buffer "*cleartool-dump-vob*"))
         (process (start-process "cleartool-dump-vob" buffer
                                 ah-clearcase-cleartool-program
                                 "dump" "-long" (concat "vob:" vob))))
    (with-current-buffer buffer
      (buffer-disable-undo)
      (setq ah-clearcase-all-labels (make-vector 63 0))
      (set (make-local-variable 'ah-clearcase-collect-labels-point) nil)
      (set (make-local-variable 'ah-clearcase-collect-labels-finished) nil)
      (set-process-filter process 'ah-clearcase-collect-labels-filter)
      (set-process-sentinel process 'ah-clearcase-collect-labels-sentinel))))

(defun ah-clearcase-complete-label (string predicate flag)
  "The completion function on the `ah-clearcase-all-labels'."
  ;; Note that we cannot pass ah-clearcase-all-labels to completing-read
  ;; because its value will be set asynchronously when the cleartool
  ;; ask command finishes.  Thus, we simply check the flag and call
  ;; the proper function (which completing-read would call if we would
  ;; pass ah-clearcase-edcs-all-view-tags directly to it.
  (let ((completion-fn (cond ((eq flag t) 'all-completions)
                             ((eq flag 'lambda)
                              ;; test-completion does not exist in emacs 21.
                              '(lambda (x l &optional p) (intern-soft x l)))
                             ((null flag) 'try-completion)
                             (t (error "unknwn value for flag %S" flag)))))
    (funcall completion-fn string ah-clearcase-all-labels predicate)))

(defun ah-clearcase-read-label (prompt vob &optional initial reuse-labels)
  "Read a label from the minibuffer and return it.

Display PROMPT to the user and read a ClearCase label using the labels
of VOB as possible completions.  When non-nil, INITIAL, is the initial
label name presented to the user.

Before promping the user, an asynchronous cleartool dump command will
be started to fetch the list of labels.  The list of labels is
populated incrementally, so completion is provided from an incomplete
list for the first few seconds.  This implementation has been chosen
to improve responsiveness, but it can be quite annoying.

When REUSE-LABELS is non-nil, the previous list of labels will be used
for completion, without starting a cleartool dump command.  This
option should be used when a function needs to read several labels
from the user, in which case starting several cleartool commands is a
vaste of resources."
  (unless reuse-labels
    (ah-clearcase-collect-labels-for-vob vob))
  (completing-read prompt 'ah-clearcase-complete-label nil nil initial))


;;;; Vc interface + some helpers

(defun ah-clearcase-maybe-set-vc-state (file &optional force)
  "Lazily set the clearcase specific properties of FILE.
If FORCE is not nil, always read the properties."
  (let ((fprop (ah-clearcase-file-fprop file)))
    (when force (ah-clearcase-reset-fprop fprop))
    (when (or (ah-clearcase-fprop-initialized-p fprop)
              (vc-clearcase-registered file))
      (unless fprop (setq fprop (ah-clearcase-file-fprop file)))
      (ah-cleartool-wait-for (ah-clearcase-fprop-version-tid fprop))
      ;; finally prepare a vprop structure for the file's view, but
      ;; only if we haven't already done so.  NOTE: we don't expect
      ;; the view of the file to ever change, so we ignore the 'force
      ;; option.
      (unless (ah-clearcase-fprop-view-tag fprop)
        (ah-clearcase-prepare-vprop file fprop)))))


(eval-when-compile

  ;; If you compile this file, these macros won't exist at runtime, so
  ;; it is safe to give them nice names.

  (defmacro with-clearcase-checkout (thing &rest forms)
    "Ensure that THING is checked out, than execute FORMS.
If THING was checked out by us, we check it back in.  THING can
be either a file or a directory."
    ;; NOTE: we could use make-symbol with the same effect
    (let ((checkout-needed (gensym))
          (real-thing (gensym)))
      `(let* ((,real-thing ,thing)
              (,checkout-needed
               (string=
                (ah-cleartool "desc -fmt \"%%Rf\" \"%s\"" ,real-thing)
                "")))
         (unwind-protect
              (progn
                (when ,checkout-needed
                  (message "Checking out %s" ,real-thing)
                  (ah-cleartool "checkout -reserved -nc \"%s\"" ,real-thing))
                ,@forms)
           (when ,checkout-needed
             (message "Checking in %s" ,real-thing)
             (ah-cleartool "checkin -nc \"%s\"" ,real-thing))))))

  (defmacro with-clearcase-cfile (comment-vars &rest forms)
    "Save a comment in a temporary file, than execute `FORMS'.
`COMMENT-VARS' is a list of (comment-file comment-text),
comment-file will be bound to a temporary file name and
comment-text will be saved into it.  When all is finished, the
comment file is removed."
    ;; NOTE: we could have used
    ;; (defmacro* with-cleacase-cfile ((comment-file comment-text) &body forms)
    ;;    ;; blah blah)
    ;; but we didn't
    (unless (listp comment-vars)
      (error "comment-vars vars should be a list"))
    (unless (= 2 (length comment-vars))
      (error "Expecting two elements in comment-vars"))
    (unless (symbolp (car comment-vars))
      (error "(car comment-vars) is not a symbol"))
    (let ((cfile (car comment-vars))
          (ctext (cadr comment-vars)))
      `(let ((,cfile
              (make-temp-name (concat temporary-file-directory "clearcase-"))))
         (unwind-protect
              (progn
                (with-temp-file ,cfile
                  (insert ,ctext))      ; ctext evaluated once, here
                ,@forms)
           (delete-file ,cfile)))))

  (defmacro ignore-cleartool-errors (&rest forms)
    "Execute forms, trapping any cleartool errors and ignoring them"
    `(condition-case nil
         (progn ,@forms)
       (ah-cleartool-error nil)))

  )                                     ; eval-when-compile

(put 'with-clearcase-checkout 'lisp-indent-function 1)
(put 'with-clearcase-cfile 'lisp-indent-function 1)
(put 'ignore-cleartool-errors 'lisp-indent-function 0)




(defadvice vc-version-backup-file-name
    (after ah-clearcase-cleanup-version (file &optional rev manual regexp))
  "Cleanup rev of \\ and / so it can be stored as a filename."
  (when (string-match "~.*~" ad-return-value)
    (let ((start (match-beginning 0))
          (data (match-string 0 ad-return-value)))
      (setq data (replace-regexp-in-string "[\\\\/]" "~" data))
      (setq ad-return-value
            (concat (substring ad-return-value 0 start) data))))
  ad-return-value)
(ad-activate 'vc-version-backup-file-name)

(defadvice vc-start-entry
    (before ah-clearcase-prepare-checkin-comment
            (file rev comment initial-contents msg action &optional after-hook))
  "Insert an initial comment when checking-in files."
  (let ((fprop (ah-clearcase-file-fprop file)))
    (when (and fprop (ah-clearcase-fprop-checkedout-p fprop))
      ;; so we are checking a file in
      (ah-cleartool-wait-for (ah-clearcase-fprop-comment-tid fprop))
      (setf comment (ah-clearcase-fprop-comment fprop))
      (setf initial-contents t))))
(ad-activate 'vc-start-entry)

(defadvice vc-create-snapshot
    (before ah-clearcase-provide-label-completion first
            (dir name branchp))
  "Override the interactive form so that we have label completion."
  (interactive
   (let* ((d (read-file-name "Directory: "
                             default-directory default-directory t))
          (vob (condition-case nil
                   (ah-clearcase-vob-tag-for-path
                    (if (file-directory-p d) d (file-name-directory d)))
                 (ah-cleartool-error nil))))
     (list d (if vob
                 (ah-clearcase-read-label "Label: " vob)
                 (read-string "New snapshot name: "))
           current-prefix-arg))))

(ad-activate 'vc-create-snapshot)



;;;###autoload(defun vc-clearcase-registered (file)
;;;###autoload  (let (wdview
;;;###autoload        retcode
;;;###autoload        (program ah-clearcase-cleartool-program))
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
  ;; we cannot guarantee that vc.el will give us an expanded file
  ;; name, and cleartool does not know about tilde expansion...
  (setq file (expand-file-name file))
  ;; if the file already has a version set, or we asked for it
  ;; already, return t
  (let ((fprop (ah-clearcase-file-fprop file)))
    (if (ah-clearcase-fprop-initialized-p fprop)
        t
        (ignore-cleartool-errors
         (unless fprop (setq fprop (ah-clearcase-make-fprop :file-name file)))
         (let ((ls-result (ah-cleartool "ls \"%s\"" file)))
           (if (string-match "Rule: \\(.*\\)$" ls-result)
               ;; file is registered
               (progn
                 (ah-clearcase-set-fprop-version-stage-1 fprop ls-result)
                 ;; anticipate that the version will be needed
                 ;; shortly, so ask for it.  When a file is
                 ;; hijacked, do the desc command on the version
                 ;; extended name of the file, as cleartool will
                 ;; return nothing for the hijacked version...
                 (let ((pname
                        (if (ah-clearcase-fprop-hijacked-p fprop)
                            (concat file "@@"
                                    (ah-clearcase-fprop-version fprop))
                            file)))
                   (setf (ah-clearcase-fprop-version-tid fprop)
                         (ah-cleartool-ask
                          (format "desc -fmt \"%%Vn %%PVn %%Rf\" \"%s\"" pname)
                          'nowait fprop
                          'ah-clearcase-set-fprop-version-stage-2))
                   (vc-file-setprop file 'vc-clearcase-fprop fprop))
                 t)                     ;file is registered
               nil)))                   ;file is not registered
        )))


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

'unlocked-change -- file is hijacked."
  ;; we cannot guarantee that vc.el will give us an expanded file
  ;; name, and cleartool does not know about tilda expansion...
  (setq file (expand-file-name file))

  ;; we are asked for a reliable computation of state, so refresh all
  ;; the properties.
  (ah-clearcase-maybe-set-vc-state file 'force)

  (let ((fprop (ah-clearcase-file-fprop file)))

    ;; we are about to operate on the file, so check if the view is
    ;; consistent.  Clearcase operations will occasionally fail saying
    ;; that an update is already in progress for this view.  We can
    ;; anticipate that, because the rule that selects this version
    ;; will be "Rule: <rule info unavailable>".  In that case, we exit
    ;; with an error telling the user to update his view.

    (when (and (ah-clearcase-snapshot-view-p fprop)
               (ah-clearcase-fprop-broken-view-p fprop))
      (error "Snapshot view is inconsistent, run an update"))

    ;; We anticipate that the file's checkout comment might be needed
    ;; shortly so ask for it before we return the state
    (when (ah-clearcase-fprop-checkedout-p fprop)
      (setf (ah-clearcase-fprop-comment-tid fprop)
            (ah-cleartool-ask
             (format "desc -fmt \"%%c\" \"%s\"" file) 'nowait fprop
             '(lambda (fprop comment)
               (setf (ah-clearcase-fprop-comment fprop) comment)))))

    ;; return the state.  The heuristic already gives all the
    ;; information we need.
    (vc-clearcase-state-heuristic file)))


(defun vc-clearcase-state-heuristic (file)
  "Determine the state of FILE.
Use whatever `ah-clearcase-maybe-set-vc-state' gave us.  See
`vc-clearcase-state' for how states are mapped to ClearCase
information."
  (ah-clearcase-maybe-set-vc-state file)
  (let ((fprop (ah-clearcase-file-fprop file)))
    (if (ah-clearcase-fprop-hijacked-p fprop)
        'unlocked-changes
        (if (ah-clearcase-fprop-checkedout-p fprop)
            (if (string= (ah-clearcase-fprop-latest fprop)
                         (ah-clearcase-fprop-parent fprop))
                'edited
                'needs-merge)
            (if (or (ah-clearcase-fprop-checkout-will-branch-p fprop)
                    (string= (ah-clearcase-fprop-latest fprop)
                             (ah-clearcase-fprop-version fprop)))
                'up-to-date
                'needs-patch)))))

(defun vc-clearcase-workfile-version (file)
  "Return the workfile version of FILE.
If the file is checked out, In ClearCase, the version is always
\"CHECKEDOUT\", but the vc.el assumes that checked out is not a
separate version, so we return the parent version in that case."
  (ah-clearcase-maybe-set-vc-state file)
  (ah-clearcase-fprop-version (ah-clearcase-file-fprop file)))

(defun vc-clearcase-latest-on-branch-p (file)
  "Return true if FILE is the latest version on the branch."
  (ah-clearcase-maybe-set-vc-state file)
  (let ((fprop (ah-clearcase-file-fprop file)))
    (string= (ah-clearcase-fprop-version fprop)
             (ah-clearcase-fprop-latest fprop))))

(defun vc-clearcase-checkout-model (file)
  "Checkout model for ClearCase is always locking for every FILE."
  'locking)

(defun vc-clearcase-workfile-unchanged-p (file)
  "Is FILE un-changed?"
  (setq file (expand-file-name file))
  (let ((diff
         (ah-cleartool
          "diff -predecessor -options -headers_only \"%s\"" file)))
    (string= diff "Files are identical\n"))

  ;; NOTE: apparently, the -status_only does not work: it returns
  ;; success all the time in the interactive cleartool process.

  ;;   (condition-case nil
  ;;       (progn
  ;;         (ah-cleartool-ask
  ;;          (format "diff -predecessor -options -status_only \"%s\"" file))
  ;;         t)
  ;;     (error nil))
  )

(defun vc-clearcase-mode-line-string (file)
  "Return the mode line string for FILE."
  (ah-clearcase-maybe-set-vc-state file)
  (let* ((fprop (ah-clearcase-file-fprop file))
         (branch (ah-clearcase-fprop-branch fprop))
         (version-number (ah-clearcase-fprop-version-number fprop)))
    (ah-clearcase-wash-mode-line
     (case (ah-clearcase-fprop-status fprop)
       ('hijacked "Cc:HIJACKED")
       ('broken-view "Cc:BROKEN-VIEW")
       ('reserved (concat "Cc:(R)" branch "/" version-number))
       ('unreserved (concat "Cd:(U)" branch "/" version-number))
       (t (concat "Cc:" branch "/" version-number))))))

(defun vc-clearcase-register (file &optional rev comment)
  "Register FILE with clearcase.  REV and COMMENT are ignored.
ClearCase requires the directory in which file resides to be
checked out for the insertion to work.  If the directory is
checked out, we leave it checked out, otherwise we do a checkout
for the file insertion than a checkin.

NOTE: if dir is not under clearcase, this code will fail.  We
don't attempt to register a directory in clearcase even if one of
it's parents is registered."
  (setq file (expand-file-name file))
  (with-clearcase-checkout (file-name-directory file)
    (message "Registering %s" (file-name-nondirectory file))
    (let ((ah-cleartool-timeout (* 2 ah-cleartool-timeout)))
      (ah-cleartool "mkelem -nc \"%s\"" file))
    (ah-clearcase-maybe-set-vc-state file 'force)
    (vc-resynch-buffer file t t)))

(defun vc-clearcase-responsible-p (file)
  "Return t if we responsible for FILE.
We don't consider ourselves responsible if cleartool ls command
returns a 'pathname not within a VOB' error message."
  (setq file (expand-file-name file))
  (if (file-exists-p file)
      (let ((case-fold-search t))
        (condition-case msg
            (if (ah-cleartool "ls \"%s\"" file)
                t
                nil)                    ; never reached
          (ah-cleartool-error
           (if (string-match "Pathname is not within a VOB:" (cadr msg))
               nil
               t))))))

(defun vc-clearcase-checkin (file rev comment)
  "Checkin FILE.
REV is ignored, COMMENT is the checkin comment."
  (setq file (expand-file-name file))
  (when rev
    (message "Ignoring revision specification: %s" rev))
  (with-clearcase-cfile (comment-file comment)
    ;; let the cleartool error be directly reported
    (ah-cleartool "checkin -cfile %s \"%s\"" comment-file file)
    (ah-clearcase-maybe-set-vc-state file 'force)))


(defun ah-clearcase-find-version-helper (file rev destfile)
  "Get the FILE revision REV into DESTFILE.
This is a helper function user by both
`vc-clearcase-find-version' and `vc-clearcase-checkout' (since we
want to preserve the Emacs 21.3 `vc-clearcase-checkout'
behavior."
  (when (string= rev "")
    (error "Refusing to checkout head of trunk"))
  (let ((fprop (ah-clearcase-file-fprop file)))
    (unless rev
      (setq rev (ah-clearcase-fprop-latest-sel fprop)))
    (ah-cleartool "get -to \"%s\" \"%s@@%s\"" destfile file rev)))


(defun vc-clearcase-find-version (file rev buffer)
  "Fetch FILE revision REV and place it into BUFFER.
If REV nil, it will get the latest on the branch, if REV is the
empty string, we signal an error, since head of trunk has no
meaning in ClearCase."
  (setq file (expand-file-name file))
  (let ((tmpfile (make-temp-file (expand-file-name file))))
    ;; it seems make-temp-file creates the file, and clearcase will
    ;; refuse to get the version into an existing file.
    (delete-file tmpfile)
    (unwind-protect
         (progn
           (ah-clearcase-find-version-helper file rev tmpfile)
           (with-current-buffer buffer
             (insert-file-contents-literally tmpfile)))
      (delete-file tmpfile))))


(defun ah-clearcase-finish-checkout (file rev comment mode)
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
          (options (concat "-ptime "
                           "-cfile " comment-file
                           " " (when rev "-version ")))
          (co-mode (if (eq mode 'reserved) "-reserved " "-unreserved "))
          ;; increase the cleartool timeout for the checkout operation
          (ah-cleartool-timeout (* 1.5 ah-cleartool-timeout)))
      ;; NOTE: if this fails, we should prompt the user to checkout
      ;; unreserved.
      (ah-cleartool "checkout %s %s \"%s\"" co-mode options pname))
    (ah-clearcase-maybe-set-vc-state file 'force)
    (vc-resynch-buffer file t t)))

;; This would be so much easier if vc-start-entry would accept a
;; closure to pass it to us...

(defun ah-clearcase-finish-checkout-reserved (file rev comment)
  "Do a reserved checkout on FILE with REV and COMMENT."
  (ah-clearcase-finish-checkout file rev comment 'reserved))

(defun ah-clearcase-finish-checkout-unreserved (file rev comment)
  "Do an unreserved checkout on FILE with REV and COMMENT."
  (ah-clearcase-finish-checkout file rev comment 'unreserved))

(defun ah-clearcase-revision-reserved-p (file)
  "Return t if FILE is checked out reserved.
If yes, return the user and view that has the reserved checkout,
otherwise return nil."
  (let ((fprop (ah-clearcase-file-fprop file))
        (checkouts
         (split-string
          (ah-cleartool "lsco -fmt \"%%PVn %%Rf %%Tf %%u\\n\" \"%s\"" file)
          "[\n\r]+")))
    (let* ((match (concat (ah-clearcase-fprop-version fprop) " reserved"))
           (len (length match))
           (rev (catch 'found
                  (dolist (c checkouts)
                    (when (eq (compare-strings match 0 len c 0 len) t)
                      (throw 'found c))))))
      (if rev
          (let ((elements (split-string rev)))
            (cons (nth 3 elements) (nth 2 elements)))
          nil))))

(defcustom ah-clearcase-checkout-comment-type 'normal
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

(defcustom ah-clearcase-checkout-policy 'heuristic
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
     (let* ((fprop (ah-clearcase-file-fprop file))
            checkout)
       ;; need to find out if we have to checkout reserved or
       ;; unreserved.
       (ecase ah-clearcase-checkout-policy
         ('reserved (setq checkout 'ah-clearcase-finish-checkout-reserved))
         ('unreserved (setq checkout 'ah-clearcase-finish-checkout-unreserved))
         ('heuristic
          (cond
            ;; if the checkout will create a branch, checkout reserved
            ((ah-clearcase-fprop-checkout-will-branch-p fprop)
             (setq checkout 'ah-clearcase-finish-checkout-reserved))

            ;; if we are not latest on branch and we are asked to
            ;; checkout this version (eq rev nil), we checkout
            ;; unseserved.
            ((and (null rev)
                  (not (string= (ah-clearcase-fprop-latest fprop)
                                (ah-clearcase-fprop-version fprop))))
             ;; patch rev first
             (setq rev (ah-clearcase-fprop-version fprop))
             (setq checkout 'ah-clearcase-finish-checkout-unreserved))

            ;; if someone else has checked out this revision in
            ;; reserved mode, ask the user if he wants an unreserved
            ;; checkout.
            (t (let ((user-and-view (ah-clearcase-revision-reserved-p file)))
                 (if user-and-view
                     (when (yes-or-no-p
                            (concat
                             "This revision is checked out reserved by "
                             (car user-and-view) "in" (cdr user-and-view)
                             ".  Checkout unreserved? "))
                       (setq checkout 'ah-clearcase-finish-checkout-unreserved))
                     ;; no one has this version checked out, checkout
                     ;; reserved.
                     (setq checkout 'ah-clearcase-finish-checkout-reserved)))))))
       (if checkout
           (ecase ah-clearcase-checkout-comment-type
             ('normal (vc-start-entry
                       file rev nil nil "Enter a checkout comment" checkout))
             ('brief (let ((comment (read-string "Enter a checkout comment: ")))
                       (funcall checkout file rev comment)))
             ('none (funcall checkout file rev "")))
           (message "Aborted."))))
    ((and (not editable) destfile)
     ;; Check out an arbitrary version to the specified file
     (ah-clearcase-find-version-helper file rev destfile))
    ((and (not editable) (or (null rev) (eq rev t)))
     ;; Update the file in the view (no-op in dynamic views)
     (let ((update-result (ah-cleartool "update -rename \"%s\"" file)))
       (when (string-match
              "^Update log has been written to .*$" update-result)
         (message (match-string 0 update-result)))
       (ah-clearcase-maybe-set-vc-state file 'force)
       (vc-resynch-buffer file t t)))
    ((not editable)                  ; last case left for not editable
     (error "Cannot to update to a specific revision"))
    (t
     (error "Bad param combinations in vc-clearcase-checkout: %S %S %S"
            editable rev destfile))))

(defcustom ah-clearcase-rmbranch-on-revert-flag t
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
  "Cancel a checkout on FILE.
CONTENTS-DONE is ignored. The
`ah-clearcase-rmbranch-on-revert-flag' is honoured."
  (setq file (expand-file-name file))
  (let* ((fprop (ah-clearcase-file-fprop file))
         (empty-branch-p (equal "0" (ah-clearcase-fprop-version-number fprop))))
    (ah-cleartool "uncheckout -keep \"%s\"" file)
    (when (and empty-branch-p ah-clearcase-rmbranch-on-revert-flag)
      (let ((base (ah-clearcase-fprop-version-base fprop)))
        (ah-cleartool "rmbranch -force -nc \"%s@@%s\"" file base)))
    (ah-clearcase-maybe-set-vc-state file 'force)))


(defun vc-clearcase-merge (file rev1 rev2)
  "Merge into FILE REV1 up to REV2.

NOTE: when trying to merge revisions (vc-merge) on a file that is
not checked-out, vc asks for a checkout, but that the comment
window pops up, and `vc-merge' assumes the file was already
checked out.  We need to do an automatic checkout in this case,
but how do we detect it?"
  (setq file (expand-file-name file))
  (let ((merge-status
         (ah-cleartool
          "merge -abort -insert -to \"%s\" -ver %s %s" file rev1 rev2)))
    (with-current-buffer (get-buffer-create "*vc-merge-result*")
      (let ((inhibit-read-only t))
        (insert merge-status)
        (switch-to-buffer-other-window (current-buffer) 'norecord)
        (shrink-window-if-larger-than-buffer)))
    0))                                 ; return success

(defun vc-clearcase-merge-news (file)
  "Merge the new versions in FILE."
  (setq file (expand-file-name file))
  (let ((latest (concat file "@@"
                        (ah-clearcase-fprop-latest-sel
                         (ah-clearcase-file-fprop file)))))
    (message "Merging LATEST into this version")
    ;; NOTE: we abort if anything goes wrong with the merge.  Let the
    ;; error propagate to the vc package.  If we just return 1, it
    ;; will try to invoke smerge-mode or ediff, expecting CVS-like
    ;; conflict markers.
    (let ((merge-status
           (ah-cleartool "merge -abort -to \"%s\" \"%s\"" file latest)))
      (with-current-buffer (get-buffer-create "*vc-merge-result*")
        (let ((inhibit-read-only t))
          (insert merge-status)
          (switch-to-buffer-other-window (current-buffer) 'norecord)
          (shrink-window-if-larger-than-buffer)))
      0)))                              ; return success


(defvar ah-clearcase-file-name nil
  "File name for which this log was generated.")

(make-variable-buffer-local 'ah-clearcase-file-name)
(put 'ah-clearcase-file-name 'permanent-local t)

(defun vc-clearcase-print-log (file)
  "Insert the history of FILE into the *clearcase-lshistory* buffer.

With a prefix argument, all events are listed (-minor option is
sent to cleartool).

This is not intended to be called directly from the vc.el.
Instead, `vc-print-log' is advised to call this function directly
for Clearcase registered files."
  (setq file (expand-file-name file))
  (let ((buf (get-buffer-create "*clearcase-lshistory*")))
    (vc-setup-buffer buf)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (ah-clearcase-log-view-mode)
        (let* ((args (list "-fmt" ah-clearcase-lshistory-fmt file))
               (process (ah-cleartool-do "lshistory"
                                         (if current-prefix-arg
                                             (cons "-minor" args)
                                             args)
                                         (current-buffer))))
          (setq ah-clearcase-file-name file)
          (setq ah-cleartool-finished-function
                '(lambda ()
                  (let ((fprop
                         (ah-clearcase-file-fprop ah-clearcase-file-name)))
                    (vc-clearcase-show-log-entry
                     (ah-clearcase-fprop-version fprop)))))
          (switch-to-buffer-other-window (process-buffer process)))))))

(defadvice vc-print-log (around ah-clearcase-print-log-advice)
  "On Clearcase files, call 'vc-clearcase-print-log' directly.
On all other files call the normal `vc-print-log'."
  (vc-ensure-vc-buffer)
  (let ((file buffer-file-name))
    (if (vc-clearcase-registered file)
        (vc-clearcase-print-log file)
        ad-do-it)))
(ad-activate 'vc-print-log)

(defun vc-clearcase-show-log-entry (version)
  "Search for VERSION in the current buffer.
Only works for the clearcase log format defined in
`ah-clearcase-lshistory-fmt'."
  (let ((regexp
         (concat "^version: "
                 (replace-regexp-in-string "[\\\\/]" "[\\\\/]" version)
                 "\\>"))
        pos)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward regexp (point-max) 'noerror)
        (setq pos (match-beginning 0))))
    (when pos (goto-char pos))))


(defcustom ah-clearcase-diff-format 'diff
  "Format of the output by the cleartool diff command."
  :type '(choice (const :tag "Diff Format" diff)
          (const :tag "Serial Format" serial))
  :group 'vc-clearcase)

(defcustom ah-clearcase-diff-cleanup-flag t
  "Non-nil means remove ^M characters from the diff output."
  :type 'boolean
  :group 'vc-clearcase)

(defun vc-clearcase-diff (file &optional rev1 rev2)
  "Return the diff on FILE between REV1 and REV2."
  (setq file (expand-file-name file))
  (let ((fprop (ah-clearcase-file-fprop file)))
    (when (not rev1)
      (setq rev1 (ah-clearcase-fprop-version fprop)))
    (ah-cleartool "cd \"%s\"" (file-name-directory file))
    (setq file (file-name-nondirectory file))
    (let ((fver1 (concat file "@@" rev1))
          (fver2 (if rev2 (concat file "@@" rev2) file)))
      (with-current-buffer (get-buffer-create "*vc-diff*")
        (message "Comparing file revisions...")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (let* ((diff-format (ecase ah-clearcase-diff-format
                                ('diff "-diff_format")
                                ('serial "-serial_format")))
                 (diff (ah-cleartool
                        "diff %s \"%s\" \"%s\""
                        diff-format fver1 fver2)))
            (insert diff)
            (when ah-clearcase-diff-cleanup-flag
              (goto-char (point-min))
              (while (re-search-forward "\r$" nil t)
                (replace-match "" nil nil)))
            (goto-char (point-min))

            (not
             ;; the way we determine whether the files are identical
             ;; depends on the diff format we use.
             (ecase ah-clearcase-diff-format
               ('diff
                ;; diff format has an empty buffer
                (equal (point-min) (point-max)))
               ('serial
                ;; serial format prints "Files are identical", so
                ;; we look for that.
                (looking-at "Files are identical"))))))))))


(defun vc-clearcase-annotate-command (file buf rev)
  "Get the annotations for FILE and put them in BUF.
REV is the revision we want to annotate.  With prefix argument,
will ask if you want to display the deleted sections as well."
  (setq file (expand-file-name file))
  (let ((pname (concat file (when rev (concat "@@" rev)))))
    (vc-setup-buffer buf)
    (with-current-buffer buf
      (let ((fmt-args '("-fmt" "%-9.9Sd %-4.4u %Sn |"))
            (rm-args (when (and current-prefix-arg
                                (y-or-n-p "Show deleted sections? "))
                       '("-rm" "-rmfmt" "D %-9.9Sd %-4.4u |"))))
        (ah-cleartool-do
         "annotate"
         (append fmt-args rm-args `("-out" "-" "-nheader" ,pname))
         buf)
        (setq ah-cleartool-finished-function
              '(lambda ()
                (let ((buffer (current-buffer)))
                  (ah-clearcase-annotate-post-process buffer)
                  (ah-clearcase-annotate-mark-deleted buffer))))))))

(defun vc-clearcase-annotate-difference (point)
  "Return the age in days of POINT."
  (get-text-property point 'vc-clearcase-age))

(defun vc-clearcase-annotate-time ()
  "Return the time in days of (point)."
  (get-text-property (point) 'vc-clearcase-time))

(defun vc-clearcase-annotate-extract-revision-at-line ()
  "Return the version of (point)."
  (get-text-property (point) 'vc-clearcase-revision))

(defconst ah-clearcase-annotate-deleted-face
  (progn
    (unless (intern-soft 'ah-clearcase-deleted-face)
      (let ((face (make-face 'ah-clearcase-deleted-face)))
        (set-face-attribute face nil :strike-through t)))
    'ah-clearcase-deleted-face))

(defconst ah-clearcase-annotate-months
  '(("Jan" . 1) ("Feb" . 2) ("Mar" . 3) ("Apr" . 4)
    ("May" . 5) ("Jun" . 6) ("Jul" . 7) ("Aug" . 8)
    ("Sep" . 9) ("Oct" . 10) ("Nov" . 11) ("Dec" . 12)))

(defconst ah-clearcase-annotate-date-rx
  "\\([0-9]+\\)-\\([A-Za-z]+\\)-\\([0-9]+\\)")

(defun ah-clearcase-annotate-mktime (time-str)
  "Convert TIME-STR into a fractional number of days.
NOTE: we don't use `vc-annotate-convert-time' since it is not
available in Emacs 21."
  (when (and (stringp time-str)
             (string-match ah-clearcase-annotate-date-rx time-str))
    (let ((day (string-to-number (match-string 1 time-str)))
          (month
           (cdr (assoc (match-string 2 time-str)
                       ah-clearcase-annotate-months)))
          (year (string-to-number (match-string 3 time-str))))
      (incf year (if (< year 70) 2000 1900))
      (/ (float-time (encode-time 0 0 0 day month year)) 24 3600))))

(defun ah-clearcase-annotate-post-process (buffer)
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
          (setq time (ah-clearcase-annotate-mktime time-str))
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
               (str "                 .                  |")
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


(defun ah-clearcase-annotate-mark-deleted (buffer)
  "Mark all deleted files in BUFFER with strike-through face.
When BUFFER is nil, the current buffer is used."
  (with-current-buffer buffer
    (save-excursion
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (while (re-search-forward ".*|D .*|\\(.*\\)\\s-*$" nil 'noerror)
          (put-text-property (match-beginning 1) (match-end 1)
                             'face '(:strike-through t)))))))

(defcustom ah-clearcase-no-label-action 'ask
  "What to do when we are asked to apply a label that does not exist.
There are three possible values for this variable:

'error -- an error will be signalled when we are asked to apply a
          non existent label.

'create -- the label will be created if it does not exist.

'ask -- ask the user whether she wants to create the label or
not.

NOTE: in ClearCase a label exists independentlty from the files
it is applied to.  A label must be created first before it can be
applied."
  :type '(choice (const :tag "Signal error" error)
          (const :tag "Automatically create it" create)
          (const :tag "Ask the user" ask))
  :group 'vc-clearcase)

(defcustom ah-clearcase-confirm-label-move t
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

(defun vc-clearcase-create-snapshot (dir name branchp)
  "Label all files under DIR using NAME as the label.

BRANCHP is used to move an existing label.  This is not the
default behavior, but the default behavior is useless for
Clearcase.

First, if the label NAME does not exist, if is created with
mklbtype as an ordinary, non shared label.  Than the label is
applied recursively on DIR (but not moved if it already exists).
Than, for each parent directory of DIR the label is applied only
to that directory.  This means that you can select this version
of the sources with this single line in the configspec:

element * NAME -nocheckout"
  (when (and branchp
             ah-clearcase-confirm-label-move
             (not (yes-or-no-p "Move existing label? ")))
    (error "Aborted"))
  (setq dir (expand-file-name dir))
  (ah-cleartool "cd \"%s\"" (file-name-directory dir))
  ;; let's see if the label exists
  (condition-case nil
      (ah-cleartool "desc -fmt \"ok\" lbtype:%s" name)
    (ah-cleartool-error
     (let ((should-create
            (ecase ah-clearcase-no-label-action
              ('create t)
              ('error nil)
              ('ask (yes-or-no-p
                     (format "Label %s does not exist.  Create it? " name))))))
       (if should-create
           (progn
             (message "Creating label %s" name)
             (ah-cleartool "mklbtype -ordinary -nc lbtype:%s" name)
             (message nil))
           (error "Label %s does not exist and will not create it." name)))))
  (let ((dir? (file-directory-p dir)))
    (message "Applying label...")
    ;; NOTE: the mklabel command might fail if some files are
    ;; hijacked... The rest of the files will be labeled though...
    (ah-cleartool
     "mklabel -nc %s %s lbtype:%s \"%s\""
     (if branchp "-replace" "") (if dir? "-recurse" "") name dir)
    (when dir?                     ; apply label to parent directories
      (message "Applying label to parent directories...")
      (ignore-cleartool-errors
       (while t                  ; until cleartool will throw an error
         (setq dir (replace-regexp-in-string "[\\\\/]$" "" dir))
         (setq dir (file-name-directory dir))
         (ah-cleartool
          "mklabel -nc %s lbtype:%s \"%s\""
          (if branchp "-replace" "") name dir)))))
  (message "Finished applying label"))


(defun vc-clearcase-previous-version (file rev)
  "Return the FILE revision that precedes the revision REV.
Return nil if no such revision exists."
  (setq file (expand-file-name file))
  ;; We simply ask clearcase to tell us the previous version name
  ;; (%PVn).  If we came across a version number of 0, we ask for the
  ;; previous version again, since the initial version on a branch (0)
  ;; is identical to the original version in the parent branch...
  (let ((prev (ah-cleartool "desc -fmt \"%%PVn\" \"%s@@%s\"" file rev)))
    (if (string= prev "")
        nil
        (if (string-match "[\\/]0$" prev)
            (vc-clearcase-previous-version file prev)
            prev))))

(defun vc-clearcase-next-version (file rev)
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
  (let* ((fprop (ah-clearcase-file-fprop file))
         (revision-list (ah-clearcase-fprop-revision-list fprop)))
    (unless revision-list
      ;; If we don't have a revision list, build one now
      (message "Building revision list...")
      (let* ((prev (ah-clearcase-fprop-latest fprop)))
        (setq revision-list (list prev))
        (while (not (string= prev ""))
          (setq prev (ah-cleartool "desc -fmt \"%%PVn\" \"%s@@%s\"" file prev))
          (unless (or (string= prev "") (string-match "[\\/]0$" prev))
            (setq revision-list (cons prev revision-list))))
        (setf (ah-clearcase-fprop-revision-list fprop) revision-list)
        (message "Building revision list...done.")))
    (car (cdr-safe (member rev revision-list)))))

;;; NOTE: for some reason, the renamed file does not show up as a
;;; clearcase registered until after I kill it and re-open it...

(defun vc-clearcase-rename-file (old new)
  "Rename file from OLD to NEW.
Both in the working area and in the repository are renamed."
  ;; Unfortunately vc-rename-file will not expand these for us
  (setq old (expand-file-name old))
  (setq new (expand-file-name new))
  (with-clearcase-checkout (file-name-directory old)
    (with-clearcase-checkout (file-name-directory new)
      (with-clearcase-cfile (comment-file
                             (format "*renamed from %s to %s*" old new))
        ;; let the cleartool error be directly reported
        (ah-cleartool "mv -cfile %s \"%s\" \"%s\"" comment-file old new)))))


;;;; A library of clearcase utilities

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
  (ah-cleartool "cd \"%s\"" dir)
  (ah-cleartool "desc -fmt \"ok\" lbtype:%s" label-1)
  (ah-cleartool "desc -fmt \"ok\" lbtype:%s" label-2)

  (let ((buf1 (get-buffer-create " *ah-clearcase-label-1*"))
        (buf2 (get-buffer-create " *ah-clearcase-label-2*"))

        ;; Will hold the file report.  KEY is file-name, VALUE is
        ;; (cons label-1-version label-2-version).  If one of the
        ;; versions does not exist, the string "*no version*" is used
        ;; instead.
        (report (make-hash-table :test 'equal))

        ;; We don't want the full pathname in front of each file so we
        ;; remove it by skipping SKIP chars.
        (skip (length dir)))

    ;; Start a cleartool find for label-1
    (with-current-buffer buf1
      (buffer-disable-undo)
      (erase-buffer)
      (ah-cleartool-do
       "find" (list dir "-version" (format "lbtype(%s)" label-1) "-print")
       buf1))

    ;; Start a cleartool find for label-2
    (with-current-buffer buf2
      (buffer-disable-undo)
      (erase-buffer)
      (ah-cleartool-do
       "find" (list dir "-version" (format "lbtype(%s)" label-2) "-print")
       buf2))

    ;; Wait for both processes to complete
    (with-timeout (30 (error "Cleartool takes too long to complete"))
      ;; we use ignore-errors because the cleartool sentinel deletes
      ;; the process on exit.
      (while (or (ignore-errors (eq (process-status buf1) 'run))
                 (ignore-errors (eq (process-status buf2) 'run)))
        (accept-process-output)
        (sit-for 1)))

    ;; Process the listed files for label-1.  For each line in the
    ;; buffer, find the file and version and add it to the report.
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

    ;; Process the listed files for label-2.  For each line in the
    ;; buffer, find the file and version than update the report with
    ;; the second label's version.  We remove entries that have the
    ;; same version for both labels and add new entries for files that
    ;; only have label 2.
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

;;;; Additional vc clearcase commands (for files)

(defun vc-clearcase-what-version (file)
  "Show what is the version of FILE."
  (interactive (list (buffer-file-name (current-buffer))))
  (if (and (stringp file) (vc-clearcase-registered file))
      (progn
        (ah-clearcase-maybe-set-vc-state file)
        (let* ((fprop (ah-clearcase-file-fprop file))
               (version (ah-clearcase-fprop-version fprop))
               (co-status (ah-clearcase-fprop-status fprop)))
          (message "File version: %s%s" version
                   (case co-status
                     ('reserved ", checkedout reserved")
                     ('unreserved ", checkedout unreserved")
                     ('hijacked ", hijacked")
                     ('broken-view ", broken view")
                     (t "")))))
      (message "Not a clearcase file")))

(defun vc-clearcase-what-rule (file)
  "Show the configspec rule for FILE."
  (interactive (list (buffer-file-name (current-buffer))))
  (if (and (stringp file) (vc-clearcase-registered file))
      (progn
        (ah-clearcase-maybe-set-vc-state file)
        (let ((rule (ah-clearcase-fprop-what-rule
                     (ah-clearcase-file-fprop file))))
          (if rule
              (message "Configspec rule: %s" rule)
              (message "No configspec rule"))))
      (message "Not a clearcase file")))

(defun vc-clearcase-what-view-tag (file)
  "Show view in which FILE resides."
  (interactive (list (buffer-file-name (current-buffer))))
  (if (and (stringp file) (vc-clearcase-registered file))
      (progn
        (ah-clearcase-maybe-set-vc-state file)
        (let ((view-tag (ah-clearcase-fprop-view-tag
                         (ah-clearcase-file-fprop file))))
          (if view-tag
              (message "View tag: %s" view-tag)
              (message "View tag not (yet?) known"))))
      (message "Not a clearcase file")))

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
           "Vtree_browser" nil ah-clearcase-vtree-program file))
        (message "Not a clearcase file"))))


;;;; Additional vc clearcase commands (for directories)

(defconst ah-cleartool-lsco-fmt
  (concat "----------\n"
          "file: %n\n"
          "parent-version: %PVn\n"
          "checkout-status: (%Rf)\n"
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
        (other-options (list "-recurse" "-fmt" ah-cleartool-lsco-fmt dir)))
    (with-current-buffer (get-buffer-create "*clearcase-lsco*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq default-directory dir)
        (insert "Listing checkouts in " dir "\n")
        (insert "Cleartool command: "
                (format "%S" (append user-selection other-options))
                "\n")
        (ah-clearcase-log-view-mode)
        (let ((process
               (ah-cleartool-do
                "lsco"
                (append user-selection other-options) (current-buffer))))
          (switch-to-buffer-other-window (process-buffer process)))))))

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
        (let ((process (ah-cleartool-do "update" options (current-buffer))))
          (switch-to-buffer-other-window (process-buffer process))
          ;; TODO: how do we refresh all the files that were loaded
          ;; from this view?
          )))))

;;;###autoload
(defun vc-clearcase-label-diff-report (dir label-1 label-2)
  "Report the changed file revisions between labels.
A report is prepared in the *label-diff-report* buffer for the
files in `dir' that have different revisions between `label-1'
and `label-2'."
  (interactive
   (let ((d (read-file-name "Report on directory: "
                            default-directory default-directory t nil)))
     (assert (file-directory-p d))
     (let* ((vob (ah-clearcase-vob-tag-for-path d))
            (l1 (ah-clearcase-read-label "Label 1 (newer): " vob))
            (l2 (ah-clearcase-read-label "Label 2 (older): " vob nil t)))
       (list d l1 l2))))
  (setq dir (expand-file-name dir))
  (message "Fetching label differencess...")
  (let ((diff (vc-clearcase-get-label-differences dir label-1 label-2))
        ;; the format string for a line in the report
        line-fmt)
    (setq diff (sort* diff 'string< :key 'car))
    (loop for (file rev-1 rev-2) in diff
       maximize (length file) into file-len
       maximize (length rev-1) into lb1-len
       maximize (length rev-2) into lb2-len
       finally do
       (setq line-fmt (format "%% 3d    %%-%ds    %%-%ds    %%-%ds\n"
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
        (insert (make-string (length header) ?=)))
      (insert "\n")

      (loop for (file rev-1 rev-2) in diff
         count 1 into pos
         do (insert (format line-fmt pos file rev-1 rev-2)))

      (goto-char (point-min))
      (buffer-enable-undo)
      (pop-to-buffer (current-buffer))))
  (message "Fetching label differencess...done."))

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
      (ah-cleartool-do "ls" (list "-recurse" "-short" "-view_only") buf))
    (pop-to-buffer buf)))


;;;; Editing configspecs

(defvar ah-clearcase-edcs-view-tag nil
  "The name of the view whose configspec we are editing.")

;; NOTE: for some configspecs cleartool will want to ask questions, I
;; didn't find a way to turn that off.

(defun ah-clearcase-setcs (&optional buffer view-tag)
  "Set the configspec found in BUFFER to the VIEW-TAG.

If buffer is nil, the current buffer is used.  If view-tag is
nil, the value of ah-clearcase-edcs-view-tag is used (local to
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
                       ah-clearcase-edcs-view-tag)))
  (let ((vprop (ah-clearcase-get-vprop view-tag))
        (configspec (buffer-file-name buffer)))
    (unless configspec (error "Buffer has no file associated with it"))
    (unless (memq (ah-clearcase-vprop-type vprop) '(snapshot dynamic))
      (error "Nothing known about %s.  Visit a file in the view first"
             view-tag))
    (when (buffer-modified-p buffer)
      (if (yes-or-no-p (format "Save %s? " configspec))
          (save-buffer buffer)
          (error "Aborted")))
    (case (ah-clearcase-vprop-type vprop)
      ('dynamic
       ;; in a dynamic view, we simply set the configspec, than
       ;; trigger a resynch on all the visited files from that view.
       (ah-cleartool "setcs -tag %s \"%s\"" view-tag configspec)
       (message "%s's confispec updated." view-tag)
       (ah-clearcase-refresh-files-in-view view-tag))

      ('snapshot
       ;; in a snapshot view, a update will be triggered, so we set
       ;; the configspec with a ah-cleartool-do command, and trigger
       ;; the resynch in its finished callback.
       (with-current-buffer (get-buffer-create "*clearcase-setcs*")
         (let ((inhibit-read-only t))
           (erase-buffer)
           (cd (ah-clearcase-vprop-root vprop))
           (let ((process
                  (ah-cleartool-do "setcs"
                                   (list "-tag" view-tag configspec)
                                   (current-buffer))))
             (switch-to-buffer-other-window (process-buffer process))
             ;; reuse this variable to hold the view tag in the update
             ;; buffer.
             (set (make-local-variable 'ah-clearcase-edcs-view-tag) view-tag)
             (setq ah-cleartool-finished-function
                   '(lambda ()
                     (ah-clearcase-refresh-files-in-view
                      ah-clearcase-edcs-view-tag))))))))))

(defun ah-clearcase-setcs-and-kill-buffer (&optional buffer view-tag)
  "Set the configspec found in BUFFER to the VIEW-TAG than kill the buffer."
  (interactive (list (current-buffer)
                     (with-current-buffer (current-buffer)
                       ah-clearcase-edcs-view-tag)))
  (ah-clearcase-setcs buffer view-tag)
  (let ((window (get-buffer-window buffer t)))
    (when (and window (not (window-dedicated-p window)))
      (ignore-errors (delete-window window))))
  (kill-buffer buffer))

(define-derived-mode ah-clearcase-edcs-mode fundamental-mode
  "Configspec"
  "Generic mode to edit clearcase configspecs."
  (make-local-variable 'ah-clearcase-edcs-view-tag)
  ;; 'adapted' from values in emacs-lisp-mode
  (setq comment-start "#"
        comment-start-skip "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)#+ *"
        comment-end ""
        comment-end-skip nil)
  (font-lock-mode t))

;; Provide a shorter alias for the edcs mode.  This is useful if you
;; want to keep configspecs separately and have mode tags in them.
(defalias 'edcs-mode 'ah-clearcase-edcs-mode)


(easy-mmode-defmap ah-clearcase-edcs-mode-map
                   '(("\C-c\C-s" . ah-clearcase-setcs)
                     ("\C-c\C-c" . ah-clearcase-setcs-and-kill-buffer))
                   "Keymap for Clearcase Edit Configspec mode")

(modify-syntax-entry ?\# "<" ah-clearcase-edcs-mode-syntax-table)
(modify-syntax-entry ?\n ">" ah-clearcase-edcs-mode-syntax-table)


;;;###autoload
(defun vc-clearcase-edcs (view-tag)
  "Fetch the config spec for VIEW-TAG and pop up a buffer with it.
In interactive mode, prompts for a view-tag name with the default
of the current file's view-tag."
  (interactive
   (list
    (ah-clearcase-read-view-tag
     "Edit configspec for view: "
     ;; get an initial view-tag if possible.
     (let ((file (buffer-file-name (current-buffer))))
       (when (and file (vc-clearcase-registered file))
         (ah-clearcase-fprop-view-tag (ah-clearcase-file-fprop file)))))))

  (message "Fetching configspec for %s" view-tag)
  (let ((tid (ah-cleartool-ask (concat "catcs -tag " view-tag) 'nowait))
        (csfile (format "%s%s.configspec" temporary-file-directory view-tag)))
    (with-current-buffer (find-file-noselect csfile)
      (ah-clearcase-edcs-mode)
      (setq ah-clearcase-edcs-view-tag view-tag)
      (buffer-disable-undo)
      (erase-buffer)
      (insert (ah-cleartool-wait-for tid))
      (buffer-enable-undo)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer))
      (message "Edit your configspec.  Type C-c C-c when done."))))

;;;###autoload
(defun vc-clearcase-start-view (view-tag)
  "Start the dynamic view for VIEW-TAG.
In interactive mode, prompts for a view-tag name."
  (interactive (list (ah-clearcase-read-view-tag "Start dynamic view: ")))
  (message "Starting %s dynamic view..." view-tag)
  (message (ah-cleartool "startview %s" view-tag))
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

;;;###autoload
(progn
  (define-key-after vc-menu-map [separator-clearcase] '("----") 'separator2)
  (define-key-after vc-menu-map [vc-clearcase-what-version]
    '("Show file version" . vc-clearcase-what-version) 'separator2)
  (define-key-after vc-menu-map [vc-clearcase-what-rule]
    '("Show configspec rule" . vc-clearcase-what-rule) 'separator2)
  (define-key-after vc-menu-map [vc-clearcase-what-view-tag]
    '("Show view tag" . vc-clearcase-what-view-tag) 'separator2)
  (define-key-after vc-menu-map [vc-clearcase-gui-vtree-browser]
    '("Browse version tree (GUI)" . vc-clearcase-gui-vtree-browser) 'separator2))

;; 'borrowed' from pcvs-defs.el, Clearcase commands that are not file
;; related will go in a Clearcase menu under Tools.
;;;###autoload
(defvar clearcase-global-menu
  (let ((m (make-sparse-keymap "Clearcase")))
    (define-key m [vc-clearcase-label-diff-report]
      '(menu-item "Label diff report..." vc-clearcase-label-diff-report
        :help "Report file version differences between two labels"))
    (define-key m [separator-clearcase-1]
      '("----" 'separator-1))
    (define-key m [vc-clearcase-list-view-private-files]
      '(menu-item "List View Private Files..."
        vc-clearcase-list-view-private-files
        :help "List view private files in a directory"))
    (define-key m [vc-clearcase-list-checkouts]
      '(menu-item "List Checkouts..." vc-clearcase-list-checkouts
        :help "List Clearcase checkouts in a directory"))
    (define-key m [vc-clearcase-update-view]
      '(menu-item "Update snapshot view..." vc-clearcase-update-view
        :help "Update a snapshot view"))
    (define-key m [vc-clearcase-edcs]
      '(menu-item "Edit Configspec..." vc-clearcase-edcs
        :help "Edit a view's configspec"))
    (define-key m [vc-clearcase-start-view]
      '(menu-item "Start dynamic view..." vc-clearcase-start-view
        :help "Start a dynamic view"))
    (fset 'clearcase-global-menu m)))

;;;###autoload
(progn
  (define-key-after menu-bar-tools-menu [ah-clearcase]
    '(menu-item "Clearcase" clearcase-global-menu)
    'vc))


;;;; Debugging aids, reporting bugs

(defun ah-clearcase-version ()
  "Return the clearcase version as a string.
This is the string returned by the cleartool -version command."
  (with-temp-buffer
    (setq ah-cleartool-finished-function (lambda () (throw 'done nil)))
    (ah-cleartool-do "-version" nil (current-buffer))
    (catch 'done (while t (sit-for 0.1)))
    (replace-regexp-in-string "\r\n?" "\n" (buffer-string))))

(defun vc-clearcase-report-bug ()
  "Submit via mail a bug report on vc-clearcase.el"
  (interactive)
  (require 'reporter)
  (let ((reporter-prompt-for-summary-p t))
    (reporter-submit-bug-report
     vc-clearcase-maintainer-address
     "vc-clearcase.el"
     `( vc-clearcase-cvsid

        ah-clearcase-cleartool-program
        ah-cleartool-timeout
        ah-cleartool-idle-timeout
        ah-cleartool-save-stop-data
        ah-clearcase-rmbranch-on-revert-flag
        ah-clearcase-checkout-comment-type
        ah-clearcase-checkout-policy
        ah-clearcase-confirm-label-move
        ah-clearcase-diff-format
        ah-clearcase-diff-cleanup-flag
        ah-clearcase-no-label-action

        ah-cleartool-next-command

        ,(cons 'ah-cleartool-last-command-timestamp
               (lambda (x buf)
                 (let ((name (symbol-name x))
                       (value (symbol-value x)))
                   (insert name " " (format "%.3f" value)
                           "; " (format "%.3f" (- (float-time) value))
                           " seconds ago")
                   (lisp-indent-line)
                   (insert "\n"))))

        ah-cleartool-ctid
        ah-cleartool-ntid
        ah-cleartool-terr
        ah-cleartool-tresults)

     (lambda ()
       (insert "\n\nClearCase version:\n==================\n\n"
               (ah-clearcase-version))
       (insert "\n\nContents of *cleartool-aborts*:\n"
               "===============================\n\n")
       (if ah-cleartool-save-stop-data
           (ignore-errors (insert-buffer-substring
                           (get-buffer "*cleartool-aborts*")))
           (insert "ah-cleartool-save-stop-data is nil, "
                   "*cleartool-aborts* is not available"))
       (insert "\n")
       (insert "\n\nContents of *cleartool-tq-trace*:\n"
               "=================================\n\n")
       (let ((buf (get-buffer "*cleartool-tq-trace*")))
         (if buf
             (insert-buffer-substring buf)
             (insert "buffer *cleartool-tq-trace* is not available")))
       (insert "\n")))))

(defvar ah-clearcase-function-to-trace
  '(ah-cleartool-ask ah-cleartool-wait-for ah-cleartool-tq-handler)
  "List of function to trace.
See `ah-clearcase-trace-cleartool-tq' and
`ah-clearcase-untrace-cleartool-tq' ")

(defun ah-clearcase-trace-cleartool-tq ()
  "Trace some of the cleartool commands."
  (interactive)
  (let ((trace-buf (get-buffer-create "*cleartool-tq-trace*")))
    (dolist (f ah-clearcase-function-to-trace)
      (trace-function-background f trace-buf))))

(defun ah-clearcase-untrace-cleartool-tq ()
  "Disable tracing of cleartool commands."
  (interactive)
  (dolist (f ah-clearcase-function-to-trace)
    (untrace-function f)))

;;;; Finish up

;;;###autoload
(if (boundp 'vc-handled-backends)
    (unless (memq 'CLEARCASE vc-handled-backends)
      (setq vc-handled-backends (nconc vc-handled-backends '(CLEARCASE))))
    (setq vc-handled-backends '(RCS CVS CLEARCASE)))

;; start cleartool here, we will need it anyway
(ah-cleartool-tq-maybe-start)

(provide 'vc-clearcase)

;;; Local Variables:
;;; mode: emacs-lisp
;;; mode: outline-minor
;;; outline-regexp: ";;;;+"
;;; End:

;;; vc-clearcase.el ends here
