;; vc-clearcase.el --- support for ClearCase version control system

;; Author: Alexandru Harsanyi (harsanyi@bigpond.com)
;; Created: 28 July 2004
;; Keywords: version-control, clearcase
;; $Id$

;;{{{ Commentary

;;; Commentary:
;;
;; vc-clearcase.el is a ClearCase integration package that works as a
;; client to the Emacs VC package.  In adition to the standard VC
;; functionality, this package also allows you to update static views,
;; edit a view's configspec, list checkouts and start the version tree
;; browser GUI on a file.  Once the package is loaded, these
;; additional commands will show up in the Tools/Version Control menu.
;;
;; To use it, simply put this file somewhere in your load-path (maybe
;; byte-compile it) and add the string "CLEARCASE" to the
;; 'vc-handled-backends' variable (preferably via customize).  Since
;; cleartool is slow to start, you might want to load the package when
;; emacs starts up.  To do that, simply add a (require 'vc-clearcase)
;; in your .emacs.el file.
;;
;;; Implementation notes:
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
;; by VC) and the aditional functions.
;;
;; the 'ah' prefix stands for Alex Harsanyi and is used to avoid
;; conflicts with the clearcase.el package.
;;
;; In adition, two macros are defined: with-checkedout-dir and
;; with-comment-file, if you compile this file they will not polute
;; the Emacs namespace.
;;
;;
;; ClearCase is slow.  Two techniques are used to improve the
;; responsiveness of Emacs when doing version-control opreations.
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
;;
;;; Todo:
;;
;; - allow expanding of revision strings to avoid the need to type a
;; full path.  The ideea is: if revision is a number, it should expand
;; to the file's base + that number, if the revision is name/number we
;; should search for 'name' as a branch and use that revision
;; (involves finding out about all the branches), and if the revision
;; starts with a '/' just leave it in place.
;;
;; - reformat the annotate buffer to print a meaningfull substring of
;; the version for long version strings.  Provide a keymapping to
;; display the version at the line.
;;
;; - provide filter functions for the modeline string to reduce its
;; length.  For example, VTK_Iteration12_patch should be reduced to
;; VTK_I~12_pat.  This should be configurable, as other sites will
;; have different needs.  (This is implemented, but it applies a fixed
;; set of replacements).
;;
;; - update vc-clearcase-merge to only add merge hyperlinks instead of
;; doing actual merges when the prefix arg is specified.
;;
;;

;;; Known bugs:
;;
;; When trying to merge revisions (vc-merge) on a file that is not
;; checked-out, vc asks for a checkout, but that the comment window
;; pops up, and vc-merge assumes the file was already checked out.  We
;; need to do an automatic checkout in this case, but how do we detect
;; it?
;;
;; On solaris, when cleartool starts up it prints out the prompt,
;; causing an error from tq.  The error can be safely ignored, but it
;; is annoying.

;;}}}

;;; History:
;;

;;; Code:

;;{{{ Initial requires and setup

(require 'tq)
(require 'vc-hooks)
(require 'vc)
;; we ask for this at runtime because we use find-if, remove-if-not,
;; etc
(require 'cl)

(defvar ah-clearcase-tmpdir
  (or (getenv "TEMP") "/tmp")
  "The location of the temporary directory.")

(defvar ah-clearcase-vtree-program
  (if (eq system-type 'windows-nt)
      "clearvtree"
    "xlsvtree")
  "The Vtree browser program.")

;;}}}

;;{{{ Cleartool transaction queue interface

(defvar ah-cleartool-tq nil
  "The transaction queue to cleartool.")

(defvar ah-cleartool-next-command 1
  "Command counter for cleartool commands.

Used to track if someone else is sending commands to cleartool, or if
two commands were sent in one go (eg \"cd\nls\n\")")

(defconst ah-cleartool-status-rx
  (concat "Command \\([0-9]+\\) returned status \\([0-9]+\\)[ \t\r\n]+"
          ;; under Windows NT, we communicate with cleartool using
          ;; pipes (instead of PTY's, and cleartool won't output a
          ;; prompt...
          (unless (eq system-type 'windows-nt)
            "cleartool \\([0-9]+\\)+>[ \t\r\n]+"))
  "Regexp to match the end of each cleartool result.

If it does not match properly, tq will neved pass back the answer to
us." )

(defvar ah-cleartool-timeout 20
  "Timeout (in seconds) for cleartool commands.")

(defvar ah-cleartool-ctid 0
  "The ID of the last completed transaction.

 This is an incrementing number, any transaction ID that is less than
this value is considered completed.")

(defvar ah-cleartool-ntid 1
  "The next transaction id.

Whenever 'ah-cleartool-ask' enqueues a transaction, it increments this
value.")

(defvar ah-cleartool-terr nil
  "Assoc list of (tid . error-message).

Transactions that have errors will have their tid's and error messages
stored in this list.  'ah-cleartool-wait-for' will check this list and
signal an error with the error message." )

(defvar ah-cleartool-tresults nil
  "Assoc list of (tid . answer).

Transactions that don't have a callback function attached, will have
their answer stored here for retrieval by 'ah-cleartool-wait-for'.")

(defun ah-cleartool-tq-sentinel (process event)
  "Sentinel for the cleartool command.

Cleans up properly if cleartool exits."
  (let ((status  (process-status process))
        (exit-status (process-exit-status process))
        (pbuffer (process-buffer process)))
    (when (memq status '(signal exit))
      (if (null (buffer-name pbuffer))
          (error "Cleartool process buffer was killed")
        (ah-cleartool-tq-stop)
        (kill-buffer pbuffer)))))

(defun ah-cleartool-tq-start ()
  "Start the transaction queue to cleartool."
  (let ((process
         (start-process "cleartool" " *cleartool*" "cleartool" "-status")))
    (set-process-sentinel process #'ah-cleartool-tq-sentinel)
    (process-kill-without-query process nil)
    (setq ah-cleartool-tq (tq-create process)
          ah-cleartool-next-command 1)))

(defun ah-cleartool-tq-stop ()
  "Stop the transaction queue to cleartool and kill cleartool."
  (when ah-cleartool-tq
    (tq-close ah-cleartool-tq)
    ;; (kill-buffer (process-buffer (tq-process ah-cleartool-tq)))
    ;; (kill-buffer (tq-buffer ah-cleartool-tq))
    (setq ah-cleartool-next-command 0)
    (setq ah-cleartool-tq nil)

    ;; mark all pending transactions as aborted
    (while (< ah-cleartool-ctid (1- ah-cleartool-ntid))
      (incf ah-cleartool-ctid)
      (push (cons ah-cleartool-ctid "cleatool command was aborted")
            ah-cleartool-terr))))

(defsubst ah-cleartool-tq-maybe-start ()
  "Start the transaction queue to cleartool, if not already started."
  (unless ah-cleartool-tq
    (ah-cleartool-tq-start))
  ah-cleartool-tq)

(defun ah-cleartool-tq-handler (closure answer)
  "Handle responses from cleartool-tq.

CLOSURE the closure that was enqueued with `ah-cleartool-ask', it is a
vector containing the transaction id plus the closure and function
that were passed to `ah-cleartool-ask' (the last two might be null).

ANSWER is the string that was received from cleartool.

The function checks the command index and status received from
cleartool, updates the completed transaction id ('ah-cleartool-ctid')
and either stores the answer in `ah-cleartool-terr' or
`ah-cleartool-tresults' for later retrieval by `ah-cleartool-wait-for', or
calls the function callback with the answer."

  (save-match-data
    (if (string-match ah-cleartool-status-rx answer)
        (let ((cmd (string-to-int (match-string 1 answer)))
              (status (string-to-int (match-string 2 answer))))
          (unless (eq ah-cleartool-next-command cmd)
            ;; transaction queue is out of sync, stop it
            (ah-cleartool-tq-stop)
            (error "Unexpected command index received"))
          ;; it's the command we're expecting
          (incf ah-cleartool-next-command)
          (let ((result (replace-match "" t t answer))
                (tid (aref closure 0))
                (cb-closure (aref closure 1))
                (cb (aref closure 2)))
            (setq ah-cleartool-ctid tid) ; assume tid's always grow
            (cond ((> status 0)
                   (push (cons tid result) ah-cleartool-terr))
                  (cb                ; do we have a callback function?
                   (funcall cb cb-closure result))
                  (t
                   (push (cons tid result) ah-cleartool-tresults)))))
      (error "Ah-cleartool-tq-handler: answer does not have a status"))))

(defun ah-cleartool-wait-for (tid &optional timeout)
  "Wait for TID to be completed, but no more than TIMEOUT seconds.

If timeout is nil, wait 'ah-cleartool-to' seconds.  If transaction-id
has completed, searches 'ah-cleartool-terr' for an error message
associated with that transaction, and if found, signals an error.
Otherwise looks in 'ah-cleartool-tresults' for a result for the
transaction and returns that.  Else returns t.

NOTE: a succesfull transaction might not have a result associated, as
'ah-cleartool-tq-handler' passes the result to the callback function
if that is available."
  (when tid
    (let ((start (float-time))
          (to (if timeout timeout ah-cleartool-timeout)))
      ;; busy wait for our transaction to complete
      (while (and (< ah-cleartool-ctid tid)
                  (< (- (float-time) start) to))
        (sit-for 0.1)))
    (if (>= ah-cleartool-ctid tid)
        (let ((err (assq tid ah-cleartool-terr))
              (result (assq tid ah-cleartool-tresults)))
          (when err
            (setq ah-cleartool-terr (assq-delete-all tid ah-cleartool-terr)))
          (when result
            (setq ah-cleartool-tresults
                  (assq-delete-all tid ah-cleartool-tresults)))
          (if err (error (cdr err)) (if result (cdr result) t)))
      ;; Restart cleartool on a timeout, as we might loose
      ;; synchronisation with the old one.
      (ah-cleartool-tq-stop)
      (ah-cleartool-tq-start)
      (error "Cleartool timed out"))))


(defun ah-cleartool-ask (question &optional wait closure fn)
  "Enqueue QUESTION to the cleartool-tq.

If WAIT is different than 'nowait, the transaction is waited for with
'ah-cleartool-wait-for' and returns whatever 'ah-cleartool-wait-for'
returns.  Otherwise the the transaction id is returned (you will have
to wait for it yourself).  If CLOSURE and FN are specified, fn will be
called when the transaction is complete as funcall(fn closure
answer)."
  (ah-cleartool-tq-maybe-start)
  (let ((tid ah-cleartool-ntid)
        (command (concat question "\n")))
    (incf ah-cleartool-ntid)
    (tq-enqueue ah-cleartool-tq command ah-cleartool-status-rx
                (vector tid closure fn) #'ah-cleartool-tq-handler)
    (if (eq wait 'nowait)
        tid
      (ah-cleartool-wait-for tid))))

;;}}}

;;{{{ Cleartool subprocess interface

(defvar ah-cleartool-mode-line nil
  "Modeline argument for cleartool commands.")

(defvar ah-cleartool-finished-function nil
  "Function to be called when the cleartool proces finishes.")

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
  "Process sentinel for cleartool subprocess commands.

Updates the modeline when the cleartool command finishes, calls
'cleartool-finished-callback' and kills the process buffer when
'cleartool-kill-buffer-when-done' is set."
  (let ((status  (process-status process))
        (exit-status (process-exit-status process))
        (pbuffer (process-buffer process)))
    (when (memq status '(signal exit))
      (if (null (buffer-name pbuffer))
          (error "Cleartool process buffer was killed")
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

Command is a cleartool command, that is the actuall command run is
\"cleartool cmd args\".

The arguments need to be a list of strings as in execv(2) call.  This
is different from the arguments to 'ah-cleartool-ask'.

This command only starts the process and returns it.  The process will
continue to run and fill buffer.  If you want to be notified when the
process is finished, setup a callback function in
'ah-cleartool-finished-function' (see below.)

The sentinel for the resulting process inspects the following buffer
local variables in the proces buffer:

'ah-cleartool-finished-function' -- function to call when the
cleartool command has finished.

'ah-cleartool-kill-buffer-when-done' -- when t, the buffer will be
killed when the cleartool command has finished.

In adition, the buffer local variable 'ah-cleartool-last-command' is
set to the command and arguments that were run to create this buffer."
  (let ((name (format "cleartool-%s" cmd))
        (args1 (cons cmd args)))
    (let ((process (apply 'start-process name buffer "cleartool" args1)))
      (with-current-buffer (process-buffer process)
        (set-process-sentinel process 'ah-cleartool-sentinel)
        (setq ah-cleartool-mode-line "run")
        (setq mode-line-process '(" cleartool: " ah-cleartool-mode-line))
        (force-mode-line-update)
        (setq ah-cleartool-last-command (copy-sequence args1))
        process))))

;;}}}

;;{{{ Clearcase Log View mode

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
  (if (re-search-backward ah-clearcase-record-separator-rx
                          (point-min) 'noerror)
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

This method assumes that the record fields look like: 'field-name:
value' and returns 'value'.  If the field is not found, nil is
returned."
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
    (ah-cleartool-do (car ah-cleartool-last-command)
                     (cdr ah-cleartool-last-command)
                     (current-buffer))))

(defun ah-clearcase-log-view-forward-record (num-records)
  "Move forward NUM-RECORDS, if negative, move backward.

Return the number of records actually moved."
  (interactive "p")
  (multiple-value-bind (search-fn limit adjust)
      (if (>= num-records 0)
          (list #'re-search-forward (point-max) 1)
        (list #'re-search-backward (point-min) -1))
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

(define-derived-mode ah-clearcase-log-view-mode fundamental-mode "Cc-Log-View"
  "Generic mode to view clearcase log listings."
  ;; this gets reset when we swith modes
  (make-local-variable 'font-lock-defaults)
  (setq case-fold-search nil)
  (setq font-lock-defaults '(ah-clearcase-log-view-font-lock-keywords nil nil))
  (font-lock-mode t)
  (setq buffer-read-only t))

(easy-mmode-defmap ah-clearcase-log-view-mode-map
                   '(("n" . ah-clearcase-log-view-forward-record)
                     ("p" . ah-clearcase-log-view-backward-record)
                     ("\M-a" . ah-clearcase-log-view-bor)
                     ("\M-e" . ah-clearcase-log-view-eor)
                     ("g" . ah-clearcase-log-view-again)
                     ("f" . ah-clearcase-log-view-visit-file)
                     ("w" . ah-clearcase-log-view-wash-log))
                   "Mode map for Clearcase Log View mode")

;;}}}

;;{{{ Clearcase file properties

;; Rather than keeping all the version information as poperties
;; accessible via vc-file-{get/set}prop, we define a structure to hold
;; all the information and store it in as the 'vc-clearcase-prop
;; property of a file.

(defstruct (ah-clearcase-fprop
            (:constructor ah-clearcase-make-fprop)
            (:copier ah-clearcase-copy-fprop))

  version-tid
  version
  parent
  latest
  latest-sel                            ; latest selector (LATEST)
  checkout                    ; nil, 'reserved, 'unreserved, 'hijacked
  mode-line
  base
  branch
  what-rule

  comment-tid
  comment

  view-tag
  )

(defsubst ah-clearcase-fprop-file (file)
  "Return the fprop structure associated with FILE."
  (vc-file-getprop file 'vc-clearcase-fprop))

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

(defun ah-clearcase-fprop-reset (fprop)
  "Clear the version fields in FPROP.

This will mark fprop as not initialized for the functions that care
about this.  This function accepts a nil fprop (in which case it does
nothing), to the user can reset a file's fprop without having to check
first that it exists."
  (when fprop
    (setf (ah-clearcase-fprop-version fprop) nil)
    (setf (ah-clearcase-fprop-latest fprop) nil)
    (setf (ah-clearcase-fprop-version-tid fprop) nil)))

(defsubst ah-clearcase-fprop-hijacked-p (fprop)
  "Return true if FPROP is hijacked."
  (eq (ah-clearcase-fprop-checkout fprop) 'hijacked))

(defsubst ah-clearcase-fprop-checkedout-p (fprop)
  "Return the checked out mode for FPROP or nil."
  (memq (ah-clearcase-fprop-checkout fprop) '(reserved unreserved)))


(defun ah-clearcase-wash-mode-line (mode-line)
  "Make the modeline string shorter by replacing some of the words in
it with shorter versions.  This is probably specific to my site, so it
should be made configurable..."
  (replace-regexp-in-string
   "\\<release\\([0-9]*\\)\\>" "rel\\1"
   (replace-regexp-in-string
    "\\<branch\\([0-9]*\\)\\>" "br\\1"
    (replace-regexp-in-string
     "\\<patch\\([0-9]*\\)\\>" "pat\\1"
     (replace-regexp-in-string
      "iteration\\([0-9]+\\)" "I~\\1" mode-line)))))


(defun ah-clearcase-fprop-set-version (fprop version-string)
  "Set the version information in FPROP from VERSION-STRING.

Version string is returned by a 'cleartool desc -fmt \"%Sn %PSn %Rf\"
file' command."
  (let ((fver-raw (split-string version-string)))
    (let ((fver (nth 0 fver-raw))       ; file version
          (pver (nth 1 fver-raw))       ; parent version
          (co-mode (let ((co-mode-raw (nth 2 fver-raw)))
                     (cond ((eq co-mode-raw nil) nil)
                           ((string= co-mode-raw "reserved") 'reserved)
                           ((string= co-mode-raw "unreserved") 'unreserved)
                           (t 'unknown)))))
      (when co-mode
        ;; The semantics of vc.el requies that the workfile version be
        ;; the parent version if the file is checked out.
        (setq fver pver))
      (let ((base (ah-clearcase-version-base fver))
            (branch (ah-clearcase-version-branch fver))
            (version (ah-clearcase-version-version fver)))

        (setf (ah-clearcase-fprop-version fprop) fver)
        (setf (ah-clearcase-fprop-parent fprop) pver)
        ;; a hijacked file should keep its existing checkout status
        ;; and modeline (set by ah-clearcase-fprop-set-version-simple)
        (unless (ah-clearcase-fprop-hijacked-p fprop)
          (setf (ah-clearcase-fprop-checkout fprop) co-mode)
          (setf (ah-clearcase-fprop-mode-line fprop)
                (ah-clearcase-wash-mode-line
                (concat "Cc:"
                        (cond ((eq co-mode 'reserved) "(R)")
                              ((eq co-mode 'unreserved) "(U)")
                              (t ""))
                         ;; ".../"
                         branch "/" version))))
        (setf (ah-clearcase-fprop-base fprop) base)
        (setf (ah-clearcase-fprop-branch fprop) branch)))))

(defun ah-clearcase-fprop-set-version-simple (fprop ls-string)
  "Set version information in FPROP from LS-STRING.

Ls-string is returned by a 'cleartool ls file' command."
  (when (string-match "Rule: \\(.*\\)$" ls-string)
    (setf (ah-clearcase-fprop-what-rule fprop) (match-string 1 ls-string)))
  (when (string-match "@@\\([^ \t]+\\)" ls-string)
    (let* ((fver (match-string 1 ls-string))
           (base (ah-clearcase-version-base fver))
           (branch (ah-clearcase-version-branch fver)))
      (setf (ah-clearcase-fprop-version fprop) fver)

      ;; do we need to set these here?
      (setf (ah-clearcase-fprop-base fprop) base)
      (setf (ah-clearcase-fprop-branch fprop) branch)))
  (when (string-match "hijacked" ls-string)
    (setf (ah-clearcase-fprop-checkout fprop) 'hijacked)
    (setf (ah-clearcase-fprop-mode-line fprop) "Cc:HIJACKED")))

;;}}}

;;{{{ Clearcase view-tag properties

(defstruct (ah-clearcase-vprop
            (:constructor ah-clearcase-make-vprop)
            (:copier ah-clearcase-copy-vprop))
  name
  root                                  ; for snapshot views only
  type                                  ; (nil 'snapshot 'dynamic)
  )

(defvar ah-clearcase-all-vprops
  (make-hash-table :test 'equal))

(defun ah-clearcase-vprop-prepare (file fprop)
  "Find the view in which FILE resides and populate it.

FPROP is used to get the view name.  If the view is not known, create
a new vprop for it."
  ;; first, we switch the current directory in cleartool, as it is the
  ;; only way to get the current view and its root directory
  (ah-cleartool-ask (format "cd \"%s\"" (file-name-directory file)))
  (ah-cleartool-ask "pwv -short" 'wait fprop
              '(lambda (fprop view-tag)
                 (setf (ah-clearcase-fprop-view-tag fprop)
                             (replace-regexp-in-string "[\n\r]+" "" view-tag))))
  (let ((vprop (ah-clearcase-vprop-get (ah-clearcase-fprop-view-tag fprop))))
    (unless (ah-clearcase-vprop-type vprop)
      ;; This is the first time we see this view, colect some info on it
      (let ((lsview (ah-cleartool-ask
                  (format "lsview %s" (ah-clearcase-vprop-name vprop)))))
    (setf (ah-clearcase-vprop-type vprop)
              ;; the first char in a lsview listing is '*'.  At least
              ;; at our site...  I think the '*' means the view is
              ;; started, but that's the same thing for us... (since
              ;; we are visiting a file in this view, it must be
              ;; started)
          (if (eq ?* (aref lsview 0)) 'dynamic 'snapshot)))
      (when (ah-clearcase-snapshot-view-p vprop)
  (ah-cleartool-ask
         "pwv -root" 'nowait vprop
         '(lambda (vprop root-dir)
        (let ((root (replace-regexp-in-string "[\n\r]+" "" root-dir)))
              (setf (ah-clearcase-vprop-root vprop) root))))))))

(defun ah-clearcase-vprop-get (view-tag)
  "Return the vprop struct associated with VIEW-TAG.

If no such structure exists, a new, empty, one is created and
returned."
  (let ((vprop (gethash view-tag ah-clearcase-all-vprops)))
    (unless vprop
      (setq vprop (ah-clearcase-make-vprop :name view-tag))
      (puthash view-tag vprop ah-clearcase-all-vprops))
    vprop))

(defun ah-clearcase-declare-view (view-tag type &optional root)
  "Declare a VIEW as a view of TYPE 'snapshot or 'dynamic.

For a snapshot view, the view's ROOT also has to be specified.

This function can be used to pre-declare views so that
ah-clearcase-setcs will not complain that you have to visit a file in
that view first."
  (unless (memq type '(snapshot dynamic))
    (error "Bad view type"))
  (when (eq type 'snapshot)
    (unless (and (stringp root) (file-directory-p root))
      (error "Bad view root")))
  (let ((vprop (ah-clearcase-vprop-get view-tag)))
    (setf (ah-clearcase-vprop-type vprop) type)
    (setf (ah-clearcase-vprop-root vprop) root)))

(defun ah-clearcase-snapshot-view-p (view)
  "Return t if VIEW is a snapshot view.

View can be either a view name (a string) or a vprop"
  (let ((vprop (if (ah-clearcase-vprop-p view) view
                 (ah-clearcase-vprop-get view))))
    (eq (ah-clearcase-vprop-type vprop) 'snapshot)))

(defun ah-clearcase-dynamic-view-p (view)
  "Return t if VIEW is a dynamic  view.

View can be either a view name (a string) or a vprop"
  (let ((vprop (if (ah-clearcase-vprop-p view) view
                 (ah-clearcase-vprop-get view))))
    (eq (ah-clearcase-vprop-type vprop) 'dynamic)))

(defun ah-clearcase-refresh-files-in-view (view)
  "Update all visited files from VIEW.

This is usefull when the view changes (by a setcs or update
command).  VIEW can be either a view-tag name or a vprop."
  (when (ah-clearcase-vprop-p view)
    (setq view (ah-clearcase-vprop-name view)))
  (dolist (buffer (buffer-list))
    (ignore-errors
      ;; ignore modified buffers, don't rob the user from the joy of
      ;; figuring out that he just changed the view and he had
      ;; modified files in it...
      (unless (buffer-modified-p buffer)
        (let* ((file (buffer-file-name buffer))
               (fprop (ah-clearcase-fprop-file file))
               (vtag (ah-clearcase-fprop-view-tag fprop)))
          (when (string= vtag view)
            (ah-clearcase-maybe-set-vc-state file 'force)
            (vc-resynch-buffer file t t)))))))

;;}}}

;;{{{ Vc interface + some helpers

(defun ah-clearcase-version-branch (version)
  "Return the branch part of  VERSION.

 (second last element in version path)."
  (nth 1 (nreverse (split-string version "[\\\\/]"))))

(defun ah-clearcase-version-base (version)
  "Return the VERSION minus the last element."
  (let ((v (copy-sequence version)))
    (when (string-match "[\\\\/][^\\\\/]*$" v)
      (replace-match "" t t v))))

(defun ah-clearcase-version-version (version)
  "Return the VERSION (last element in version path)."
  (when (string-match "[\\\\/]\\([^\\\\/]*\\)$" version)
    (match-string 1 version)))

(defun ah-clearcase-maybe-set-vc-state (file &optional force)
  "Lazily set the clearcase specific properties of FILE.

If FORCE is not nil, always read the properties."
  (let ((fprop (ah-clearcase-fprop-file file)))
    (when force (ah-clearcase-fprop-reset fprop))
    (when (or (ah-clearcase-fprop-initialized-p fprop)
              (vc-clearcase-registered file))
      (unless fprop (setq fprop (ah-clearcase-fprop-file file)))
      (ah-cleartool-wait-for (ah-clearcase-fprop-version-tid fprop))
      (unless (ah-clearcase-fprop-latest fprop)
        (let ((latest-sel
               (format "%s/LATEST" (ah-clearcase-fprop-base fprop))))
          (setf (ah-clearcase-fprop-latest-sel fprop) latest-sel)
          (setf (ah-clearcase-fprop-latest fprop)
                (ah-cleartool-ask
                 (format "desc -fmt \"%%Sn\" \"%s@@%s\"" file latest-sel)))
          ;; finally prepare a vprop structure for the file's view,
          ;; but only if we haven't already done so.  NOTE: we don't
          ;; expect the view of the file to ever change, so we ignore
          ;; the 'force option.
          (unless (ah-clearcase-fprop-view-tag fprop)
            (ah-clearcase-vprop-prepare file fprop)))))))


(eval-when-compile

  ;; If you compile this file, these macros won't exist at runtime, so
  ;; it is safe to give them nice names.

(defmacro with-checkedout-dir (dir &rest forms)
  "Ensure that DIR is checked out, than execute FORMS.

If DIR was checked out by us, check it back in."

  ;; NOTE: we could use make-symbol with the same effect
  (let ((checkout-needed-flag (gensym))
        (real-dir (gensym)))
    `(let* ((,real-dir ,dir)
            (,checkout-needed-flag
             (string=
              (ah-cleartool-ask
               (format "desc -fmt \"%%Rf\" \"%s\"" ,real-dir))
              "")))
       (unwind-protect
           (progn
             (when ,checkout-needed-flag
               (message "Checking out %s" ,real-dir)
               (ah-cleartool-ask
                (format "checkout -reserved -nc \"%s\"" ,real-dir)))
             ,@forms)
         (when ,checkout-needed-flag
           (message "Checking in %s" ,real-dir)
           (ah-cleartool-ask (format "checkin -nc \"%s\"" ,real-dir)))))))

(defmacro with-comment-file (comment-text &rest forms)
  "Save COMMENT-TEXT in a temporary file, than execute FORMS.

Binds the name of the temporary file to the variable COMMENT-FILE.
When alll is finished, COMMENT-FILE is removed."
  `(let ((comment-file 
          (make-temp-name (concat ah-clearcase-tmpdir "/clearcase-")))
         (comment-text ,comment-text))
     (unwind-protect
         (progn
           (with-current-buffer (find-file-noselect comment-file)
             (erase-buffer)           ; do we need this?
             (insert comment-text)
             (save-buffer)
             (kill-buffer (current-buffer)))
           ,@forms)
       (delete-file comment-file))))

)                                       ; eval-when-compile

(put 'with-checkedout-dir 'lisp-indent-function 1)
(put 'with-comment-file 'lisp-indent-function 1)



(defadvice vc-version-backup-file-name
  (before ah-clearcase-cleanup-version (file &optional rev manual regexp))
  "Cleanup rev of \\ and / so it can be stored as a filename."
  (when rev
    (setf rev (replace-regexp-in-string "[\\\\/]" "~" rev))))
(ad-activate 'vc-version-backup-file-name)

(defadvice vc-start-entry
  (before ah-clearcase-prepare-checkin-comment
          (file rev comment initial-contents msg action &optional after-hook))
  "Insert an initial comment when checking-in files."
  (let ((fprop (ah-clearcase-fprop-file file)))
    (when (and fprop (ah-clearcase-fprop-checkedout-p fprop))
      ;; so we are checking a file in
      (ah-cleartool-wait-for (ah-clearcase-fprop-comment-tid fprop))
      (setf comment (ah-clearcase-fprop-comment fprop))
      (setf initial-contents t))))
(ad-activate 'vc-start-entry)



(defun vc-clearcase-registered (file)
  "Return non nil if FILE is registered in ClearCase.

We consider it to be registered, if cleartool can tell us its
version."

  ;; if the file already has a version set, or we asked for it
  ;; already, return t
  (let ((fprop (ah-clearcase-fprop-file file)))
    (if (ah-clearcase-fprop-initialized-p fprop)
        t
      (condition-case nil
          (progn
            (unless fprop (setq fprop (ah-clearcase-make-fprop)))
            (let ((ls-result (ah-cleartool-ask (format "ls \"%s\"" file))))
              (if (string-match "Rule: \\(.*\\)$" ls-result)
                  ;; file is registered
                  (progn
                    (ah-clearcase-fprop-set-version-simple fprop ls-result)
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
                             (format "desc -fmt \"%%Sn %%PSn %%Rf\" \"%s\""
                                     pname)
                             'nowait fprop #'ah-clearcase-fprop-set-version))
                      (vc-file-setprop file 'vc-clearcase-fprop fprop))
                    t)                  ;file is registered
                nil)))                  ;file is not registered
        (error nil)))))


(defvar vc-clearcase-state-needs-update nil)

(defun vc-clearcase-state (file)
  "Return the current version control state of FILE.

How we map clearcase states to vc states:

'up-to-date -- file is not checked out and the current version is the
one selected by our config-spec.

'edited -- file is checked out by us and no newer version exists on
the branch.

\"USER\" -- this is never returned, we handle this by asking for an
unreserved checkout.

'needs-patch -- an update -print commad indicates that it would update
the file.

'needs-merge -- file is not the latest on our branch and we checked it
out.

'unlocked-change -- file is hijacked."
  ;; we are asked for a reliable computation of state, so refresh all
  ;; the properties.
  (ah-clearcase-maybe-set-vc-state file 'force)

  (let ((fprop (ah-clearcase-fprop-file file))
        (vc-clearcase-state-needs-update "")
        update-tid state)
    ;; let's see if this file needs updating (only on a snapshot
    ;; view).  We ask the question first, compute the state via
    ;; heuristic, than check the answer later.
    (when (ah-clearcase-snapshot-view-p (ah-clearcase-fprop-view-tag fprop))
      ;; NOTE: the update -print command writes a update log, which we
      ;; could check for a more reliable result.  For now, we just
      ;; disable it.
      (setq update-tid
            (ah-cleartool-ask
             (format "update -print -log NUL \"%s\"" file) 'nowait file
             #'(lambda (file update-log)
                 (setq vc-clearcase-state-needs-update update-log)))))

    (setq state (vc-clearcase-state-heuristic file))
    (ah-cleartool-wait-for update-tid)

    ;; We anticipate that the file's checkout comment might be needed
    ;; shortly so ask for it before we return the state
    (when (ah-clearcase-fprop-checkedout-p fprop)
      (setf (ah-clearcase-fprop-comment-tid fprop)
            (ah-cleartool-ask
             (format "desc -fmt \"%%c\" \"%s\"" file) 'nowait fprop
             #'(lambda (fprop comment)
                 (setf (ah-clearcase-fprop-comment fprop) comment)))))

    (if (eq state 'up-to-date)
        (if (string-match "^Loading " vc-clearcase-state-needs-update)
            'needs-patch
          'up-to-date)
      state)))


(defun vc-clearcase-state-heuristic (file)
  "Determine the state of FILE.

Use whatever 'ah-clearcase-maybe-set-vc-state' gave us.  This method
will only return 'edited, 'needs-merge or 'up-to-date.  If 'up-to-date
is returned, the file might still need a patch (needs-patch)."
  (ah-clearcase-maybe-set-vc-state file)
  (let ((fprop (ah-clearcase-fprop-file file)))
    (if (ah-clearcase-fprop-hijacked-p fprop)
        'unlocked-changes
      (if (ah-clearcase-fprop-checkedout-p fprop)
          (if (string= (ah-clearcase-fprop-latest fprop)
                       (ah-clearcase-fprop-parent fprop))
              'edited
            'needs-merge)
        'up-to-date))))


;; NOTE: this does not work corectly, as the 'need-update state is not
;; detected (we need to run a cleartool update and parse the resulting
;; file).  The vc's dir-state is also inefficient for clearcase, as it
;; calls this function for each sub-directory when we could get the
;; state more efficiently for the entire sub-directory tree.

(defun vc-clearcase-dir-state (dir)
  "Retrieve version state in DIR.  Fast."
  (when (string-match "\\(\\\\\\|/\\)$" dir)
    (setq dir (replace-match "" nil nil dir)))
  (when (vc-clearcase-registered dir)
    (ah-clearcase-maybe-set-vc-state dir)
    (message "Processing %s" dir)
    (let ((ls-result
           (split-string (ah-cleartool-ask (format "ls \"%s\"" dir))
                         "[\n\r]+"))
          (static-view?
           (ah-clearcase-snapshot-view-p
            (ah-clearcase-fprop-view-tag
             (ah-clearcase-fprop-file dir)))))
      (dolist (entry ls-result)
        (ignore-errors
          (when (string-match "Rule: \\(.*\\)$" entry)
            ;; looks like clearcase knows something about this file
            (when (string-match "^\\(.*\\)@@\\([^ \t]+\\)" entry)
              (let ((file (match-string 1 entry))
                    (revision (match-string 2 entry)))
                (if (string-match "hijacked" entry)
                    (vc-file-setprop file 'vc-state 'unlocked-changes)
                  (if (string-match "CHECKEDOUT$" revision)
                      (progn
                        (when (string-match "from \\([^ \t]+\\)" entry)
                          (let ((parent-revision (match-string 1 entry))
                                (latest-revision
                                 (ah-cleartool-ask
                                  (format
                                   "desc -fmt \"%%Sn\" \"%s@@%s/LATEST\""
                                   file
                                   (ah-clearcase-version-base revision)))))
                            (if (string= parent-revision latest-revision)
                                (vc-file-setprop file 'vc-state 'edited)
                              (vc-file-setprop file 'vc-state 'needs-merge)))))
                    ;; we need to check if the file needs update when
                    ;; in a snapshot view...
                    (vc-file-setprop file 'vc-state 'up-to-date)))))))))))

(defun vc-clearcase-workfile-version (file)
  "Return the workfile version of FILE.

If the file is checked out, In ClearCase, the version is always
\"CHECKEDOUT\", but the vc.el assumes that checked out is not a
separate version, so we return the parent version in that case."
  (ah-clearcase-maybe-set-vc-state file)
  (ah-clearcase-fprop-version (ah-clearcase-fprop-file file)))

(defun vc-clearcase-latest-on-branch-p (file)
  "Return true if FILE is the latest version on the branch."
  (ah-clearcase-maybe-set-vc-state file)
  (let ((fprop (ah-clearcase-fprop-file file)))
    (string= (ah-clearcase-fprop-version fprop)
             (ah-clearcase-fprop-latest fprop))))

(defun vc-clearcase-checkout-model (file)
  "Checkout model for ClearCase.

This is always locking, for every FILE."
  'locking)

(defun vc-clearcase-workfile-unchanged-p (file)
  "Is FILE unchangned?"
  (not (vc-clearcase-diff file)))

(defun vc-clearcase-mode-line-string (file)
  "Return the mode line string for FILE."
  (ah-clearcase-maybe-set-vc-state file)
  (let ((fprop (ah-clearcase-fprop-file file)))
    (let ((mode-line (ah-clearcase-fprop-mode-line fprop)))
      (if mode-line mode-line "Cc:"))))

(defun vc-clearcase-register (file &optional rev comment)
  "Register FILE with clearcase.  REV and COMMENT are ignored.

ClearCase rquires the directory in which file resides to be checked
out for the insertion to work.  If the directory is checked out, we
leave it checked out, otherwise we do a checkout for the file
insertion than a checkin.

NOTE: if dir is not under clearcase, this code will fail.  We don't
attempt to register a directory in clearcase even if one of it's
parents is registered."
  (with-checkedout-dir (file-name-directory file)
    (message "Registering %s" (file-name-nondirectory file))
    (ah-cleartool-ask (format "mkelem -nc \"%s\"" file))
    (ah-clearcase-maybe-set-vc-state file 'force)
    (vc-resynch-buffer file t t)))

(defun vc-clearcase-responsible-p (file)
  "Return t if we responsible for FILE.

We don't consider ourselves responsible if cleartool ls command
returns a 'pathname not within a VOB' error message."
  (if (file-exists-p file)
      (let ((case-fold-search t))
        (condition-case msg
            (if (ah-cleartool-ask (format "ls \"%s\"" file))
                t
              nil)                      ; never reached
          (error (if (string-match "Pathname is not within a VOB:" (cadr msg))
                     nil
                   t))))))

(defun vc-clearcase-checkin (file rev comment)
  "Checkin FILE with COMMENT.  REV is ignored."
  (when rev
    (message "Ignoring revision specification: %s" rev))
  (with-comment-file comment
    ;; let the cleartool error be directly reported
    (ah-cleartool-ask (format "checkin -cfile %s \"%s\"" comment-file file))
    (ah-clearcase-maybe-set-vc-state file 'force)))

(defun ah-clearcase-finish-checkout (file rev comment mode)
  "Finish a checkout started by 'vc-clearcase-checkout'.

FILE, REV and COMMENT are the same as the one from
`vc-clearcase-checkout', MODE selects the checkout mode and can be
'reserved or 'unreserved"
  (with-comment-file comment
    (let ((pname (if rev (concat file "@@" rev) file))
          (options (concat "-cfile " comment-file " " (when rev "-version ")))
          (co-mode (if (eq mode 'reserved) "-reserved " "-unreserved ")))
      ;; NOTE: if this fails, we should prompt the user to
      ;; checkout unreserved.
      (ah-cleartool-ask (concat "checkout " co-mode options " \"" pname "\"")))
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
  (let ((fprop (ah-clearcase-fprop-file file))
        (checkouts
         (split-string
          (ah-cleartool-ask
           (format "lsco -fmt \"%%PSn %%Rf %%Tf %%u\\n\" \"%s\"" file))
          "[\n\r]+")))
    (let* ((match (concat (ah-clearcase-fprop-version fprop) " reserved"))
           (len (length match))
           (rev (find-if (lambda (x)
                           (eq (compare-strings match 0 len x 0 len) t))
                         checkouts)))
      (if rev
          (let ((elements (split-string rev)))
            (cons (nth 3 elements) (nth 2 elements)))
        nil))))

;; TODO: We should accept several checkout modes:
;;
;; 1/ Do we want a checkout comment (-cfile) or not (-nc)
;;
;; 2/ Do we want to go exclusive reserved, exclusive unreserved or try
;; reserved than unreserved
;;
;; Since the VC code might call this function and assumes we won't
;; bother the user, only -nc -unreserved can provide that.
;;

(defun vc-clearcase-checkout (file &optional editable rev destfile)
  "Checkout FILE.

This method does three completely different things:

  1/ Checkout a version of the file.
  2/ Get a version of the file in a separate file
  3/ Update the file (in snapshot views)."
  (if editable
      ;; Checkout the file
      (progn
        (when destfile
          ;; Technically, we can use the -out option, but I'm not sure
          ;; of all its implications.
          (error "Cannot checkout to a specific file"))

        (let* ((fprop (ah-clearcase-fprop-file file))
               (checkout-mode
                (cond
                 ;; if the checkout will create a branch, checkout
                 ;; reserved
                 ((string-match 
                   "-mkbranch" (ah-clearcase-fprop-what-rule fprop))
                  'ah-clearcase-finish-checkout-reserved)
                
                 ;; if someone else has checked out this revision in
                 ;; reserved mode, ask the user if he wants an
                 ;; unreserved checkout.
                 ((let ((user-and-view
                         (ah-clearcase-revision-reserved-p file)))
                    (if user-and-view
            (if (yes-or-no-p
                 (format
                  "This revision is checked out reserved by %s in %s.  %s"
                  (car user-and-view) (cdr user-and-view)
                  "Checkout unreserved? "))
                            'ah-clearcase-finish-checkout-unreserved
                          ;; will abort the checkout
                          nil)
                      ;; no one has this version checked out, checkout
                      ;; reserved.
                      'ah-clearcase-finish-checkout-reserved))))))
          (if checkout-mode
              (vc-start-entry 
               file rev nil nil "Enter a checkout comment" checkout-mode)
            (message "Aborted."))))

    ;; This will go in vc-clearcase-find-version when the next emacs
    ;; version comes out.
    (if destfile
        ;; Check out an arbitrary version to the specified file
        (progn
          (unless rev
            (setq rev (ah-clearcase-fprop-latest-sel
                       (ah-clearcase-fprop-file file))))
          (when (string= rev "")
            (error "Refusing to checkout head of trunk"))
          (ah-cleartool-ask
           (format "get -to \"%s\" \"%s@@%s\"" destfile file rev)))

      (progn
        ;; We cannot update to a specific revision, the user should
        ;; edit the config spec.
        (when rev
          (error "Cannot to update to a specific revision"))
        ;; Update to the configspec
        (let ((update-result
               (ah-cleartool-ask (format "update -rename \"%s\"" file))))
          (when (string-match
                 "^Update log has been written to .*$" update-result)
            (message (match-string 0 update-result))))))))

(defun vc-clearcase-revert (file &optional contents-done)
  "Cancel a checkout on FILE."
  (ah-cleartool-ask (format "uncheckout -keep \"%s\"" file))
  (ah-clearcase-maybe-set-vc-state file 'force))


(defun vc-clearcase-merge (file rev1 rev2)
  "Merge into FILE REV1 up to REV2.

NOTE: when trying to merge revisions (vc-merge) on a file that is not
checked-out, vc asks for a checkout, but that the comment window pops
up, and vc-merge assumes the file was already checked out.  We need to
do an automatic checkout in this case, but how do we detect it?"
  (let ((merge-status
         (ah-cleartool-ask
          (format "merge -abort -insert -to \"%s\" -ver %s %s"
                  file rev1 rev2))))
    (with-current-buffer (get-buffer-create "*vc-merge-result*")
      (let ((inhibit-read-only t))
        (insert merge-status)
        (switch-to-buffer-other-window (current-buffer) 'norecord)
        (shrink-window-if-larger-than-buffer)))))

(defun vc-clearcase-merge-news (file)
  "Merge the new changes in FILE."
  (let ((latest (concat file "@@"
                        (ah-clearcase-fprop-latest-sel
                         (ah-clearcase-fprop-file file)))))
    (message "Merging LATEST into this version")
    ;; NOTE: we abort if anything goes wrong with the merge.
    (let ((merge-status
           (ah-cleartool-ask (format "merge -abort -to \"%s\" \"%s\""
                                     file latest))))
      (with-current-buffer (get-buffer-create "*vc-merge-result*")
        (let ((inhibit-read-only t))
          (insert merge-status)
          (switch-to-buffer-other-window (current-buffer) 'norecord)
          (shrink-window-if-larger-than-buffer))))))

;;;; steal-lock -- if we have a hijacked file, check-it out and use
;;;; the contents of the hijacked file as the checked out contents

(defvar ah-clearcase-file-name nil
  "File name for which this log was generated.")

(make-variable-buffer-local 'ah-clearcase-file-name)
(put 'ah-clearcase-file-name 'permanent-local t)

(defun vc-clearcase-print-log (file)
  "Insert the history of FILE into the *clearcase-lshistory* buffer.

With a prefix argument, all events are listed (-minor option is sent
to cleartool).

This is not intended to be called directly from the vc.el.  Instead,
`vc-print-log' is advised to call this function directly for Clearcase
registered files."
  (with-current-buffer (get-buffer-create "*clearcase-lshistory*")
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
              #'(lambda ()
                  (let ((fprop
                         (ah-clearcase-fprop-file ah-clearcase-file-name)))
                    (vc-clearcase-show-log-entry
                     (ah-clearcase-fprop-version fprop)))))
        (switch-to-buffer-other-window (process-buffer process))))))

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
'ah-clearcase-lshistory-fmt'."
  (let ((regexp
         (concat "^version: "
                 (replace-regexp-in-string "[\\\\/]" "[\\\\/]" version)))
        pos)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward regexp (point-max) 'noerror)
        (setq pos (match-beginning 0))))
    (when pos (goto-char pos))))

;;;; wash-log
;;;; logentry-ckeck
;;;; comment-history
;;;; update-changelog

(defvar ah-clearcase-diff-format 'diff
  "Type of diff to be output by the cleartool diff command.  Can be
'diff or 'serial")

(defvar ah-clearcase-diff-cleanup-flag t
  "Default remove ^M characters from the diff output")

(defun vc-clearcase-diff (file &optional rev1 rev2)
  "Return the diff on FILE between REV1 and REV2."
  (let ((fprop (ah-clearcase-fprop-file file)))
    (when (not rev1)
      (setq rev1 (ah-clearcase-fprop-version fprop)))
    (ah-cleartool-ask (format "cd \"%s\"" (file-name-directory file)))
    (setq file (file-name-nondirectory file))
    (let ((fver1 (concat file "@@" rev1))
          (fver2 (if rev2 (concat file "@@" rev2) file)))
      (with-current-buffer (get-buffer-create "*vc-diff*")
        (message "Comparing file revisions...")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (let* ((diff-format (cond ((eq ah-clearcase-diff-format 'diff)
                                     "-diff_format")
                                    ((eq ah-clearcase-diff-format 'serial)
                                     "-serial_format")
                                    (t (error "Unknown diff format"))))
                 (diff (ah-cleartool-ask
                        (format "diff %s \"%s\" \"%s\""
                                diff-format fver1 fver2))))
            (insert diff)
            (when ah-clearcase-diff-cleanup-flag
              (replace-regexp "\r$" "" nil (point-min) (point-max)))
            (goto-char (point-min))

            (not
             ;; the way we determine whether the files are identical
             ;; depends on the diff format we use.
             (cond ((eq ah-clearcase-diff-format 'diff)
                    ;; diff format has an empty buffer
                    (equal (point-min) (point-max)))
                   ((eq ah-clearcase-diff-format 'serial)
                    ;; serial format prints "Files are identical", so
                    ;; we look for that.
                    (looking-at "Files are identical"))
                   (t (error "Unknown diff format"))))))))))


;;;; diff-tree

(defun vc-clearcase-annotate-command (file buf rev)
  "Fill BUF with the annotation of FILE's REV.

With prefix argument, will ask if you want to display the deleted
sections as well."
  (let ((pname (concat file (when rev (concat "@@" rev)))))
    (with-current-buffer buf
      (let ((fmt-args '("-fmt" "%-9.9Sd %-4.4u %-20.20Sn |"))
            (rm-args (when (and current-prefix-arg
                                (y-or-n-p "Show deleted sections? "))
                       '("-rm" "-rmfmt" "D %-9.9Sd %-4.4u |"))))
        (ah-cleartool-do
         "annotate"
         (append fmt-args rm-args `("-out" "-" "-nheader" ,pname))
         buf)
        (setq ah-cleartool-finished-function
              #'(lambda ()
                  (ah-clearcase-annotate-age-buffer)
                  (ah-clearcase-annotate-mark-deleted)))))))

(defun vc-clearcase-annotate-difference (point)
  "Return the age in days of POINT."
  (get-text-property point 'vc-clearcase-age))

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

(defun ah-clearcase-annotate-mk-age (age-str now)
  "Calculate an age in days from AGE-STR and NOW."
  (when (and (stringp age-str)
             (string-match ah-clearcase-annotate-date-rx age-str))
    (/ (- now
          (let ((day (string-to-int (match-string 1 age-str)))
                (month
                 (cdr (assoc (match-string 2 age-str)
                             ah-clearcase-annotate-months)))
                (year (string-to-int (match-string 3 age-str))))
            (incf year (if (< year 70) 2000 1900))
            (float-time (encode-time 0 0 0 day month year))))
       86400)))

(defun ah-clearcase-annotate-age-buffer (&optional buffer)
  "Compute the age of each line in BUFFER.

Assume that buffer contains an annotation result.  This is faster than
looking up regexps for each line in
'vc-clearcase-annotate-difference'"
  ;; until we arrange to call this when cleartool finishes...
  (interactive)
  (with-current-buffer (if buffer buffer (current-buffer))
    (save-excursion
      (goto-char (point-max))
      (let ((now (float-time))
            (beg (point))
            (end (point))
            (age-str "*INITIAL*")
            age-str1
            (date-rx1 "\\(^\\|D \\)[0-9]+-[A-Za-z]+-[0-9]+"))
        (while (re-search-backward date-rx1 nil 'noerror)
          (setq age-str1 (match-string 0))

          (unless (string= age-str1 age-str)
            (put-text-property
             beg end 'vc-clearcase-age
             (ah-clearcase-annotate-mk-age age-str now))

            (setq age-str age-str1)
            (setq end beg))

          (beginning-of-line)
          (setq beg (point)))

        ;; put the age on the first region
        (put-text-property
         beg end 'vc-clearcase-age
         (ah-clearcase-annotate-mk-age age-str now))))))

(defun ah-clearcase-annotate-mark-deleted (&optional buffer)
  "Mark all deleted files in the buffer with strike-through face."
  (interactive)
  (with-current-buffer (if buffer buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward ".*|D .*|\\(.*\\)\\s-*$" nil 'noerror)
        (put-text-property (match-beginning 1) (match-end 1)
                           'face '(:strike-through t))))))

(defun vc-clearcase-create-snapshot (dir name branchp)
  "Apply label NAME to DIR.

BRANCHP is used to move an existing label.  This is not the default
behaviour, but the default behaviour is useless for Clearcase.

First, if the label NAME does not exist, if is created with mklbtype
as an ordinary, non shared label.  Than the label is applied
recursively on DIR (but not moved if it already exists).  Than, for
each parent directory of DIR the label is applied only to that
directory.  This means that you can select this version of the sources
with this single line in the configspec:

element * NAME -nocheckout"
   (when (and branchp (not (yes-or-no-p "Move existing label? ")))
     (error "Aborted."))
  ;; let's see if the label exists
  (condition-case nil
      (ah-cleartool-ask (concat "desc lbtype:" name))
    (error (progn
             (message "Creating label %s" name)
             (ah-cleartool-ask (concat "mklbtype -ordinary -nc lbtype:" name))
             (message nil))))
  (let ((dir? (file-directory-p dir)))
    (message "Applying label...")
    (condition-case nil
        (ah-cleartool-ask (concat "mklabel -nc "
                                  (when branchp "-replace")
                                  (when dir? "-recurse")
                                  " lbtype:" name " " dir))
      (error nil))
    (when dir?                     ; apply label to parent directories
      (message "Applying label to parent directories...")
      (condition-case nil
          (while t               ; until cleartool will throw an error
            (setq dir (replace-regexp-in-string "[\\\\/]$" "" dir))
            (setq dir (file-name-directory dir))
            (ah-cleartool-ask (concat "mklabel -nc lbtype:" name " " dir)))
        (error nil)))
    (message "Finished applying label")))

;;;; assign-name
;;;; retrieve-snapshot
;;;; make-version-backups-p
;;;; check-headers
;;;; clear-headers

;;; NOTE: for some reason, the renamed file does not show up as a
;;; clearcase registered until after I kill it and re-open it...

(defun vc-clearcase-rename-file (old new)
  "Rename file OLD to NEW, both in the working area and in the
repository."
  (with-checkedout-dir (file-name-directory old)
    (with-checkedout-dir (file-name-directory new)
      (with-comment-file (format "*renamed from %s to %s*" old new)
        ;; let the cleartool error be directly reported
        (ah-cleartool-ask
         (format "mv -cfile %s \"%s\" \"%s\"" comment-file old new))))))

;;}}}

;;{{{ Aditional vc clearcase commands

(defun vc-clearcase-what-version ()
  "Show what is the version of the current file."
  (interactive)
  (let ((file (buffer-file-name (current-buffer))))
    (if (vc-clearcase-registered file)
        (progn
          (ah-clearcase-maybe-set-vc-state file)
          (let* ((fprop (ah-clearcase-fprop-file file))
                 (version (ah-clearcase-fprop-version fprop))
                 (co-status (ah-clearcase-fprop-checkout fprop)))
            (message "File version: %s%s" version
                     (cond
                      ((eq co-status 'reserved) ", checkedout reserved")
                      ((eq co-status 'unreserved) ", checkedout unreserved")
                      ((eq co-status 'hijacked) ", hijacked")
                      (t "")))))
      (message "Not a clearcase file"))))

(defun vc-clearcase-what-rule ()
  "Show what configspec rule selects this version of the current file."
  (interactive)
  (let ((file (buffer-file-name (current-buffer))))
    (if (vc-clearcase-registered file)
        (progn
          (ah-clearcase-maybe-set-vc-state file)
          (let ((rule (ah-clearcase-fprop-what-rule
                       (ah-clearcase-fprop-file file))))
            (if rule
                (message "Configspec rule: %s" rule)
              (message "No configspec rule"))))
      (message "Not a clearcase file"))))

(defun vc-clearcase-what-view-tag ()
  "Show view-tag for the current file."
  (interactive)
  (let ((file (buffer-file-name (current-buffer))))
    (if (vc-clearcase-registered file)
        (progn
          (ah-clearcase-maybe-set-vc-state file)
          (let ((view-tag (ah-clearcase-fprop-view-tag
                           (ah-clearcase-fprop-file file))))
            (if view-tag
                (message "View tag: %s" view-tag)
              (message "View tag not (yet?) known"))))
      (message "Not a clearcase file"))))

(defun vc-clearcase-gui-vtree-browser (ask-for-file)
  "Start the version tree browser gui on a file or directory.

When ASK-FOR-FILE is nil, the file in the current buffer is used.
Otherwise, it will ask for a file (you can also specify a directory,
in this case the versions of the directory itself will be browsed)"
  (interactive "P")
  (let ((file (buffer-file-name (current-buffer))))
    (when ask-for-file
      (setq file
            (read-file-name "Browse vtree for: " file file t)))
    (if (and file (vc-clearcase-registered file))
        (progn
          (message "Starting Vtree browser...")
          (start-process-shell-command
           "Vtree_browser" nil ah-clearcase-vtree-program file))
      (message "Not a clearcase file"))))

(defconst ah-cleartool-lsco-fmt
  (concat "----------\n"
          "file: %n\n"
          "parent-version: %PVn\n"
          "checkout-status: (%Rf)\n"
          "user: %u; view: %Tf; date: %Sd (%Ad days ago)\n\n"
          "%c")
  "Format string to use when listing checkouts.")

(defun vc-clearcase-list-checkouts (dir prefix-arg)
  "List the checkouts of the current user in DIR.

If PREFIX-ARG is present, an user name can be entered, and all the
views are searched for checkouts of the specified user.  If the
entered user name is empty, checkouts from all the users on all the
views are listed."
  (interactive "DList checkouts in directory: \nP")
  (when (string-match "\\(\\\\\\|/\\)$" dir)
    (setq dir (replace-match "" nil nil dir)))
  (let ((user-selection
         (if prefix-arg
             (let ((u (read-from-minibuffer "User: ")))
               (unless (equal u "")
                 (list "-user" u)))
           (list "-me" "-cview")))
        (other-options (list "-recurse" "-fmt" ah-cleartool-lsco-fmt dir)))
    (with-current-buffer (get-buffer-create "*clearcase-lsco*")
      (let ((inhibit-read-only t))
        (erase-buffer)
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

(defun vc-clearcase-update-view (dir prefix-arg)
  "Run a cleartool update command in DIR and display the results.

With PREFIX-ARG, run update in preview mode (no actual changes are
made to the views)."
  (interactive "DUpdate directory: \nP")
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

;;}}}

;;{{{ Editing configspecs

(defvar ah-clearcase-edcs-view-tag nil
  "The name of the view whose configspec we are editing.")

;; NOTE: for some configspecs cleartool will want to ask questions, I
;; didn't find a way to turn that off.

(defun ah-clearcase-setcs (&optional buffer view-tag)
  "Set the configspec found in BUFFER to the VIEW-TAG.

If buffer is nil, the current buffer is used.  If view-tag is nil, the
value of ah-clearcase-edcs-view-tag is used (local to buffer).  This
function should be invoked on a buffer setup by vc-clearcase-edcs.

NOTE: we can only set the configspec if view-tag has an associated
vprop to tell us if it's a dynamic or static view.  This usually means
you have to visit a file in that view before you can set the
configspec.  This note is here, because vc-clearcase-edcs allows you
to inspect the configspec of ANY view accessible from this machine."
  (interactive)
  (unless buffer (setq buffer (current-buffer)))
  (unless view-tag
    (setq view-tag (with-current-buffer buffer ah-clearcase-edcs-view-tag)))
  (let ((vprop (ah-clearcase-vprop-get view-tag))
        (configspec (buffer-file-name buffer)))
    (unless configspec (error "Buffer has no file associated with it"))
    (unless (memq (ah-clearcase-vprop-type vprop) '(snapshot dynamic))
      (error "Nothing known about %s.  Visist a file in the view first."
             view-tag))
    (when (buffer-modified-p buffer)
      (if (yes-or-no-p (format "Save %s? " configspec))
          (save-buffer buffer)
        (error "Aborted")))
    (let ((type (ah-clearcase-vprop-type vprop)))
      (cond ((eq type 'dynamic)
             ;; in a dynamic view, we simply set the configspec, than
             ;; trigger a resynch on all the visited files from that
             ;; view.
             (ah-cleartool-ask (concat "setcs -tag " view-tag
                                       " \"" configspec "\""))
             (message "%s's confispec updated." view-tag)
             (ah-clearcase-refresh-files-in-view view-tag))

            ((eq type 'snapshot)
             ;; in a snapshot view, a update will be triggered, so we
             ;; set the configspec with a ah-cleartool-do command, and
             ;; trigger the resynch in its finished callback.
             (with-current-buffer (get-buffer-create "*clearcase-setcs*")
               (let ((inhibit-read-only t))
                 (erase-buffer)
                 (cd (ah-clearcase-vprop-root vprop))
                 (let ((process
                        (ah-cleartool-do "setcs"
                                         (list "-tag" view-tag configspec)
                                         (current-buffer))))
                   (switch-to-buffer-other-window (process-buffer process))
                   ;; reuse this variable to hold the view tag in the
                   ;; update buffer.
                   (make-local-variable 'ah-clearcase-edcs-view-tag)
                   (setq ah-clearcase-edcs-view-tag view-tag)
                   (setq ah-cleartool-finished-function
                         #'(lambda ()
                             (ah-clearcase-refresh-files-in-view
                              ah-clearcase-edcs-view-tag)))))))))))

(define-derived-mode ah-clearcase-edcs-mode fundamental-mode "Configspec"
  "Generic mode to edit clearcase configspecs."
  (make-local-variable 'ah-clearcase-edcs-view-tag)

  ;; 'adapded' from values in emacs-lisp-mode
  (setq comment-start "#"
        comment-start-skip "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)#+ *"
        comment-end ""
        comment-end-skip nil)

  (font-lock-mode t))

;; Provide a shorter alias for the edcs mode.  This is usefull if you
;; want to keep configspecs separately and have mode tags in them.
(defalias 'edcs-mode 'ah-clearcase-edcs-mode)


(easy-mmode-defmap ah-clearcase-edcs-mode-map
                   '(("\C-c\C-s" . ah-clearcase-setcs))
                   "Keymap for Clearcase Edit Configspec mode")

(modify-syntax-entry ?\# "<" ah-clearcase-edcs-mode-syntax-table)
(modify-syntax-entry ?\n ">" ah-clearcase-edcs-mode-syntax-table)

(defvar ah-clearcase-edcs-all-view-tags nil
  "A list of all viewtags on this server.")

(defvar  ah-clearcase-edcs-all-view-tags-tid nil
  "Transaction ID to wait for fetching all view-tags.")

(defun ah-clearcase-view-tag-complete (string predicate flag)
  "Completion function for entering view-tag."
  ;; most of the code below could be implemented by try-completion,
  ;; except that ah-clearcase-edcs-all-view-tags might be set after
  ;; the first time we are called..
  (let ((len (length string))
        (exact-match? nil))
    (let ((matches (remove-if-not
                    #'(lambda (x)
                        (and (or (not predicate) (funcall predicate x))
                             (if (string= string x)
                                 ;; will also return t for remove-if-not
                                 (setq exact-match? t)
                               (and (>= (length x) len)
                                    (eq (compare-strings string 0 len x 0 len)
                                        t)))))
                    ah-clearcase-edcs-all-view-tags)))

      ;; NOTE: when we are asked for a list of completions, we must
      ;; return a fresh copy of the sequence, as I believe
      ;; completing-read manipulates that list internally and would
      ;; destroy our list.
      (cond
       ((eq flag t) (copy-sequence matches))
       ((eq flag 'lambda) exact-match?)
       (t (cond ((= (length matches) 0) nil)
                ((and exact-match? (= (length matches) 1)) t) ; exact match
                (exact-match? string)
                (t
                 ;; we need to find the longest common substring...
                 (let ((min-len (apply #'min (mapcar #'length matches)))
                       (a-string (car matches)))
                   (while (notevery
                           #'(lambda (x)
                               (eq t (compare-strings a-string 0 min-len
                                                      x 0 min-len)))
                           matches)
                     (decf min-len))
                   (substring a-string 0 min-len)))))))))

(defun vc-clearcase-edcs ()
  "Start ediding a config spec.

Prompts for a view-tag name with the default of the current file's
view-tag, fetches that view's config spec and pops up a buffer with
it."
  (interactive)
  (let ((file (buffer-file-name (current-buffer)))
        view-tag)
    (when (and file (vc-clearcase-registered file))
      (ah-clearcase-maybe-set-vc-state file)
      (setq view-tag (ah-clearcase-fprop-view-tag
                      (ah-clearcase-fprop-file file))))

    ;; don't remove the old view-tag list.  The view we are looking
    ;; for might already be in there.

    ;; (setq ah-clearcase-edcs-all-view-tags nil)

    ;; start reading the view-tags. By the time the user decides what
    ;; view-tag it wants, we may have the answer already.
    (setq ah-clearcase-edcs-all-view-tags-tid
          (ah-cleartool-ask
           "lsview -short" 'nowait nil
           #'(lambda (x view-tags)
               (setq ah-clearcase-edcs-all-view-tags
                     (split-string view-tags "[\n\r]+")))))

    (setq view-tag (completing-read
                    "Edit configspec for view: "
                    #'ah-clearcase-view-tag-complete
                    nil nil view-tag))

    (message "Fetching configspec for %s" view-tag)
    (let ((tid (ah-cleartool-ask (concat "catcs -tag " view-tag) 'nowait))
          (configspec-file (concat ah-clearcase-tmpdir
                                   (format "/%s.configspec" view-tag))))

      (with-current-buffer (find-file-noselect configspec-file)
        (ah-clearcase-edcs-mode)
        (setq ah-clearcase-edcs-view-tag view-tag)
        (erase-buffer)
        (insert (ah-cleartool-wait-for tid))
        (goto-char (point-min))
        (pop-to-buffer (current-buffer))
        (message nil)))

    ;; We wait for this transaction here because by this time it will
    ;; already have ended (since we already read a configspec above).
    (ignore-errors
      (ah-cleartool-wait-for ah-clearcase-edcs-all-view-tags-tid))))

;;}}}

;;{{{ Update vc keymap

;;; bind the extra clearcase commands to keys and menus in the vc
;;; keymap

(define-key vc-prefix-map "y" 'vc-clearcase-what-version)
(define-key vc-prefix-map "w" 'vc-clearcase-what-rule)
(define-key vc-prefix-map "t" 'vc-clearcase-what-view-tag)
(define-key vc-prefix-map "e" 'vc-clearcase-edcs)
(define-key vc-prefix-map "p" 'vc-clearcase-update-view)
(define-key vc-prefix-map "o" 'vc-clearcase-list-checkouts)
(define-key vc-prefix-map "j" 'vc-clearcase-gui-vtree-browser)

(define-key-after vc-menu-map [separator-clearcase] '("----") 'separator2)
(define-key-after vc-menu-map [vc-clearcase-what-version]
  '("Show file version" . vc-clearcase-what-version) 'separator2)
(define-key-after vc-menu-map [vc-clearcase-what-rule]
  '("Show configspec rule" . vc-clearcase-what-rule) 'separator2)
(define-key-after vc-menu-map [vc-clearcase-what-view-tag]
  '("Show view tag" . vc-clearcase-what-view-tag) 'separator2)
(define-key-after vc-menu-map [vc-clearcase-gui-vtree-browser]
  '("Browse version tree (GUI)" . vc-clearcase-gui-vtree-browser) 'separator2)

;; 'borrowed' from pcvs-defs.el, Clearcase commands that are not file
;; related will go in a Clearcase menu under Tools.
(defvar clearcase-global-menu
  (let ((m (make-sparse-keymap "Clearcase")))
    (define-key m [vc-clearcase-list-checkouts]
      '(menu-item "List Checkouts" vc-clearcase-list-checkouts
		  :help "List Clearcase checkouts in a directory"))
    (define-key m [vc-clearcase-update-view]
      '(menu-item "Update snapshot view" vc-clearcase-update-view
		  :help "Update a snpshot view"))
    (define-key m [vc-clearcase-edcs]
      '(menu-item "Edit Configspec" vc-clearcase-edcs
		  :help "Edit a view's configspec"))
    (fset 'clearcase-global-menu m)))

(define-key-after menu-bar-tools-menu [ah-clearcase]
  '(menu-item "Clearcase" clearcase-global-menu)
  'vc)

;;}}}

;;{{{ ah-annotate-show-color-map

(defun ah-annotate-show-color-map ()
  "Display a color map as a guide for the annotate colors.

This function mimics vc.el from Emacs 21.3.1 and will need to be
updated for later versions."
  (interactive)
  (let ((days-code
         (apply #'mapcar* #'list
                (mapcar (lambda (x)
                          (vc-annotate-time-span vc-annotate-color-map x))
                        (cons 1 vc-annotate-menu-elements))))
        (code-name
         (cons "Default"
               (let ((max-days (caar (reverse vc-annotate-color-map))))
                 (mapcar (lambda (x)
                           (format "%d days" (round (* max-days x 0.7585))))
                         vc-annotate-menu-elements))))
        (code-faces
         (mapcar (lambda (x)
                   (let ((face-name
                          (concat "ah-annotate-face-" (substring (cdr x) 1))))
                     (or (intern-soft face-name)
                         (let ((tmp-face (make-face (intern face-name))))
                           (set-face-foreground tmp-face (cdr x))
                           (if vc-annotate-background
                               (set-face-background tmp-face
                                                    vc-annotate-background))
                           tmp-face))))
                 vc-annotate-color-map)))

    (with-current-buffer (get-buffer-create "*Annotate Color Map*")
      (setq buffer-read-only t)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (goto-char (point-min))
        (insert "\nTime span: ")
        (mapcar (lambda (x) (insert (format "%10s  " x))) code-name)
        (insert "\n\n")
        (mapcar* (lambda (x y)
                   (let ((beg (point)))
                     (insert "           ")
                     (mapcar (lambda (y)
                               (insert (format "%10.2f  " (car y))))
                             x)
                     (put-text-property beg (point) 'face y)
                     (insert "\n")))
                 days-code code-faces)
        (pop-to-buffer (current-buffer))))))

;;}}}

;;{{{ Debugging aids

(defun ah-clearcase-trace-cleartool-tq ()
  "Trace some of the cleartool commands."
  (interactive)
  (let ((trace-buf (get-buffer-create "*cleartool-tq-trace*")))
    (trace-function-background 'ah-cleartool-ask trace-buf)
    (trace-function-background 'ah-cleartool-wait-for trace-buf)
    (trace-function-background 'ah-cleartool-tq-handler trace-buf)))

;;}}}

;; start cleartool here, we will need it anyway
(ah-cleartool-tq-maybe-start)

(provide 'vc-clearcase)

;;; vc-clearcase.el ends here
