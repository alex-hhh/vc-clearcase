;; ucm.el -- Support for ClearCase Unified Change Management (UCM)

(eval-when-compile (require 'cl))
(require 'vc-clearcase)
(require 'button)

(defun ucm-read-activity (prompt &optional view-tag include-obsolete initial)
  "Display PROMPT and read a UCM activity with completion.

VIEW-TAG is the view from which activities are read -- when nil,
the view is determined from `default-directory' (which is the
current directory).

When INCLUDE-OBSOLETE is not nil, obsolete activities are also
included in the list.

INITIAL is the initial activity name presented to the user.  When
nil, the current activity in the view is presented to the user."

  (with-cleartool-directory (expand-file-name default-directory)
    (unless view-tag
      (setq view-tag
	    (replace-regexp-in-string
	     "[\n\r]+" "" (cleartool "pwv -short"))))
    (unless initial
      (setq initial
	    (ignore-cleartool-errors
	     (cleartool-ask "lsact -cact -fmt \"%n\"")))))

  ;; The view might not be known, so we pass in the `default-directory' to
  ;; `clearcase-get-vprop' so it can be properly created.
  (lexical-let ((vprop (clearcase-get-vprop view-tag default-directory)))

    ;; wait for a previous activity collection transaction

    (cleartool-wait-for (clearcase-vprop-activities-tid vprop))

    ;; Start reading the activities asynchronously.  By the time the user
    ;; decides what activity it wants, we may have the answer already.  Note
    ;; that the previous activity list still exists and the user can perform
    ;; completions form that one.

    (let ((tid (cleartool-ask
		(format "lsact %s -fmt \"%%n\\n\" -view %s"
			(if include-obsolete "-obsolete" "")
			(clearcase-vprop-name vprop))
		'nowait
		vprop
		(lambda (vprop answer)
		  (setf (clearcase-vprop-activities vprop)
			(append (list "*NONE*" "*NEW-ACTIVITY*")
				(split-string answer "[\n\r]+" t)))))))
      (setf (clearcase-vprop-activities-tid vprop) tid))

    (completing-read
     prompt
     '(lambda (string predicate flag)
       (let ((fn (cond ((eq flag t) 'all-completions)
		       ((eq flag 'lambda)
			;; test-completion does not exist in emacs 21.
			'(lambda (x l &optional p) (member x l)))
		       ((null flag) 'try-completion)
		       (t (error "Unknown value for flag %S" flag)))))
	 (funcall
	  fn string (clearcase-vprop-activities vprop) predicate)))
     nil
     'require-match
     initial)))


;;;###autoload
(defun ucm-set-activity (&optional activity)
  "Set the UCM ACTIVITY in the current directory.
In interactive mode, the user is prompted for the available
activities in the stream associated with the UCM view in the
`default-directory', and the selected one is set.

Two special activity names are also accepted: *NONE* which will
cause the current activity to be unset and *NEW-ACTIVITY* which
will create and set a new activity (the user is prompted for the
activity headline)."
  (interactive (list (ucm-read-activity "Set activity: ")))
  (with-cleartool-directory (expand-file-name default-directory)
    (cond
      ((equal activity "*NONE*")
       (cleartool "setact -none"))
      ((equal activity "*NEW-ACTIVITY*")
       (let ((headline (read-from-minibuffer "Activity headline: ")))
	 (when (equal headline "")
	   (error "Activity headline cannot be empty"))
	 (cleartool "mkact -force -headline \"%s\"" headline)))
      (t
       (cleartool "setact \"%s\"" activity)))))

;;;###autoload
(defun ucm-show-current-activity (&optional extra-info)
  "Show the current activity in the view.
With prefix arguument (EXTRA-INFO), also shows the number of
files modified under this activity, number of versions and the
number of checked out files."
  (interactive "P")
  (with-cleartool-directory (expand-file-name default-directory)
    (let ((headline (cleartool "lsact -cact -fmt \"%%[headline]p\""))
	  versions
	  (files 0)
	  (checkouts 0))
      (if (equal headline "")
	  (message "No current activity set.")
	  (when extra-info
	    (with-temp-message "Collecting activitiy statistics..."
	      (setq versions
		    (split-string
		     (cleartool "lsact -cact -fmt \"%%[versions]Cp\"") ", " t))
	      (setq files (make-hash-table :test 'equal))
	      (dolist (v versions)
		(when (string-match "CHECKEDOUT\\(\.[0-9]+\\)?$" v)
		  (incf checkouts))
		(when (string-match "^\\(.*\\)@@" v)
		  (setf (gethash (match-string 1 v) files) t)))))
	  (if extra-info
	      (message "%s; %d files, %d revisions, %d checked-out"
		       headline (hash-table-count files) (length versions) checkouts)
	      (message "%s" headline))))))

(defvar ucm-activity nil
  "The name of the current activity being browsed.")

(defvar ucm-previous-activities '()
  "A stack of previous activities we visited.
Used to implement the BACK button.")

(define-button-type 'ucm-file-link
    'face 'default
    'help-echo "mouse-2, RET: Visit this file"
    'follow-link t
    'action (lambda (button)
	      (pop-to-buffer (button-get button 'buffer))
	      (let ((file-name (button-get button 'file-name)))
		(pop-to-buffer (find-file-noselect file-name))))
    'skip t)

(define-button-type 'ucm-activity-link
    'face 'default
    'help-echo "mouse-2, RET: Browse this activity"
    'follow-link t
    'action (lambda (button)
	      (pop-to-buffer (button-get button 'buffer))
	      (push ucm-activity ucm-previous-activities)
	      (ucm-browse-activity (button-get button 'ucm-activity)))
    'skip t)

(define-button-type 'ucm-previous-activity-link
    'face 'default
    'help-echo "mouse-2, RET: Browse previous activity"
    'action (lambda (button)
	      (pop-to-buffer (button-get button 'buffer))
	      (when ucm-previous-activities
		(ucm-browse-activity (pop ucm-previous-activities))))
    'follow-link t
    'skip t)

(define-button-type 'ucm-show-diff-link
    'face 'default
    'help-echo "mouse-2, RET: Show diff"
    'action (lambda (button)
	      (pop-to-buffer (button-get button 'buffer))

	      (let* ((file (expand-file-name (button-get button 'file-name)))
		     (current (button-get button 'revision)))
		;; make sure file is loaded, as we need a proper FPROP to run
		;; the diff.
		(find-file-noselect file)
		(if (string-match "\\<CHECKEDOUT\\(.[0-9]+\\)?" current)
		    ;; this is a checked out version, let's hope it is in our
		    ;; view...
		    (vc-clearcase-diff file)
		    ;; else
		    (let ((previous (cleartool
				     "desc -fmt \"%%PVn\" \"%s@@%s\"" file current)))
		      (vc-clearcase-diff file previous current)))
		(pop-to-buffer (get-buffer "*vc-diff*"))))
    'follow-link t
    'skip t)

;;;###autoload
(defun ucm-browse-activity (activity)
  (interactive (list (ucm-read-activity "Browse activity: " nil 'include-obsolete)))
  (with-temp-message "Preparing report..."
    (with-cleartool-directory (expand-file-name default-directory)
      (let ((changeset (make-hash-table :test 'equal))
	    (view (replace-regexp-in-string "[\n\r]+" "" (cleartool "pwv -short"))))

	(unless (string= view "")
	  (let ((vprop (clearcase-get-vprop view)))
	    (setq view (clearcase-vprop-root-path vprop))))

	(dolist (v (split-string
		    (cleartool "lsact -fmt \"%%[versions]Cp\" %s" activity)
		    ", " 'omit-nulls))
	  (when (string-match "\\(.*\\)@@\\(.*\\)" v)
	    (let ((file (match-string 1 v))
		  (revision (match-string 2 v)))
	      (push revision (gethash file changeset)))))

	(with-current-buffer (get-buffer-create "*UCM Activity*")
	  (setq buffer-read-only t)
	  (buffer-disable-undo)
	  (set (make-local-variable 'ucm-activity) activity)
	  (make-local-variable 'ucm-previous-activities)

	  (let ((inhibit-read-only t))
	    (erase-buffer)
	    (insert (cleartool
		     "lsact -fmt \"%%[headline]p    (%%[locked]p)\" %s" activity))
	    (insert "\nName: " activity)
	    (insert "\nCreated By: "
		    (cleartool "lsact -fmt \"%%[owner]p\" %s" activity))
	    (insert "\n\nFile Versions:\n"
		    "==============\n")
	    (loop for file being the hash-keys of changeset
	       do (progn
		    (insert "\n    ")
		    (insert-text-button
		     (file-name-nondirectory file)
		     'face 'bold
		     'type 'ucm-file-link
		     'buffer (current-buffer)
		     'file-name file)
		    (insert " in " (file-relative-name (file-name-directory file) view) "\n"))
	       do (dolist (revision (gethash file changeset))
		    (insert "        ")
		    (insert-text-button
		     (concat "@@" revision)
		     'face 'italic
		     'type 'ucm-show-diff-link
		     'buffer (current-buffer)
		     'file-name file
		     'revision revision)
		    (insert "\n")))
	    (ignore-cleartool-errors
	      ;; There seems to be a bug in my version of ClearCase: if
	      ;; `activity' is not a rebase or integration activity an error
	      ;; will be reported, but the status of the command will be 0
	      ;; (meaning success).  We have to test the returned string
	      ;; explicitely ...
	      (let ((contrib (cleartool
			      "lsact -fmt \"%%[contrib_acts]p\" %s" activity)))
		(when (and contrib
			   (not (string-match "^cleartool: Error: " contrib)))
		  (insert "\nContributing Activities:\n"
			  "========================\n\n")
		  (dolist (c (split-string contrib " " 'omit-nulls))
		    (insert "    ")
		    (insert-text-button
		     c
		     'type 'ucm-activity-link
		     'ucm-activity c)
		    (insert "\n")))))

	    (when ucm-previous-activities
	      (insert "\n\n")
	      (insert-text-button
	       "[back]"
	       'type 'ucm-previous-activity-link)
	      (insert "\n")))

	  (set-buffer-modified-p nil)
	  (goto-char (point-min))
	  (pop-to-buffer (current-buffer)))))))

;;;###autoload
(defun ucm-checkin-activity (activity)
  "Check in all files checked-out under ACTIVITY.
This will pop-up a `log-edit' buffer to enter the check in
comment, than atempt to check in the files.

If the log buffer is empty, each file to be checked in using its
original check out comment, otherwise the same log message will
be used for all files.

An error will be signaled if no files are checked out under
ACTIVITY.

HINT: `log-edit-modes' allows to see what files will be
checked-in using \\[log-edit-show-files]."
  (interactive (list (ucm-read-activity "Checkin activity: ")))
  (when (member activity '("*NONE*" "*NEW-ACTIVITY*"))
    (error "Not a real activity"))
  (lexical-let ((checkin-activity (cleartool "lsact -fmt \"%%Xn\" %s" activity)))
    (when (null (ucm-checked-out-files checkin-activity))
      (error "No files checked out under this activity"))
    (log-edit (lambda () (ucm-finish-activity-checkin checkin-activity))
	      'setup
	      (lambda () (ucm-checked-out-files checkin-activity)
	      (get-buffer-create "*UCM-Checkin-Log*")))))

(defun ucm-checked-out-files (activity)
  "Return the list of files checked out under ACTIVITY.
The file names are relative to the `default-directory'"
  (let ((files nil))
    (dolist (v (split-string
		(cleartool "lsact -fmt \"%%[versions]Cp\" %s" activity)
		", " 'omit-nulls))
      (when (string-match "\\(.*\\)@@\\(.*\\)" v)
	(let ((file (match-string 1 v))
	      (revision (match-string 2 v)))
	  (when (string-match "\\<CHECKEDOUT\\(.[0-9]+\\)?" revision)
	    (add-to-list 'files (file-relative-name file))))))
    files))

(defun ucm-finish-activity-checkin (activity)
  "Check-in files under ACTIVITY using the contents of
*UCM-Checkin-Log* as the comment."
  (interactive)
  (with-temp-message (format "Checking in %s..." activity)
    (let ((comment-text (with-current-buffer (get-buffer "*UCM-Checkin-Log*")
			  (buffer-substring-no-properties (point-min) (point-max)))))
      (if (string= comment-text "")
	  (cleartool "checkin -nc %s" activity)
	  (with-clearcase-cfile (comment comment-text)
	    (cleartool "checkin -cfile \"%s\" %s" comment activity)))
      (clearcase-refresh-files-in-view))))

(provide 'ucm)
