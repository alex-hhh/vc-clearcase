;; ucm.el -- Support for ClearCase Unified Change Management (UCM)

;; Copyright (C) 2007 Alexandru Harsanyi
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
;; Created: 25 Nov 2007
;; Keywords: version-control, clearcase
;; Homepage: http://sourceforge.net/projects/vc-clearcase/
;; $Id$

;;; Commentary
;; This implements support for ClearCase UCM.  The user functions
;; `ucm-set-activity', `ucm-show-current-activity', `ucm-browse-activity' and
;; `ucm-checkin-activity'.  See their doc strings for details.  As of now,
;; these functions are not bound to keys and menus, so you need to invoke them
;; using M-x (`execute-extended-command').

;;; Code

(eval-when-compile (require 'cl))
(require 'vc-clearcase)
(require 'button)

(defgroup ucm nil
  "Support for UCM under ClearCase"
  :group 'vc-clearcase)

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
With prefix argument (EXTRA-INFO), also shows the number of
files modified under this activity, number of versions and the
number of checked out files."
  (interactive "P")
  (with-cleartool-directory (expand-file-name default-directory)
    (let ((headline (cleartool "lsact -cact -fmt \"%%[headline]p\""))
	  versions
	  (files 0)
	  (checkouts 0)
	  file-activity-headline)

      ;; If the current file is checked out under a different activity,
      ;; display that in the message...

      (let ((file (buffer-file-name (current-buffer))))
	(when file
	  (let ((fprop (clearcase-file-fprop file)))
	    (when fprop
	      (when (clearcase-fprop-checkedout-p fprop)
		(setq file-activity-headline
		      (cleartool "lsact -fmt \"%%[headline]p\" %s"
				 (cleartool "desc -fmt \"%%[activity]p\" \"%s\""
					    file))))))))
      (if (and file-activity-headline
	       (not (equal headline file-activity-headline)))

	  (message "View activity: %s; File's checkout activity: %s"
		   headline file-activity-headline)

	  (if (equal headline "")
	      (message "No current activity set.")
	      (when extra-info
		(with-temp-message "Collecting activity statistics..."
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
		  (message "%s" headline)))))))

(defface ucm-field-name-face
    '((t (:inherit font-lock-keyword-face :weight bold)))
  "Face used for field names in UCM browse buffers."
  :group 'ucm)

(defface ucm-file-name-face
    '((t (:weight bold)))
  "Face used for file names in UCM browse buffers."
  :group 'ucm)

(defface ucm-revision-face
    '((t (:slant italic)))
  "Face used for revisions in UCM browse buffers."
  :group 'ucm)

(defface ucm-checkedout-revision-face
    '((t (:foreground "IndianRed" :slant italic)))
  "Face used for checkedout revisions in UCM browse buffers."
  :group 'ucm)

(defvar ucm-activity nil
  "The name of the current activity being browsed.")

(defvar ucm-previous-activities '()
  "A stack of previous activities we visited.
Used to implement the BACK button.")

(defun ucm-file-link-handler (button)
  (with-current-buffer (button-get button 'buffer)
    (pop-to-buffer (current-buffer))
    (let ((file-name (button-get button 'file-name)))
      (pop-to-buffer (find-file-noselect file-name)))))

(define-button-type 'ucm-file-link
    'face 'default
    'help-echo "mouse-2, RET: Visit this file"
    'follow-link t
    'action 'ucm-file-link-handler
    'skip t)

(defun ucm-url-link-handler (button)
  (browse-url (button-get button 'url)))

(define-button-type 'ucm-url-link
    'face 'default
    'help-echo "mouse-2, RET: Visit this URL"
    'follow-link t
    'action 'ucm-url-link-handler)

(defun ucm-activity-link-handler (button)
  (with-current-buffer (button-get button 'buffer)
    (pop-to-buffer (current-buffer))
    (assert ucm-activity)
    (push ucm-activity ucm-previous-activities)
    (ucm-browse-activity (button-get button 'ucm-activity))))

(define-button-type 'ucm-activity-link
    'face 'default
    'help-echo "mouse-2, RET: Browse this activity"
    'follow-link t
    'action 'ucm-activity-link-handler
    'skip t)

(defun ucm-previous-activity-link-handler (button)
  (with-current-buffer (button-get button 'buffer)
    (pop-to-buffer (current-buffer))
    (when ucm-previous-activities
      (ucm-browse-activity (pop ucm-previous-activities)))))

(define-button-type 'ucm-previous-activity-link
    'face 'default
    'help-echo "mouse-2, RET: Browse previous activity"
    'action 'ucm-previous-activity-link-handler
    'follow-link t
    'skip t)

(defun ucm-show-diff-link-handler (button)
  (with-current-buffer (button-get button 'buffer)
    (pop-to-buffer (current-buffer))
    (let* ((file (expand-file-name (button-get button 'file-name)))
	   (current (button-get button 'revision)))
      ;; make sure file is loaded, as we need a proper FPROP to run the diff.
      (find-file-noselect file)
      (if (string-match "\\<CHECKEDOUT\\(.[0-9]+\\)?" current)
	  ;; this is a checked out version, let's hope it is in our view...
	  (vc-clearcase-diff file)
	  ;; else
	  (let ((previous (cleartool
			   "desc -fmt \"%%PVn\" \"%s@@%s\"" file current)))
	    (vc-clearcase-diff file previous current)))
      (pop-to-buffer (get-buffer "*vc-diff*")))))

(define-button-type 'ucm-show-diff-link
    'face 'default
    'help-echo "mouse-2, RET: Show diff"
    'action 'ucm-show-diff-link-handler
    'follow-link t
    'skip t)

;;;###autoload
(defun ucm-browse-activity (activity)
  "Pop-up an information buffer about ACTIVITY.
The buffer will contain a report about the file versions
checked-in under the activity plus any contributing activities.
The report contains buttons (hyperlinks) to files, versions and
other activities.

In interactive mode, the user is prompted for an activity name
and completion is available.  ACTIVITY must be in the current
stream (corresponding to the view in `default-directory').  With
prefix argument, obsolete activities can also be selected.  With
a negative prefix argument any activity can be selected, but no
completion is provided.

There are no restriction on ACTIVITY when called from another
program."
  (interactive
   (list
    (if (or (eq current-prefix-arg '-)
	    (and (integerp current-prefix-arg)
		 (< current-prefix-arg 0)))
	(read-string "Browse activity: ")
	(ucm-read-activity
	 "Browse activity: " nil
	 (when current-prefix-arg 'include-obsolete)))))
  (with-temp-message "Preparing report..."
    (with-cleartool-directory (expand-file-name default-directory)
      (let ((changeset (make-hash-table :test 'equal))
	    (changeset-files nil)
	    (view (replace-regexp-in-string "[\n\r]+" "" (cleartool "pwv -short"))))

	(unless (string= view "")
	  (let ((vprop (clearcase-get-vprop view default-directory)))
	    (setq view (clearcase-vprop-root-path vprop))))

	(dolist (v (split-string
		    (cleartool "lsact -fmt \"%%[versions]Cp\" %s" activity)
		    ", " 'omit-nulls))
	  (when (string-match "\\(.*\\)@@\\(.*\\)" v)
	    (let ((file (match-string 1 v))
		  (revision (match-string 2 v)))
	      (push revision (gethash file changeset)))))

	(maphash (lambda (k v) (push k changeset-files)) changeset)
	(setq changeset-files
	      (sort changeset-files
		    (lambda (a b)
		      (string< (file-name-nondirectory a)
			       (file-name-nondirectory b)))))

	(with-current-buffer (get-buffer-create "*UCM Activity*")
	  (setq buffer-read-only t)
	  (buffer-disable-undo)
	  (set (make-local-variable 'ucm-activity) activity)
	  (make-local-variable 'ucm-previous-activities)

	  (let ((inhibit-read-only t)
		(headline (cleartool "lsact -fmt \"%%[headline]p\" %s" activity)))
	    (erase-buffer)
	    (insert (propertize "Name: " 'face 'ucm-field-name-face) activity
		    "\n"
		    (propertize "Headline: " 'face 'ucm-field-name-face) headline
		    "\n"
		    (propertize "Status: " 'face 'ucm-field-name-face)
		    (cleartool "lsact -fmt \"%%[locked]p\" %s" activity))
	    (when (or (string-match "SFT-[0-9]+" activity)
		      (string-match "SFT-[0-9]+" headline))
	      (insert "\n" (propertize "JIRA Link: " 'face 'ucm-field-name-face))
	      (let ((url (format "http://jira/browse/%s" (match-string 0 activity))))
		(insert-text-button
		 url
		 'type 'ucm-url-link
		 'url url)))
	    (insert "\n"
		    (propertize "Created By: " 'face 'ucm-field-name-face)
		    (cleartool "lsact -fmt \"%%[owner]p\" %s" activity))
	    (insert "\n\n"
		    (propertize "File Versions:" 'face 'ucm-field-name-face)
		    "\n==============\n")
	    (dolist (file changeset-files)
	      (insert "\n    ")
	      (insert-text-button
	       (file-name-nondirectory file)
	       'face 'ucm-file-name-face
	       'type 'ucm-file-link
	       'buffer (current-buffer)
	       'file-name file)
	      (insert " in " (file-relative-name (file-name-directory file) view) "\n")
	      (dolist (revision (gethash file changeset))
		(insert "        ")
		(insert-text-button
		 (concat "@@" revision)
		 'face (if (string-match "[\\/]CHECKEDOUT\\(.[0-9]+\\)\\'" revision)
			   'ucm-checkedout-revision-face
			   'ucm-revision-face)
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
	      ;; explicitly ...
	      (let ((contrib (cleartool
			      "lsact -fmt \"%%[contrib_acts]p\" %s" activity)))
		(when (and contrib
			   (not (string-match "^cleartool: Error: " contrib)))
		  (insert "\n"
			  (propertize "Contributing Activities:" 'face 'ucm-field-name-face)
			  "\n========================\n\n")
		  (dolist (c (split-string contrib " " 'omit-nulls))
		    (insert "    ")
		    (insert-text-button
		     c
		     'type 'ucm-activity-link
		     'buffer (current-buffer)
		     'ucm-activity c)
		    (insert "\n")))))

	    (when ucm-previous-activities
	      (insert "\n\n")
	      (insert-text-button
	       "[back]"
	       'type 'ucm-previous-activity-link
	       'buffer (current-buffer))
	      (insert "\n")))

	  (set-buffer-modified-p nil)
	  (goto-char (point-min))
	  (pop-to-buffer (current-buffer)))))))

;;;###autoload
(defun ucm-checkin-activity (activity)
  "Check in all files checked-out under ACTIVITY.
This will pop-up a `log-edit' buffer to enter the check in
comment, than attempt to check in the files.

If the log buffer is empty, each file to be checked in using its
original check out comment, otherwise the same log message will
be used for all files.

An error will be signalled if no files are checked out under
ACTIVITY.

HINT: `log-edit-modes' allows to see what files will be
checked-in using \\[log-edit-show-files]."
  (interactive (list (ucm-read-activity "Checkin activity: ")))
  (when (member activity '("*NONE*" "*NEW-ACTIVITY*"))
    (error "Not a real activity"))
  (with-cleartool-directory default-directory
    (lexical-let ((checkin-activity (cleartool "lsact -fmt \"%%Xn\" %s" activity))
		  (dir default-directory))
      (let ((modified-files nil)
	    (reverted-files nil))
	;; Checked out files which have no changes are reverted now.
	(dolist (file (ucm-checked-out-files activity dir))
	  (find-file-noselect file)     ; read in file so it has a fprop
	  (if (and (file-regular-p file)
		   (vc-clearcase-workfile-unchanged-p file))
	      (progn
		(message "Undo checkout for unmodified file %s" file)
		(cleartool "uncheckout -keep \"%s\"" file)
		(push file reverted-files))
	      (push file modified-files)))
	(when reverted-files
	  (clearcase-refresh-files-in-view))
	(when (null modified-files)
	  (error "No files to checkin.")))

      (log-edit (lambda ()
		  (interactive)
		  (ucm-finish-activity-checkin checkin-activity dir))
		'setup
		(lambda ()
		  (interactive)
		  (ucm-checked-out-files checkin-activity dir))
		(get-buffer-create "*UCM-Checkin-Log*")))))

(defun ucm-checked-out-files (activity dir)
  "Return the list of files checked out under ACTIVITY.
The file names are relative to the `default-directory'"
  (with-cleartool-directory dir
    (let ((files nil))
      (dolist (v (split-string
		  (cleartool "lsact -fmt \"%%[versions]Cp\" %s" activity)
		  ", " 'omit-nulls))
	(when (string-match "\\(.*\\)@@\\(.*\\)" v)
	  (let ((file (match-string 1 v))
		(revision (match-string 2 v)))
	    (when (string-match "\\<CHECKEDOUT\\(.[0-9]+\\)?" revision)
	      (add-to-list 'files (file-relative-name file))))))
      files)))

(defun ucm-finish-activity-checkin (activity dir)
  "Check-in files under ACTIVITY using the contents of
*UCM-Checkin-Log* as the comment."
  (with-cleartool-directory dir
    (with-temp-message (format "Checking in %s..." activity)
      (let ((comment-text (with-current-buffer (get-buffer "*UCM-Checkin-Log*")
			    (buffer-substring-no-properties (point-min) (point-max)))))
	(if (string= comment-text "")
	    (cleartool "checkin -nc %s" activity)
	    (with-clearcase-cfile (comment comment-text)
	      (cleartool "checkin -cfile \"%s\" %s" comment activity)))
	(clearcase-refresh-files-in-view)))))

;;;###autoload
(defun ucm-lock-activity (activity)
  "Lock ACTIVITY.  With prefix arg, mark it as obsolete."
  (interactive (list (ucm-read-activity "Lock activity: ")))
  (with-cleartool-directory default-directory
    (let ((status (cleartool "lsact -fmt \"%%[locked]p\" %s" activity)))
      (if (member status '("locked" "obsolete"))
	  (message "%s is already locked" activity)
	  (progn
	    (cleartool "lock %s activity:%s@/projects"
		       (if current-prefix-arg "-obsolete" "") activity)
	    (message "%s is now %s" activity
		     (if current-prefix-arg "obsolete" "locked")))))))

;;;###autoload
(defun ucm-unlock-activity (activity)
  "Unlock ACTIVITY. With prefix arg, allow selecting obsolete activities."
  (interactive
   (list (ucm-read-activity
	  "Unlock activity: " nil
	  (when current-prefix-arg 'include-obsolete))))
  (with-cleartool-directory default-directory
    (let ((status (cleartool "lsact -fmt \"%%[locked]p\" %s" activity)))
      (if (equal status "unlocked")
	  (message "%s is already unlocked" activity)
	  (progn
	    (cleartool "unlock activity:%s@/projects" activity)
	    (message "%s is now unlocked" activity))))))


(provide 'ucm)
;;; ucm.el ends here
