;; NOTE: we need the ignore-cleartool-errors macro from vc-clearcase which is
;; only present when we evaluate vc-clearcase (not when we load it compiled)

;; $Id$

(require 'button)

(eval-when-compile
  (require 'vc-clearcase))

(defvar ucm-activity nil
  "The name of the current activity being browsed.")

(defvar ucm-previous-activities '()
  "A stack of previous activities we visited.
Used to implement the BACK button.")

(make-variable-buffer-local 'ucm-previous-activities)

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
		     (current (button-get button 'revision))
		     (previous (ah-cleartool
				"desc -fmt \"%%PVn\" \"%s@@%s\"" file current)))
		(vc-clearcase-diff file previous current)
		(pop-to-buffer (get-buffer "*vc-diff*"))))
    'follow-link t
    'skip t)

(defun ucm-browse-activity (activity)

  (interactive (list (ah-clearcase-read-activity
		      "Browse activity: " nil 'include-obsolete)))

  (assert activity)

  (with-temp-message "Preparing report..."
    (ah-cleartool "cd \"%s\"" (expand-file-name default-directory))
    (let ((changeset (make-hash-table :test 'equal))
	  (view (replace-regexp-in-string "[\n\r]+" "" (ah-cleartool "pwv -short"))))

      (unless (string= view "")
	(let ((vprop (ah-clearcase-get-vprop view)))
	  (setq view (ah-clearcase-vprop-root-path vprop))))

      (dolist (v (split-string
		  (ah-cleartool "lsact -fmt \"%%[versions]Cp\" %s" activity)
		  ", " 'omit-nulls))
	(when (string-match "\\(.*\\)@@\\(.*\\)" v)
	  (let ((file (match-string 1 v))
		(revision (match-string 2 v)))
	    (push revision (gethash file changeset)))))

      (with-current-buffer (get-buffer-create "*UCM Activity*")
	(setq buffer-read-only t)
	(buffer-disable-undo)
	(set (make-local-variable 'ucm-activity) activity)
	(let ((inhibit-read-only t))
	  (erase-buffer)
	  (insert (ah-cleartool
		   "lsact -fmt \"%%[headline]p    (%%[locked]p)\" %s" activity))
	  (insert "\nName: " activity)
	  (insert "\nCreated By: "
		  (ah-cleartool "lsact -fmt \"%%[owner]p\" %s" activity))
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
	    (let ((contrib (ah-cleartool
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
	(pop-to-buffer (current-buffer))))))

(provide 'ucm)