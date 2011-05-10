;; ucm.el -- Support for ClearCase Unified Change Management (UCM)
;; Copyright (C) 2007,2008,2009,2010 Alex Harsanyi

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Author: Alex Harsanyi (AlexHarsanyi@gmail.com)
;; Created: 25 Nov 2007
;; Keywords: version-control, clearcase
;; Homepage: http://code.google.com/p/vc-clearcase/

;;; Commentary
;; This implements support for ClearCase UCM.  The user functions
;; `ucm-set-activity', `ucm-show-current-activity', `ucm-browse-activity' and
;; `ucm-checkin-activity'.  See their doc strings for details.  As of now,
;; these functions are not bound to keys and menus, so you need to invoke them
;; using M-x (`execute-extended-command').

;;; TODO
;;
;; - `ucm-actb-fetch-activity' -- the listed revisions should be sorted,
;;   otherwise they might show up in the wrong order (as received from
;;   clearcase)
;;
;; - `ucm-actb-mode' -- allow starting ediff in addition to diff for a
;;   revision.
;;
;; - `ucm-actb-transfer-revisions-command' -- handle the *NONE* and
;;   *NEW-ACTIVITY* responses from `ucm-read-activity'
;;

;;; Code

(require 'cl)
(require 'vc-clearcase)
(require 'button)
(require 'ewoc)

(defgroup ucm nil
  "Support for UCM under ClearCase"
  :group 'vc-clearcase
  :link '(url-link "http://code.google.com/p/vc-clearcase/wiki/UcmSupport"))

;;;; faces
(defface ucm-field-name-face
    '((t (:inherit font-lock-builtin-face)))
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

(defface ucm-locked-activity-face
    '((t (:inherit font-lock-function-name-face)))
  "Face used for locked activities in UCM activity list buffers."
  :group 'ucm)

(defface ucm-obsolete-activity-face
    '((t (:inherit font-lock-comment-face)))
  "Face used for obsolete activities in UCM activity list buffers."
  :group 'ucm)

(defface ucm-set-activity-face
    '((t (:inherit font-lock-constant-face)))
  "Face used for the activity that is set in the current view in
UCM activity list buffers."
  :group 'ucm)

;; It used to be that the name "/projects" was recognized on both windows and
;; unix systems.  It seems that in CC 7, only "\\projects" is recognized
(defcustom ucm-projects-vob
  (if (eq system-type 'windows-nt)
      "\\projects"
      "/projects")
  "The name of the UCM projects VOB."
  :group 'ucm
  :type 'string)

;;;; ucm-read-activity
(defun ucm-read-activity (prompt &optional include-obsolete initial view-tag)
  "Display PROMPT and read a UCM activity with completion.

VIEW-TAG is the view from which activities are read -- when nil,
the view is determined from `default-directory' (which is the
current directory).

When INCLUDE-OBSOLETE is not nil, obsolete activities are also
included in the list.

INITIAL is the initial activity name presented to the user.  When
nil, a default activity is selected.  In `ucm-actb-mode' and
`ucm-lsact-mode', the current activity is used, in all other
places the current activity in the view is presented to the user.
When it is a symbol, no default activity is presented to the
user."
  (with-cleartool-directory (expand-file-name default-directory)
    (unless view-tag
      (setq view-tag
	    (replace-regexp-in-string
	     "[\n\r]+" "" (cleartool "pwv -short"))))
    (unless initial
      (setq initial
            (cond ((eq major-mode 'ucm-actb-mode)
                   ucm-activity)
                  ((eq major-mode 'ucm-lsact-mode)
                   (let ((node (ewoc-locate ucm-lsact-ewoc (point))))
                     (when node
                       (let ((data (ewoc-data node)))
                         (ucm-lsact-activity-name data)))))
                  (t
                   (ignore-cleartool-errors
                     (cleartool-ask "lsact -cact -fmt \"%n\"")))))))

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
		(format "lsact %s -fmt \"%%n\\n\" -me -view %s"
			(if include-obsolete "-obsolete" "")
			(clearcase-vprop-name vprop))
		'nowait
		vprop
		(lambda (vprop answer)
		  (setf (clearcase-vprop-activities vprop)
			(append (list "*NONE*" "*NEW-ACTIVITY*")
				(split-string answer "[\n\r]+" t)))))))
      (setf (clearcase-vprop-activities-tid vprop) tid))

    (prog1
	(completing-read
	 prompt
	 (lambda (string predicate flag)
	   (let ((fn (cond ((eq flag t) 'all-completions)
                           ((eq flag 'lambda) 'test-completion)
                           (t 'try-completion))))
	     (funcall
	      fn string (clearcase-vprop-activities vprop) predicate)))
	 nil
	 'require-match
	 (when (stringp initial) initial))
      (cleartool-wait-for (clearcase-vprop-activities-tid vprop)))))

;;;; ucm-set-activity

;;;###autoload
(defvar ucm-create-activity-function nil
  "Function to call for creating new activities.
When nil, `ucm-set-activity' will create a new activity by
prompting for a headline and using mkact.")

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
  (interactive (list (ucm-read-activity "Set activity: " nil 'no-default)))
  (with-cleartool-directory (expand-file-name default-directory)
    (cond
      ((equal activity "*NONE*")
       (cleartool "setact -none"))
      ((equal activity "*NEW-ACTIVITY*")
       (if ucm-create-activity-function
	   (call-interactively ucm-create-activity-function)
	   (let ((headline (read-from-minibuffer "Activity headline: ")))
	     (when (equal headline "")
	       (error "Activity headline cannot be empty"))
	     (cleartool "mkact -force -headline \"%s\"" headline))))
      (t
       (let ((status (cleartool "lsact -fmt \"%%[locked]p\" %s" activity)))
	 (when (equal status "locked")
	   (if (y-or-n-p "Activity is locked.  Would you like to unlock it? ")
	       (progn
		 (cleartool "unlock activity:%s@%s" activity ucm-projects-vob)
		 (message "Activity %s unlocked" activity))
	       (error "Cannot set this activity beacuse it is locked.")))
	 (cleartool "setact \"%s\"" activity))))))

;;;; ucm-show-current-activity
;;;###autoload
(defun ucm-show-current-activity (&optional extra-info)
  "Show the current activity in the view.
With prefix argument (EXTRA-INFO), also shows the number of
files modified under this activity, number of revisions and the
number of checked out files."
  (interactive "P")
  (with-cleartool-directory (expand-file-name default-directory)
    (let ((headline (cleartool "lsact -cact -fmt \"%%[headline]p\""))
	  revisions
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
		  (setq revisions
			(split-string
			 (cleartool "lsact -cact -fmt \"%%[versions]Cp\"") ", " t))
		  (setq files (make-hash-table :test 'equal))
		  (dolist (v revisions)
		    (when (string-match "CHECKEDOUT\\(\.[0-9]+\\)?$" v)
		      (incf checkouts))
		    (when (string-match "^\\(.*\\)@@" v)
		      (setf (gethash (match-string 1 v) files) t)))))
	      (if extra-info
		  (message "%s; %d files, %d revisions, %d checked-out"
			   headline (hash-table-count files) (length revisions) checkouts)
		  (message "%s" headline)))))))

;;;; ucm-delete-activity
;;;###autoload
(defun ucm-delete-activity (activity)
  "Remove ACTIVITY from UCM ClearCase.
An activity can only be removed if it contains no versions, it is
not the current activity and it is not locked."
  (interactive (list (ucm-read-activity "Delete activity: ")))
  (cleartool "rmact -force -nc activity:%s@%s" activity ucm-projects-vob))

;;;; ucm-list-activities

;; Browse the list of activities in a stream

(defvar ucm-lsact-ewoc nil
  "The EWOC structure containing activities displayed in the list
  activity buffer.")
(defvar ucm-lsact-only-current-user nil
  "When not nil, only the current users activities are shown in
  the list activity buffer.")
(defvar ucm-lsact-include-obsolete nil
  "When not nil, obsolete activities are shown in the list
  activity buffer.")

(defun ucm-lsact-activity-link-handler (button)
  "Browse the activity linked to this BUTTON."
  (pop-to-buffer (button-get button 'buffer))
  (ucm-browse-activity (button-get button 'activity)))

(define-button-type 'ucm-lsact-activity-link
    'face 'default
    'help-echo "mouse-2, RET: Browse this activity"
    'action 'ucm-lsact-activity-link-handler
    'follow-link t
    'skip t)

(defstruct ucm-lsact-activity
  name
  headline
  set?                                  ; is this activity set in this view?
  mark
  owner
  lock
  attributes                            ; an alist (name . value)
  )

(defvar ucm-lsact-pp-attributes '()
  "A list of extra attribute names to be displayed for each activity")

(defun ucm-lsact-activity-pp (activity)
  "Pretty print ACTIVITY (an `ucm-lsact-activity' structure).
This is used by ewoc to show activities in the list activity
buffer."
  (let ((face (cond ((ucm-lsact-activity-set? activity)
		     'ucm-set-activity-face)
		    ((equal (ucm-lsact-activity-lock activity) "obsolete")
		     'ucm-obsolete-activity-face)
		    ((equal (ucm-lsact-activity-lock activity) "locked")
		     'ucm-locked-activity-face)
		    (t 'default)))
	(label1 (format "%c%c "
			(if (ucm-lsact-activity-set? activity) ?! ?\  )
			(if (ucm-lsact-activity-mark activity) ?* ?\  )))
	(label2 (format "%s"
			(if (equal (ucm-lsact-activity-lock activity) "unlocked")
			    ""
			   (ucm-lsact-activity-lock activity))))
	(attr (mapconcat
	       (lambda (a)
		 (let ((v (assq a (ucm-lsact-activity-attributes activity))))
		   (format "%s" (or (cdr v) ""))))
	       ucm-lsact-pp-attributes " ")))
    (insert label1)
    (insert-text-button
     (ucm-lsact-activity-headline activity)
     'type 'ucm-lsact-activity-link
     'face face
     'buffer (current-buffer)
     'activity (ucm-lsact-activity-name activity))
    (when (not (and (equal label2 "") (equal attr "")))
      (insert " (" label2)
      (unless (equal label2 "")
	(insert " "))
      (insert attr ")"))))

(defun ucm-lsact-create-ewoc (&optional current-user obsolete)
  "Create an EWOC from the activities in STREAM.
When CURRENT-USER is not nil, only the current user's activities
are listed.  When OBSOLETE is not nil, obsolete activities are
inclued as well."
  (let* ((stream (cleartool "lsstream -fmt \"%%n\""))
	 (header (concat (propertize "Stream: " 'face 'ucm-field-name-face)
			 stream))
	 (current-activity (cleartool "lsact -cact -fmt \"%%n\""))
	 (ewoc (ewoc-create 'ucm-lsact-activity-pp header)))
    (dolist (a (split-string
		(cleartool
		 "lsact %s %s -fmt \"%%n %%[owner]p %%[locked]p\\n\""
		 (if current-user "-me" "")
		 (if obsolete "-obsolete" ""))
		"[\n\r]" 'omit-nulls))
      (destructuring-bind (name owner lock) (split-string a)
	(ewoc-enter-last ewoc
			 (make-ucm-lsact-activity
			  :name name
			  :headline (cleartool "lsact -fmt \"%%[headline]p\" %s" name)
			  :set? (equal name current-activity)
			  :owner owner
			  :lock lock
			  :attributes (clearcase-get-attributes
				       (format "activity:%s@%s" name ucm-projects-vob))))))
    ewoc))


(defun ucm-lsact-refresh-command ()
  "Refresh the activity list in the current buffer."
  (interactive)
  (with-temp-message "Preparing activity list..."
    (with-cleartool-directory (expand-file-name default-directory)
      (let ((inhibit-read-only t))
	(erase-buffer)
	(setq ucm-lsact-ewoc
	      (ucm-lsact-create-ewoc
	       ucm-lsact-only-current-user
	       ucm-lsact-include-obsolete)))
      (ewoc-refresh ucm-lsact-ewoc)
      (goto-char (point-min)))))

(defun ucm-lsact-toggle-mark-command (pos)
  "Toggle the mark on the current activity."
  (interactive "d")
  (let ((node (ewoc-locate ucm-lsact-ewoc pos)))
    (when node
      (let ((data (ewoc-data node)))
	(setf (ucm-lsact-activity-mark data)
	      (not (ucm-lsact-activity-mark data))))
      (ewoc-invalidate ucm-lsact-ewoc node)
      (let ((next (ewoc-next ucm-lsact-ewoc node)))
	(when next
	  (ewoc-goto-node ucm-lsact-ewoc next))))))

(defun ucm-lsact-set-activity-command (pos)
  "Set the activity under the cursor."
  (interactive "d")
  (let ((node (ewoc-locate ucm-lsact-ewoc pos)))
    (when node
      (let ((data (ewoc-data node)))
	(ewoc-map (lambda (d)
		    (cond ((eq d data)
			   (ucm-set-activity (ucm-lsact-activity-name d))
			   (setf (ucm-lsact-activity-set? d) t)
			   t)
			  ;; This is the previously set activity, unset it
			  ((ucm-lsact-activity-set? d)
			   (setf (ucm-lsact-activity-set? d) nil)
			   ;; return t so the display is updated
			   t)
			  (t nil)))
		  ucm-lsact-ewoc)))))

(defun ucm-lsact-unlock-activity-command (pos)
  "Unlock the selected activities, or the current one"
  (interactive "d")
  (let ((unlock-count 0))
    (ewoc-map (lambda (d)
		(when (ucm-lsact-activity-mark d)
		  (ucm-unlock-activity (ucm-lsact-activity-name d))
		  (setf (ucm-lsact-activity-lock d) "")
		  (incf unlock-count)
		  t                     ; return t so display is updated
		  ))
	      ucm-lsact-ewoc)
    (when (= unlock-count 0)
      ;; no activities were marked, try to unlock the current activity
      (let ((node (ewoc-locate ucm-lsact-ewoc pos)))
	(when node
	  (let ((data (ewoc-data node)))
	    (ucm-unlock-activity (ucm-lsact-activity-name data))
	    (setf (ucm-lsact-activity-lock data) "")
	    (ewoc-invalidate ucm-lsact-ewoc node)))))))

(defvar ucm-lsact-mode-map
  (let ((m (make-sparse-keymap)))
    (suppress-keymap m)
    (define-key m "m" 'ucm-lsact-toggle-mark-command)
    (define-key m "g" 'ucm-lsact-refresh-command)
    (define-key m "s" 'ucm-lsact-set-activity-command)
    (define-key m "u" 'ucm-lsact-unlock-activity-command)
    m))

(define-derived-mode ucm-lsact-mode fundamental-mode
  "UCM Activity List"
  (buffer-disable-undo)
  (setq buffer-read-only t)
  (set (make-local-variable 'ucm-lsact-ewoc) nil)
  (set (make-local-variable 'ucm-lsact-only-current-user) nil)
  (set (make-local-variable 'ucm-lsact-include-obsolete) nil))

;;;###autoload
(defun ucm-list-activities (&optional dir)
  (interactive "DDirectory: ")
  (unless dir
    (setq dir "."))
  (setq dir (expand-file-name dir))
  (with-cleartool-directory dir
    (let ((buf (get-buffer-create "*UCM Activity List*")))
      (pop-to-buffer buf)
      (ucm-lsact-mode)
      (setq ucm-lsact-only-current-user (not current-prefix-arg))
      (setq ucm-lsact-include-obsolete (or (consp current-prefix-arg)
					   (numberp current-prefix-arg)))
      (let ((inhibit-read-only t))
	(setq default-directory dir)
	(erase-buffer)
	(ucm-lsact-refresh-command)))))

;;;; ucm-browse-activity
;;;;; buffer-local variables
(defvar ucm-activity nil
  "The name of the current activity being browsed.")

(defvar ucm-previous-activities '()
  "A stack of previous activities we visited.
Used to implement the BACK button.")

(defvar ucm-view-root nil
  "The view root directory for the activity browser buffer")

(defvar ucm-actb-ewoc nil
  "The ewoc structure for the activity browser buffer")

(defvar ucm-previous-actb-ewocs '()
  "A stack ewocs for previous activities we visited.
Used to implement the BACK button.")

;;;;; buttons

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
    (push ucm-actb-ewoc ucm-previous-actb-ewocs)
    (setq ucm-activity (button-get button 'ucm-activity))
    (ucm-actb-refresh-command)))

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
      (let ((inhibit-read-only t))
	(setq ucm-activity (pop ucm-previous-activities))
	(setq ucm-actb-ewoc (pop ucm-previous-actb-ewocs))
	(erase-buffer)
	;; The header and footer will not be redisplayed unless we set them
	;; again.
	(let ((hf (ewoc-get-hf ucm-actb-ewoc)))
	  (ewoc-set-hf ucm-actb-ewoc (car hf) (cdr hf)))
	(ewoc-refresh ucm-actb-ewoc)
	(goto-char (point-min))))))

(define-button-type 'ucm-previous-activity-link
    'face 'default
    'help-echo "mouse-2, RET: Browse previous activity"
    'action 'ucm-previous-activity-link-handler
    'follow-link t
    'skip t)

(defun ucm-show-diff-link-handler (button)
  (with-current-buffer (button-get button 'buffer)
    (pop-to-buffer (current-buffer))
    (pop-to-buffer (ucm-actb-show-diff (button-get button 'actb-revision)))))

(define-button-type 'ucm-show-diff-link
    'face 'default
    'help-echo "mouse-2, RET: Show diff"
    'action 'ucm-show-diff-link-handler
    'follow-link t
    'skip t)

;;;;; data structures
;;;;;; ucm-actb-activity

;; Hold revision and contributor information about an activity.  See
;; `ucm-actb-fetch-activity' for how an instance of this structure is
;; constructed.
;;
;; An activity is a collection of file revisions, but we represent it as a
;; hierarch of DIRECTORY -> FILE -> REVISION: Each file revision is represented
;; by a `ucm-actb-revision' instance.  The revisions for a file are grouped
;; under a `ucm-actb-file' instance.  All files under a directory are grouped
;; under a `ucm-actb-directory' instance.

(defstruct ucm-actb-activity
  name
  ;; a list of `ucm-actb-directory' structures, one for each directory in
  ;; which this activity has file revisions
  directories

  ;; a list of `ucm-actb-contributor' structures.  The empty list if the
  ;; activity is not a rebase or deliver activity.
  contributors)

;;;;;; ucm-actb-directory

;; Hold files which have revisions under the current activity and are in the
;; same directory.

(defstruct ucm-actb-directory
  name                                  ; the directory name
  files)                                ; a list of `ucm-actb-file' structures

(defun ucm-actb-directory-pp (directory)
  "Pretty print an `ucm-actb-directory' structure."
  (insert "\nIn directory `")
  (let ((dir (file-relative-name
	      (ucm-actb-directory-name directory) ucm-view-root)))
    (insert-text-button
     dir
     'face 'font-lock-function-name-face
     'type 'ucm-file-link
     'buffer (current-buffer)
     'file-name (ucm-actb-directory-name directory)))
  (insert "':\n"))

(defun ucm-actb-directory-toggle-mark (directory)
  "Toggle the marks on all revisions under this DIRECTORY.
The toggle works like this: if all revisions are marked, the
marks are cleared, if any revision is unmarked, the marks are
set."
  (let* ((files (ucm-actb-directory-files directory))
	 (marks (mapcar (lambda (f)
			  (some 'ucm-actb-revision-mark
				(ucm-actb-file-revisions f)))
			files)))
    (cond ((every 'identity marks)
	   (mapc 'ucm-actb-file-clear-mark files))
	  (t
	   (mapc 'ucm-actb-file-set-mark files)))))

;;;;;; ucm-actb-file

;; Hold together the revisions of a file which are part of an activity.

(defstruct ucm-actb-file
  name                         ; the name of the file, sans directory
  directory                    ; the `ucm-actb-directory' which we are part of
  revisions)                    ; a list of `ucm-actb-revision' structures

(defun ucm-actb-file-full-path (f)
  "Return the full path of F (an `ucm-actb-file' structure)."
  (let ((dir (ucm-actb-directory-name (ucm-actb-file-directory f))))
    (expand-file-name (ucm-actb-file-name f) dir)))

(defun ucm-actb-file-pp (file)
  "Pretty print FILE, an `ucm-actb-file' structure."
  (insert "    ")
  (insert-text-button
   (ucm-actb-file-name file)
   'face 'ucm-file-name-face
   'type 'ucm-file-link
   'buffer (current-buffer)
   'file-name (ucm-actb-file-full-path file)))

(defun ucm-actb-file-set-mark (file)
  "Set the mark on all revisions of FILE"
  (mapc 'ucm-actb-revision-set-mark (ucm-actb-file-revisions file)))

(defun ucm-actb-file-clear-mark (file)
  "Clear the mark from all revisions of FILE"
  (mapc 'ucm-actb-revision-clear-mark (ucm-actb-file-revisions file)))

(defun ucm-actb-file-toggle-mark (file)
  "Toggle the mark for the revisions of FILE.
The toggle works like this: if all revisions are marked, the
marks are cleared, if any revision is unmarked, the marks are
set."
  (let ((revisions (ucm-actb-file-revisions file)))
    (cond ((notevery 'ucm-actb-revision-mark revisions)
	   (mapc 'ucm-actb-revision-set-mark revisions))
	  (t
	   (mapc 'ucm-actb-revision-toggle-mark revisions)))))

;;;;;; ucm-actb-revision

;; The revision of a file under an activity.
(defstruct ucm-actb-revision
  name
  ;; the previous revision against which we will diff.  Don't use this slot
  ;; directly.  Use `ucm-actb-revision-previous' instead.
  previous^

  ;; number of revisions between name and previous^
  (count 1)

  file                                  ; The `ucm-actb-file' we belong to
  mark                                  ; (t nil)
  ;; Becomes t when the mark has changed.  Used to know when the ewoc node
  ;; should be refreshed.  It is a bit of a hack.
  changed)

(defun ucm-actb-revision-pp (revision)
  "Pretty print REVISION, a `ucm-actb-revision' structure"
  (if (ucm-actb-revision-mark revision)
      (insert "      * ")
      (insert "        "))
  (let ((file (ucm-actb-file-full-path (ucm-actb-revision-file revision)))
	(rev (ucm-actb-revision-name revision)))
    (insert-text-button
     (concat "@@" rev)
     'face (if (ucm-actb-revision-checkedout-p revision)
	       'ucm-checkedout-revision-face
	       'ucm-revision-face)
     'type 'ucm-show-diff-link
     'buffer (current-buffer)
     'actb-revision revision))
  (if (> (ucm-actb-revision-count revision) 1)
      (insert (format " (%d revisions)" (ucm-actb-revision-count revision)))))


(defun ucm-actb-revision-set-mark (revision)
  "Set the mark on REVISION."
  (let ((mark (ucm-actb-revision-mark revision)))
    (unless mark
      (setf (ucm-actb-revision-mark revision) t)
      (setf (ucm-actb-revision-changed revision) t))))

(defun ucm-actb-revision-clear-mark (revision)
  "Clear the mark on REVISION."
  (let ((mark (ucm-actb-revision-mark revision)))
    (when mark
      (setf (ucm-actb-revision-mark revision) nil)
      (setf (ucm-actb-revision-changed revision) t))))

(defun ucm-actb-revision-toggle-mark (revision)
  "Toggle the mark on REVISION."
  (setf (ucm-actb-revision-mark revision)
	(not (ucm-actb-revision-mark revision)))
  (setf (ucm-actb-revision-changed revision) t))

(defun ucm-actb-revision-checkedout-p (revision)
  "Return t if REVISION is a checkout."
  (string-match "[\\/]CHECKEDOUT\\(.[0-9]+\\)\\'"
		(ucm-actb-revision-name revision)))

(defun ucm-actb-revision-pname (revision)
  "Return the revision extended path name of REVISION."
  (let ((file (ucm-actb-file-full-path (ucm-actb-revision-file revision))))
    (concat file "@@" (ucm-actb-revision-name revision))))

(defun ucm-actb-revision-previous (revision)
  "Return the previous revision from this one.
By default we return the immediate predecessor of this revision,
but we might collapse several revisions into one, in which case
the previous revision will be an older one"
  (if (ucm-actb-revision-previous^ revision)
      (ucm-actb-revision-previous^ revision)
      (setf (ucm-actb-revision-previous^ revision)
	    (cleartool "desc -fmt \"%%PVn\" \"%s\""
		       (if (string-match "\\<CHECKEDOUT\\(.[0-9]+\\)?"
					 (ucm-actb-revision-name revision))
			   (ucm-actb-file-full-path (ucm-actb-revision-file revision))
			   (ucm-actb-revision-pname revision))))))

(defun ucm-actb-show-diff (revision)
  "Pop up a buffer containing the diff of this revision"
  (let ((file (ucm-actb-file-full-path (ucm-actb-revision-file revision)))
	(current (ucm-actb-revision-name revision))
	(previous (ucm-actb-revision-previous revision)))
    (when (string-match "\\<CHECKEDOUT\\(.[0-9]+\\)?" current)
      (setq current nil))
    (vc-clearcase-registered file)      ; may create a FPROP if needed
    (vc-clearcase-diff (list file) previous current)
    (get-buffer "*vc-diff*")))

(defun ucm-actb-file-optimize-revisions (file)
  "Reduce the number of revisions of a file by colapsing
 consecutive revisions into one."
  (let ((revisions (reverse (ucm-actb-file-revisions file)))
        (optimized '()))
    (while revisions
      (let ((first (car revisions))
            (rest (cdr revisions)))
        ;; Never merge checked out versions with predecessors.
        (unless (string-match "\\<CHECKEDOUT\\(.[0-9]+\\)?"
                              (ucm-actb-revision-name first))
          (catch 'next
            (while t
              (let ((prev (find (ucm-actb-revision-previous first)
                                rest
                                :test 'equal
                                :key 'ucm-actb-revision-name)))
                (if prev
                    (progn
                      (setf (ucm-actb-revision-previous^ first)
                            (ucm-actb-revision-previous prev))
                      (incf (ucm-actb-revision-count first))
                      (setf rest (delq prev rest)))
                    (throw 'next nil))))))
        (push first optimized)
        (setf revisions rest)))
    (setf (ucm-actb-file-revisions file) optimized)))

;;;;;; ucm-actb-contributor

(defstruct ucm-actb-contributor
  name)                                 ; the contributor activity name

(defun ucm-actb-contributor-pp (contributor)
  "Pretty print CONTRIBUTOR, an `ucm-actb-contributor' structure."
  (insert "    ")
  (insert-text-button
   (ucm-actb-contributor-name contributor)
   'type 'ucm-activity-link
   'buffer (current-buffer)
   'ucm-activity (ucm-actb-contributor-name contributor)))

;;;;;; generic operations

(defun ucm-actb-pp (data)
  "Pretty print DATA, which can be any data structure we attach
to the activity ewoc."
  (typecase data
    (symbol
     (when (eq data 'back-button)
       (insert "\n\n")
       (insert-text-button
	"[back]"
	'type 'ucm-previous-activity-link
	'buffer (current-buffer))))
    (string
     (insert "\n" (propertize data 'face 'ucm-field-name-face) "\n"))
    (ucm-actb-directory (ucm-actb-directory-pp data))
    (ucm-actb-file (ucm-actb-file-pp data))
    (ucm-actb-revision (ucm-actb-revision-pp data))
    (ucm-actb-contributor (ucm-actb-contributor-pp data))
    (t (error "Unknown data type %S" data))))

(defun ucm-actb-toggle-mark (data)
  "Toggle the mark on DATA, which can be any data structure we
attach to the activity ewoc."
  (typecase data
    (ucm-actb-directory (ucm-actb-directory-toggle-mark data))
    (ucm-actb-file (ucm-actb-file-toggle-mark data))
    (ucm-actb-revision (ucm-actb-revision-toggle-mark data))
    (t nil)))

;;;;; ucm-actb-fetch-activity
(defun ucm-actb-fetch-activity (activity)
  "Retrieve information about ACTIVITY and construct an
`ucm-actb-activity' instance from it."
  (let ((dirs '())
	(contributors '()))
    (dolist (v (split-string
		(cleartool "lsact -fmt \"%%[versions]Cp\" %s" activity)
		", " 'omit-nulls))
      (when (string-match "\\(.*\\)@@\\(.*\\)" v)

	(let ((file (match-string 1 v))
	      (revision (match-string 2 v)))

	  (let ((dir (file-name-directory file))
		(basename (file-name-nondirectory file)))

	    (let ((d (find dir dirs
			   :key 'ucm-actb-directory-name :test 'string=)))
	      (unless d
		(setq d (make-ucm-actb-directory :name dir))
		(push d dirs))

	      (let ((f (find basename (ucm-actb-directory-files d)
			     :key 'ucm-actb-file-name :test 'string=)))
		(unless f
		  (setq f (make-ucm-actb-file :name basename :directory d))
		  (push f (ucm-actb-directory-files d)))

		(push (make-ucm-actb-revision :name revision :file f)
		      (ucm-actb-file-revisions f)))
	      )))))

    (ignore-cleartool-errors
      ;; There seems to be a bug in my revision of ClearCase: if `activity' is
      ;; not a rebase or integration activity an error will be reported, but
      ;; the status of the command will be 0 (meaning success).  We have to
      ;; test the returned string explicitly ...
      (let ((ca (cleartool "lsact -fmt \"%%[contrib_acts]p\" %s" activity)))
	(when (and ca (not (string-match "^cleartool: Error: " ca)))
	  (dolist (c (sort (split-string ca " " 'omit-nulls) 'string<))
	    (push (make-ucm-actb-contributor :name c) contributors)))))

    (setq contributors (nreverse contributors))

    ;; Sort the revisions of each file.  Most of the time, revisions come out
    ;; sorted, but they don't if revisions were transferred from one activity
    ;; to another.
    (dolist (d dirs)
      (dolist (f (ucm-actb-directory-files d))
        (setf (ucm-actb-file-revisions f)
              (sort (ucm-actb-file-revisions f)
                    (lambda (a b)
                      (let ((a-rev (ucm-actb-revision-name a))
                            (b-rev (ucm-actb-revision-name b))
                            a-branch a-revision b-branch b-revision)
                        (when (string-match "^\\(.*\\)[\\\/]\\([0-9]+\\)$" a-rev)
                          (setq a-branch (match-string 1 a-rev)
                                a-revision (string-to-number (match-string 2 a-rev))))
                        (when (string-match "^\\(.*\\)[\\\/]\\([0-9]+\\)$" b-rev)
                          (setq b-branch (match-string 1 b-rev)
                                b-revision (string-to-number (match-string 2 b-rev))))
                        (or (string< a-branch b-branch)
                            (and (string= a-branch b-branch)
                                 (< a-revision b-revision)))))))))

    ;; Sort the files inside each directory
    (dolist (d dirs)
      (setf (ucm-actb-directory-files d)
	    (sort (ucm-actb-directory-files d)
		  (lambda (a b)
		    (string< (ucm-actb-file-name a)
			     (ucm-actb-file-name b))))))

    ;; Sort the directories
    (setq dirs
	  (sort dirs (lambda (a b)
		       (string< (ucm-actb-directory-name a)
				(ucm-actb-directory-name b)))))

    (make-ucm-actb-activity :name activity
			    :directories dirs
			    :contributors contributors)))

;;;;; ucm-actb-create-ewoc
(defun ucm-actb-create-ewoc (activity)
  "Create an ewoc structure for ACTIVITY (an `ucm-actb-activity'
structure)"
  (let* ((name (ucm-actb-activity-name activity))
	 (header
	  (concat (propertize "Activity: " 'face 'ucm-field-name-face)
		  "\""
		  (cleartool "lsact -fmt \"%%[headline]p\" %s" name)
		  "\"\n"
		  (propertize "Name: " 'face 'ucm-field-name-face)
		  name
		  " ("
		  (cleartool "lsact -fmt \"%%[locked]p\" %s" name)
		  ", "
		  (cleartool "lsact -fmt \"%%[owner]p\" %s" name)
		  ")\n"
		  (propertize "Stream: " 'face 'ucm-field-name-face)
		  (cleartool "lsact -fmt \"%%[stream]p\" %s" name)
		  "\n"
		  (propertize "Attributes: " 'face 'ucm-field-name-face)
		  "\n"))
	 (ewoc nil))

    (loop for (attribute . value)
       in (clearcase-get-attributes (format "activity:%s@%s" name ucm-projects-vob))
       do (setq header (concat header (format "\t%s = %s\n" (symbol-name attribute) value))))

    (setq ewoc (ewoc-create 'ucm-actb-pp header))

    (dolist (d (ucm-actb-activity-directories activity))
      (ewoc-enter-last ewoc d)
      (dolist (f (ucm-actb-directory-files d))
	(ewoc-enter-last ewoc f)
	(dolist (v (ucm-actb-file-revisions f))
	  (ewoc-enter-last ewoc v))))
    (let ((contributors (ucm-actb-activity-contributors activity)))
      (when (> (length contributors) 0)
	(ewoc-enter-last ewoc "Contributors")
	(dolist (c contributors)
	  (ewoc-enter-last ewoc c))))

    (when ucm-previous-actb-ewocs
      (ewoc-enter-last ewoc 'back-button))

    ewoc))

;;;;; ucm-actb-mode

(defvar ucm-actb-mode-map
  (let ((m (make-sparse-keymap)))
    (suppress-keymap m)
    (define-key m "m" 'ucm-actb-toggle-mark-command)
    (define-key m "g" 'ucm-actb-refresh-command)
    (define-key m "c" 'ucm-actb-checkin-command)
    (define-key m "r" 'ucm-actb-revert-command)
    (define-key m "t" 'ucm-actb-transfer-revisions-command)
    (define-key m "v" 'ucm-actb-visit-item-command)
    (define-key m "e" 'ucm-actb-ediff-command)
    (define-key m "O" 'ucm-actb-optimize-display-command)
    m))

(define-derived-mode ucm-actb-mode fundamental-mode
  "UCM Activity Browser"
  (buffer-disable-undo)
  (setq buffer-read-only t)
  (set (make-local-variable 'ucm-activity) nil)
  (set (make-local-variable 'ucm-view-root) nil)
  (set (make-local-variable 'ucm-actb-ewoc) nil)
  (set (make-local-variable 'ucm-previous-actb-ewocs) nil))


;;;;;; ucm-actb-collect-marked-checkouts
(defun ucm-actb-collect-marked-checkouts ()
  "Return a list of files which have marked checked out revisions.
If no checked out revisions are marked return the file under
cursor if it is checked out.  Signal an error otherwise."
  ;; First, try to get the selected checked out revisions

  (let ((marked-revisions (ewoc-collect ucm-actb-ewoc
				       '(lambda (data)
					 (and
					  (ucm-actb-revision-p data)
					  (ucm-actb-revision-mark data)))))
	(files '()))
    (if marked-revisions
	(progn
	  (dolist (v marked-revisions)
	    (when (ucm-actb-revision-checkedout-p v)
	      (push (ucm-actb-file-full-path (ucm-actb-revision-file v)) files)))
	  (unless files
	    (error "No checkouts selected.")))

	;; If no revisions were selected, try to see if the current file has a
	;; checkout.
	(let ((data (ewoc-data (ewoc-locate ucm-actb-ewoc))))
	  (typecase data
	    (ucm-actb-revision
	     (if (ucm-actb-revision-checkedout-p data)
		 (push (ucm-actb-file-full-path (ucm-actb-revision-file data)) files)
		 (error "Revision is not checked out.")))
	    (ucm-actb-file
	     (if (some 'ucm-actb-revision-checkedout-p (ucm-actb-file-revisions data))
		 (push (ucm-actb-file-full-path data) files)
		 (error "File has no checkouts.")))
	    (t
	     (error "Must select a file or checked out revision.")))))
    files))

;;;;;; ucm-actb-toggle-mark-command
(defun ucm-actb-toggle-mark-command (pos)
  "Toggle the mark on the current item.
If the current item is a revision the mark is changed (set if it
is unset and cleared if it is set).  For files or directories, if
all revisions under them are marked, the marks are cleared,
otherwise the marks are set."
  (interactive "d")
  (let ((node (ewoc-locate ucm-actb-ewoc pos)))
    (when node
      (ucm-actb-toggle-mark (ewoc-data node))
      (ewoc-map
       (lambda (data)
	 (when (and (ucm-actb-revision-p data)
		    (ucm-actb-revision-changed data))
	   (setf (ucm-actb-revision-changed data) nil)
	   t))
       ucm-actb-ewoc)
      (ewoc-goto-node ucm-actb-ewoc node)
      ;; move to the next node of the same type -- we rely on an
      ;; implementation detail for finding the type of a node data
      (when (arrayp (ewoc-data node))
	(loop
	   with node-type = (aref (ewoc-data node) 0)
	   for n = (ewoc-next ucm-actb-ewoc node) then (ewoc-next ucm-actb-ewoc n)
	   while n
	   until (let ((d (ewoc-data n)))
		   (and (arrayp d) (eq node-type (aref d 0))))
	   finally (when n (ewoc-goto-node ucm-actb-ewoc n)))))))

;;;;;; ucm-actb-refresh-command
(defun ucm-actb-refresh-command ()
  "Refresh the activity in the current buffer."
  (interactive)
  (with-temp-message "Preparing activity report..."
    (with-cleartool-directory (expand-file-name default-directory)
      (let ((a (ucm-actb-fetch-activity ucm-activity))
	    (inhibit-read-only t))
	(erase-buffer)
	(setq ucm-actb-ewoc (ucm-actb-create-ewoc a))
	(ewoc-refresh ucm-actb-ewoc)
	(goto-char (point-min))))))

;;;;;; ucm-actb-checkin-command
(defun ucm-actb-checkin-command ()
  "Checkin selected revisions from the UCM activity buffer.
If no revisions are selected, the current revision is checked in."
  (interactive)
  (lexical-let ((buf (current-buffer))
		(modified-files (clearcase-revert-unchanged-files 
                                 (ucm-actb-collect-marked-checkouts)))
		(dir default-directory)
		(window-configuration (current-window-configuration)))

    (when (null modified-files)
      (error "No files to checkin."))

    (log-edit (lambda ()
		(interactive)
		(with-cleartool-directory dir
		  (let ((comment-text
                         (buffer-substring-no-properties (point-min) (point-max))))
		    (vc-clearcase-checkin modified-files nil comment-text)))
		(clearcase-refresh-files modified-files)
		(with-current-buffer buf
		  (ucm-actb-refresh-command))
		(set-window-configuration window-configuration))
	      'setup
	      `((log-edit-listfun
                 . ,(lambda ()
                            (interactive)
                            (mapcar 'file-relative-name modified-files)))
                (log-edit-diff-function
                 . ,(lambda ()
                            (interactive)
                            (with-current-buffer (get-buffer-create "*vc-diff*")
                              (erase-buffer)
                              (diff-mode)
                              (setq buffer-read-only t)
                              (vc-clearcase-diff modified-files nil nil (current-buffer))
                              (pop-to-buffer (current-buffer))))))
	      (get-buffer-create "*UCM-Checkin-Log*"))))

;;;;;; ucm-actb-revert-command
(defun ucm-actb-revert-command ()
  "Revert checkout on selected files"
  (interactive)
  (let ((files (ucm-actb-collect-marked-checkouts)))
    (dolist (file files)
      (message "Reverting %s" (file-relative-name file))
      (when (vc-clearcase-registered file) ; ensure a FPROP exists!
        (vc-clearcase-revert file))
    (clearcase-refresh-files files))
  (ucm-actb-refresh-command)))

;;;;;; ucm-actb-transfer-revisions-command
(defun ucm-actb-transfer-revisions-command ()
  "Transfer the selected revisions to another activity."
  (interactive)
  (let ((revisions (ewoc-collect ucm-actb-ewoc
				 '(lambda (data)
				   (and
				    (ucm-actb-revision-p data)
				    (ucm-actb-revision-mark data)))))
	(target-activity (ucm-read-activity "Transfer to activity: " nil 'no-initial)))

    (when (equal target-activity ucm-activity)
      (error "Will not transfer to the same activity"))

    (unless revisions                   ; no revisions are marked, use current
      (let ((data (ewoc-data (ewoc-locate ucm-actb-ewoc))))
	(if (ucm-actb-revision-p data)
	    (setq revisions (list data))
	    (error "Must select a revision."))))

    (with-temp-message "Transferring revisions..."
      (with-cleartool-directory default-directory
	(dolist (revision revisions)
	  (cleartool "chact -fcset %s -tcset %s \"%s\""
		     ucm-activity target-activity
		     (ucm-actb-revision-pname revision)))))

    (ucm-actb-refresh-command)))

;;;;;; ucm-actb-transfer-revisions-command
(defun ucm-actb-visit-item-command (pos)
  "Visit the current item.
If it is a file, it is visited, if it is a directory a dired
buffer is opened, if it is a revision, that file's revision is
visited."
  (interactive "d")
  (let ((node (ewoc-locate ucm-actb-ewoc pos)))
    (when node
      (let ((data (ewoc-data node)))
	(typecase data
	  ;; NOTE: if the node is of another type, we silently ignore it.
	  (ucm-actb-directory
	   (pop-to-buffer (find-file-noselect (ucm-actb-directory-name data))))
	  (ucm-actb-file
	   (pop-to-buffer (find-file-noselect (ucm-actb-file-full-path data))))
	  (ucm-actb-revision
	   ;; `clearcase-file-not-found-handler' will take care of this,
	   ;; except in dynamic views
	   (pop-to-buffer (find-file-noselect (ucm-actb-revision-pname data)))))))))

;;;;;; ucm-actb-ediff-command
(defun ucm-actb-ediff-command (pos)
  "Start an Ediff session for the current revision of the file."
  (interactive "d")
  (let ((node (ewoc-locate ucm-actb-ewoc pos)))
    (when node
      (let ((data (ewoc-data node)))
	(typecase data
	  (ucm-actb-revision
	   (require 'ediff)
	   (require 'ediff-vers)        ; ediff-vc-internal
	   (declare (special ediff-revision-control-package))
	   (let* ((file (ucm-actb-file-full-path (ucm-actb-revision-file data)))
		  (current (let ((v (ucm-actb-revision-name data)))
			     (if (string-match "\\<CHECKEDOUT\\(.[0-9]+\\)?" v)
				 "" v)))
		  (previous (ucm-actb-revision-previous data)))
	     (with-current-buffer (find-file-noselect file)
	       (funcall 'ediff-vc-internal previous current nil)))))))))

;;;;;; ucm-actb-optimize-display-command
(defun ucm-actb-optimize-display-command ()
  "Compact consecutive file revisions into one single revision."
  (interactive)
  (ewoc-filter ucm-actb-ewoc
               (lambda (data)
                 (typecase data
                   (ucm-actb-file
                    (message "Optimizing %s" (ucm-actb-file-name data))
                    (ucm-actb-file-optimize-revisions data) t)
                   (ucm-actb-revision
                    ;; does this revision still exist for file?
                    (memq data (ucm-actb-file-revisions (ucm-actb-revision-file data))))
                   (t t))))
  (ewoc-refresh ucm-actb-ewoc))

;;;;; ucm-browse-activity
;;;###autoload
(defun ucm-browse-activity (activity)
  "Pop-up an information buffer about ACTIVITY.
The buffer will contain a report about the file revisions
checked-in under the activity plus any contributing activities.
The report contains buttons (hyperlinks) to directories, files,
revisions and other activities.

In interactive mode, the user is prompted for an activity name
and completion is available.  ACTIVITY must be in the current
stream (corresponding to the view in `default-directory').  With
prefix argument, obsolete activities can also be selected.  With
a negative prefix argument any activity can be selected, but no
completion is provided.

There are no restriction on ACTIVITY when this function is called
directly."
  (interactive
   (list
    (if (or (eq current-prefix-arg '-)
	    (and (integerp current-prefix-arg)
		 (< current-prefix-arg 0)))
	(read-string "Browse activity: ")
	(ucm-read-activity
	 "Browse activity: "
	 (when current-prefix-arg 'include-obsolete)))))
  (let ((dir (expand-file-name default-directory)))
    (with-cleartool-directory dir
      (let ((buf (get-buffer-create "*UCM Activity Browser*")))
	(pop-to-buffer buf)
	(ucm-actb-mode)
	(let ((inhibit-read-only t))
	  (setq default-directory dir)
	  (erase-buffer)
	  (let ((view (replace-regexp-in-string "[\n\r]+" "" (cleartool "pwv -short"))))
	    (unless (string= view "")
	      (let ((vprop (clearcase-get-vprop view default-directory)))
		(setq ucm-view-root (clearcase-vprop-root-path vprop))))))

	(setq ucm-activity activity)
	(ucm-actb-refresh-command)))))

;;;; ucm-checkin-activity
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
    (lexical-let ((activity activity)
                  (dir default-directory)
                  (window-configuration (current-window-configuration)))

      (let ((modified-files (clearcase-revert-unchanged-files 
                             (ucm-checked-out-files activity dir))))
        (when (null modified-files)
          (error "No files to checkin.")))
      
      (log-edit (lambda ()
		  (interactive)
		  (with-cleartool-directory dir
		    (with-temp-message (format "Checking in %s..." activity)
		      (let ((comment-text
                             (buffer-substring-no-properties (point-min) (point-max)))
			    (default-directory dir)
			    ;; Need to grab the file list again, in case it
			    ;; has changed.
			    (files (ucm-checked-out-files activity dir)))
                        
			(if (string= comment-text "")
			    (cleartool "checkin -nc activity:%s@%s" activity ucm-projects-vob)
			    (with-clearcase-cfile (comment comment-text)
			      (cleartool "checkin -cfile \"%s\" activity:%s@%s"
					 comment activity ucm-projects-vob)))
                        
			(clearcase-refresh-files files))))
		  (set-window-configuration window-configuration))
                'setup
                `((log-edit-listfun
                   . ,(lambda ()
                              (interactive)
                              (let ((default-directory dir))
                                (mapcar 'file-relative-name
                                        (ucm-checked-out-files activity dir)))))
                  (log-edit-diff-function
                   . ,(lambda ()
                              (interactive)
                              (let ((default-directory dir)
                                    (diff-buffer (get-buffer-create "*vc-diff*")))
                                (vc-setup-buffer diff-buffer)
                                (with-current-buffer diff-buffer
                                  (diff-mode)
                                  (setq buffer-read-only t))
                                (let ((files (ucm-checked-out-files activity dir)))
                                  (vc-clearcase-diff files nil nil diff-buffer))
                                (pop-to-buffer diff-buffer)))))
                (get-buffer-create "*UCM-Checkin-Log*")))))

(defun ucm-checked-out-files (activity dir)
  "Return the list of files checked out under ACTIVITY.
DIR is a directory inside a valid ClearCase view (needed for
cleartool to work)"
  (with-cleartool-directory dir
    (let ((files nil))
      (dolist (v (split-string
		  (cleartool "lsact -fmt \"%%[versions]Cp\" %s" activity)
		  ", " 'omit-nulls))
	(when (string-match "\\(.*\\)@@\\(.*\\)" v)
	  (let ((file (match-string 1 v))
		(revision (match-string 2 v)))
	    (when (string-match "\\<CHECKEDOUT\\(.[0-9]+\\)?" revision)
	      (add-to-list 'files file)))))
      files)))

(defun ucm-activity-files (activity dir)
  "Return the list of files that are part of ACTIVITY.
DIR is a directory inside a valid ClearCase view (needed for
cleartool to work)"
  (with-cleartool-directory dir
    (let ((files nil))
      (dolist (v (split-string
		  (cleartool "lsact -fmt \"%%[versions]Cp\" %s" activity)
		  ", " 'omit-nulls))
	(when (string-match "\\(.*\\)@@\\(.*\\)" v)
	  (let ((file (match-string 1 v))
		(revision (match-string 2 v)))
            (add-to-list 'files file))))
      files)))

;;;; ucm-lock-activity, ucm-unlock-activity

;;;###autoload
(defvar ucm-before-activity-lock-hook '()
  "Hook run before an activity is locked.
This can be used for example to attach/modifiy attributes on the
activity.

Each hook function is called with two arguments, the first is
the activity name, the second is t.")

;;;###autoload
(defun ucm-lock-activity (activity)
  "Lock ACTIVITY.  With prefix arg, mark it as obsolete."
  (interactive (list (ucm-read-activity "Lock activity: ")))
  (with-cleartool-directory default-directory
    (let ((status (cleartool "lsact -fmt \"%%[locked]p\" %s" activity)))
      (if (member status '("locked" "obsolete"))
	  (message "%s is already locked" activity)
	  (progn
	    (run-hook-with-args
	     'ucm-before-activity-lock-hook activity current-prefix-arg)
	    (cleartool "lock %s activity:%s@%s"
		       (if current-prefix-arg "-obsolete" "") activity ucm-projects-vob)
	    (message "%s is now %s" activity
		     (if current-prefix-arg "obsolete" "locked")))))))

;;;###autoload
(defun ucm-unlock-activity (activity)
  "Unlock ACTIVITY. With prefix arg, allow selecting obsolete activities."
  (interactive
   (list (ucm-read-activity
	  "Unlock activity: "
	  (when current-prefix-arg 'include-obsolete))))
  (with-cleartool-directory default-directory
    (let ((status (cleartool "lsact -fmt \"%%[locked]p\" %s" activity)))
      (if (equal status "unlocked")
	  (message "%s is already unlocked" activity)
	  (progn
	    (cleartool "unlock activity:%s@%s" activity ucm-projects-vob)
	    (message "%s is now unlocked" activity))))))


(provide 'ucm)


;;; Local Variables:
;;; mode: emacs-lisp
;;; mode: outline-minor
;;; mode: bug-reference
;;; outline-regexp: ";;;;+"
;;; End:

;;; ucm.el ends here
