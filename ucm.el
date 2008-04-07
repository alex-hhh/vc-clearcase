;; ucm.el -- Support for ClearCase Unified Change Management (UCM)

;; Copyright (C) 2007,2008 Alexandru Harsanyi
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
(require 'ewoc)

(defgroup ucm nil
  "Support for UCM under ClearCase"
  :group 'vc-clearcase)

;;;; ucm-read-activity
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

;;;; ucm-set-activity
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
       (let ((status (cleartool "lsact -fmt \"%%[locked]p\" %s" activity)))
	 (when (equal status "locked")
	   (if (y-or-n-p "Activity is locked.  Would you like to unlock it? ")
	       (progn
		 (cleartool "unlock activity:%s@/projects" activity)
		 (message "Activity %s unlocked" activity))
	       (error "Cannot set this activity beacuse it is locked.")))
	 (cleartool "setact \"%s\"" activity))))))

;;;; ucm-show-current-activity
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

;;;; ucm-browse-activity
;;;;; faces
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
      (setq ucm-activity (pop ucm-previous-activities))
      (setq ucm-actb-ewoc (pop ucm-previous-actb-ewocs))
      (erase-buffer)
      ;; The header and footer will not be redisplayed unless we set them
      ;; again.
      (let ((hf (ewoc-get-hf ucm-actb-ewoc)))
	(ewoc-set-hf ucm-actb-ewoc (car hf) (cdr hf)))
      (ewoc-refresh ucm-actb-ewoc)
      (goto-char (point-min)))))

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

;;;;; data structures
;;;;;; ucm-actb-activity

;; Hold revision and contributor information about an activity.  See
;; `ucm-actb-fetch-activity' for how an instance of this structure is
;; constructed.
;;
;; An activity is a collection of file revisions, but we represent it as a
;; hierarch of DIRECTORY -> FILE -> VERSION: Each file revision is represented
;; by a `ucm-actb-version' instance.  The revisions for a file are grouped
;; under a `ucm-actb-file' instance.  All files under a directory are grouped
;; under a `ucm-actb-directory' instance.

(defstruct ucm-actb-activity
  name
  ;; a list of `ucm-actb-directory' structures, one for each directory in
  ;; which this activity has file versions
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
  "Toggle the marks on all versions under this DIRECTORY.
The toggle works like this: if all revisions are marked, the
marks are cleared, if any revision is unmarked, the marks are
set."
  (let* ((files (ucm-actb-directory-files directory))
	 (marks (mapcar (lambda (f)
			  (some 'ucm-actb-version-mark
				(ucm-actb-file-versions f)))
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
  versions)                    ; a list of `ucm-actb-version' structures

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
  (mapc 'ucm-actb-version-set-mark (ucm-actb-file-versions file)))

(defun ucm-actb-file-clear-mark (file)
  "Clear the mark from all revisions of FILE"
  (mapc 'ucm-actb-version-clear-mark (ucm-actb-file-versions file)))

(defun ucm-actb-file-toggle-mark (file)
  "Toggle the mark for the revisions of FILE.
The toggle works like this: if all revisions are marked, the
marks are cleared, if any revision is unmarked, the marks are
set."
  (let ((versions (ucm-actb-file-versions file)))
    (cond ((notevery 'ucm-actb-version-mark versions)
	   (mapc 'ucm-actb-version-set-mark versions))
	  (t
	   (mapc 'ucm-actb-version-toggle-mark versions)))))

;;;;;; ucm-actb-version

;; The revision of a file under an activity.
(defstruct ucm-actb-version
  name
  file                                  ; The `ucm-actb-file' we belong to
  mark                                  ; (t nil)
  ;; Becomes t when the mark has changed.  Used to know when the ewoc node
  ;; should be refreshed.  It is a bit of a hack.
  changed)

(defun ucm-actb-version-pp (version)
  "Pretty print VERSION, a `ucm-actb-version' structure"
  (if (ucm-actb-version-mark version)
      (insert "      * ")
      (insert "        "))
  (let ((file (ucm-actb-file-full-path (ucm-actb-version-file version)))
	(rev (ucm-actb-version-name version)))
		(insert-text-button
     (concat "@@" rev)
     'face (if (string-match "[\\/]CHECKEDOUT\\(.[0-9]+\\)\\'" rev)
			   'ucm-checkedout-revision-face
			   'ucm-revision-face)
		 'type 'ucm-show-diff-link
		 'buffer (current-buffer)
		 'file-name file
     'revision rev)))

(defun ucm-actb-version-set-mark (version)
  "Set the mark on VERSION."
  (let ((mark (ucm-actb-version-mark version)))
    (unless mark
      (setf (ucm-actb-version-mark version) t)
      (setf (ucm-actb-version-changed version) t))))

(defun ucm-actb-version-clear-mark (version)
  "Clear the mark on VERSION."
  (let ((mark (ucm-actb-version-mark version)))
    (when mark
      (setf (ucm-actb-version-mark version) nil)
      (setf (ucm-actb-version-changed version) t))))

(defun ucm-actb-version-toggle-mark (version)
  "Toggle the mark on VERSION."
  (setf (ucm-actb-version-mark version)
	(not (ucm-actb-version-mark version)))
  (setf (ucm-actb-version-changed version) t))


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
    (ucm-actb-version (ucm-actb-version-pp data))
    (ucm-actb-contributor (ucm-actb-contributor-pp data))
    (t (error "Unknown data type %S" data))))

(defun ucm-actb-toggle-mark (data)
  "Toggle the mark on DATA, which can be any data structure we
attach to the activity ewoc."
  (typecase data
    (ucm-actb-directory (ucm-actb-directory-toggle-mark data))
    (ucm-actb-file (ucm-actb-file-toggle-mark data))
    (ucm-actb-version (ucm-actb-version-toggle-mark data))
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
	      (version (match-string 2 v)))

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

		(push (make-ucm-actb-version :name version :file f)
		      (ucm-actb-file-versions f)))
	    )))))

    (ignore-cleartool-errors
      ;; There seems to be a bug in my version of ClearCase: if `activity' is
      ;; not a rebase or integration activity an error will be reported, but
      ;; the status of the command will be 0 (meaning success).  We have to
      ;; test the returned string explicitly ...
      (let ((ca (cleartool "lsact -fmt \"%%[contrib_acts]p\" %s" activity)))
	(when (and ca (not (string-match "^cleartool: Error: " ca)))
	  (dolist (c (sort (split-string ca " " 'omit-nulls) 'string<))
	    (push (make-ucm-actb-contributor :name c) contributors)))))

    (setq contributors (nreverse contributors))

    (dolist (d dirs)
      (setf (ucm-actb-directory-files d)
	    (sort (ucm-actb-directory-files d)
		  (lambda (a b)
		    (string< (ucm-actb-file-name a)
			     (ucm-actb-file-name b))))))
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
		  ")\n"))
	 (ewoc (ewoc-create 'ucm-actb-pp header)))
    (dolist (d (ucm-actb-activity-directories activity))
      (ewoc-enter-last ewoc d)
      (dolist (f (ucm-actb-directory-files d))
	(ewoc-enter-last ewoc f)
	(dolist (v (ucm-actb-file-versions f))
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
    m))

(define-derived-mode ucm-actb-mode fundamental-mode
  "UCM Activity Browser"
  (buffer-disable-undo)
  (set (make-local-variable 'ucm-activity) nil)
  (set (make-local-variable 'ucm-view-root) nil)
  (set (make-local-variable 'ucm-actb-ewoc) nil)
  (set (make-local-variable 'ucm-previous-actb-ewocs) nil))

;;;;;; ucm-actb-toggle-mark-command
(defun ucm-actb-toggle-mark-command (pos)
  "Toggle the mark on the current item.
If the current item is a version the mark is changed (set if it
is unset and cleared if it is set).  For files or directories, if
all revisions under them are marked, the marks are cleared,
otherwise the marks are set."
  (interactive "d")
  (let ((node (ewoc-locate ucm-actb-ewoc pos)))
    (when node
      (ucm-actb-toggle-mark (ewoc-data node))
      (ewoc-map
       (lambda (data)
	 (when (and (ucm-actb-version-p data)
		    (ucm-actb-version-changed data))
	   (setf (ucm-actb-version-changed data) nil)
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
    (erase-buffer)
    (with-cleartool-directory (expand-file-name default-directory)
      (let ((a (ucm-actb-fetch-activity ucm-activity)))
	(setq ucm-actb-ewoc (ucm-actb-create-ewoc a))
	(ewoc-refresh ucm-actb-ewoc)
	(goto-char (point-min))))))

;;;;; ucm-browse-activity
;;;###autoload
(defun ucm-browse-activity (activity)
  "Pop-up an information buffer about ACTIVITY.
The buffer will contain a report about the file versions
checked-in under the activity plus any contributing activities.
The report contains buttons (hyperlinks) to directories, files,
versions and other activities.

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
	 "Browse activity: " nil
	 (when current-prefix-arg 'include-obsolete)))))
  (with-cleartool-directory (expand-file-name default-directory)
    (let ((buf (get-buffer-create "*UCM Activity Browser*")))
      (switch-to-buffer buf)
      (ucm-actb-mode)
      (erase-buffer)
      (let ((view (replace-regexp-in-string "[\n\r]+" "" (cleartool "pwv -short"))))
	(unless (string= view "")
	  (let ((vprop (clearcase-get-vprop view default-directory)))
	    (setq ucm-view-root (clearcase-vprop-root-path vprop)))))

      (setq ucm-activity activity)
      (ucm-actb-refresh-command))))

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
		(cleartool "uncheckout -rm \"%s\"" file)
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

;;;; ucm-lock-activity, ucm-unlock-activity
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


;;; Local Variables:
;;; mode: emacs-lisp
;;; mode: outline-minor
;;; outline-regexp: ";;;;+"
;;; End:

;;; ucm.el ends here
