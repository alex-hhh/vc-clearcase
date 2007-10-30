;;;; emacs -batch -l install.el -f clearcase-build-package


(require 'autoload)

(defconst clearcase-package-files
  '("vc-clearcase.el" "ucm.el"))

(defconst clearcase-autoload-file "vc-clearcase-auto.el")

(defun clearcase-build-package (&optional dir)
  (unless dir (setq dir default-directory))
  (let ((generated-autoload-file
	 (expand-file-name clearcase-autoload-file dir))
	(load-path (cons dir load-path)))
    (with-current-buffer (find-file generated-autoload-file)
      (erase-buffer)
      (insert "\f")
      (dolist (file clearcase-package-files)
	(let ((file (expand-file-name file dir)))
	  (unless (file-exists-p file)
	    (error "cannot find %s" file))
	  (unless (byte-compile-file file)
	    (error
	     "Failed to compile %s, check the *Compile-Log* buffer for errors"
	     file))
	  (generate-file-autoloads file)))
      (save-buffer))))
