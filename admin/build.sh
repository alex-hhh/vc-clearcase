#!/bin/bash

EMACS=${EMACS:-`which emacs`}

TMP=`mktemp -t build.sh.XXXXXX`
trap "rm $TMP* 2>/dev/null" 0

cat >> $TMP <<EOF
(require 'autoload)

(defconst clearcase-package-files
  '("vc-clearcase.el" "ucm.el"))

(defconst clearcase-autoload-file "vc-clearcase-auto.el")

(defun clearcase-build-package (dir &optional for-release)
  (interactive "DClearcase package dir: \nP")
  (let ((generated-autoload-file
         (expand-file-name clearcase-autoload-file dir))
        (load-path (cons dir load-path)))
    (with-current-buffer (find-file generated-autoload-file)
      (erase-buffer)
      (insert "\f")

      (when for-release
        (load (car clearcase-package-files))
        (unless (>= (car (last (version-to-list vc-clearcase-version))) 0)
          (error "Cannot release beta package: %s" vc-clearcase-version)))

      (dolist (file clearcase-package-files)
        (let ((file (expand-file-name file dir)))
          (unless (file-exists-p file)
            (error "Cannot find %s" file))
          (unless (byte-compile-file file)
            (error "Failed to compile %s" file))
          (generate-file-autoloads file)))
      (save-buffer))))

(defun clearcase-build-package-batch ()
  (condition-case nil
      (clearcase-build-package default-directory 'for-release)
    (error
     (kill-emacs 1))))
EOF

$EMACS -batch -l $TMP -f clearcase-build-package-batch

