;; The code below will byte-compile vc-clearcase.el and generate the
;; autoloads in a file named vc-clearcase-auto.el in the same
;; directory as vc-clearcase.el. If the user does not have
;; vc-clearcase.el opened, or has multiple copies of it, we ask for
;; the file name explicitely.
;;
;; After the code below runs, the compilation results will be in a
;; buffer named "*Compile-Log* (which you can switch to).  The
;; vc-clearcase-auto.el file will be saved.
;;
;; To evaluate the code below, move the cursor at the end of it and
;; type C-x C-e


;; Emacs 22 version
(save-window-excursion
  (require 'autoload)
  (with-current-buffer
      (or (get-buffer "vc-clearcase.el")
          (find-file
           (read-file-name "Find vc-clearcase.el: " nil nil 'must-match)))
    (let* ((file (buffer-file-name (current-buffer)))
           (base (file-name-nondirectory file))
           (dir (file-name-directory file))
           (generated-autoload-file
            (expand-file-name "vc-clearcase-auto.el" dir)))
      (unless (equal base "vc-clearcase.el")
        (error "Expecting a file named vc-clearcase.el, got %s" base))
      ;; byte-compile-file returns nil if there were errors
      (unless (byte-compile-file file)
        (error
         "Failed to compile %s, check the *Compile-Log* buffer for errors"
         file))
      ;; update-file-autoloads returns nil if no autoloads were found.
      (when (update-file-autoloads file 'save-after)
        (error "Failed to find autoloads in %s" file)))))

;; Emacs 21 version
(save-window-excursion
  (require 'autoload)
  (with-current-buffer
      (or (get-buffer "vc-clearcase.el")
          (find-file
           (read-file-name "Find vc-clearcase.el: " nil nil 'must-match)))
    (let* ((file (buffer-file-name (current-buffer)))
           (base (file-name-nondirectory file))
           (dir (file-name-directory file))
           (generated-autoload-file
            (expand-file-name "vc-clearcase-auto.el" dir)))
      (unless (equal base "vc-clearcase.el")
        (error "Expecting a file named vc-clearcase.el, got %s" base))
      ;; byte-compile-file returns nil if there were errors
      (unless (byte-compile-file file)
        (error
         "Failed to compile %s, check the *Compile-Log* buffer for errors"
         file))
      (with-current-buffer (find-file generated-autoload-file)
        (erase-buffer)
        (insert "\f")
        (generate-file-autoloads file)
        (save-buffer)))))
