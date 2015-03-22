vc-clearcase relies on the ClearCase service being available. Currently it does not play nice when the view and vob servers cannot be reached. To workaround the issue, I keep the following Emacs-Lisp forms around when I want to disable ClearCase:

```
;; Disable ClearCase
(setq vc-handled-backends
      (delete 'CLEARCASE vc-handled-backends))

;; Enable ClearCase
(add-to-list 'vc-handled-backends 'CLEARCASE 'append)
```

Note that this piece of code sould not be loaded. Instead, open the file, put the cursor inside of the form you want to evaluate and press C-M-x or M-x eval-defun.

These were based on a suggestion by Dimitry in [[bug report](http://sourceforge.net/tracker/index.php?func=detail&aid=1818879&group_id=160898&atid=817687|this)].