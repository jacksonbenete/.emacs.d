;;; elisp-slime-nave
;; -----------------------------------------------
;; [https://github.com/purcell/elisp-slime-nav]
(require 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))
;; -----------------------------------------------

