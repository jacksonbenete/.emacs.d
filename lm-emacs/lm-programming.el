;;; lm-programming.el --- Programming languages

;; Project: lm-emacs
;; Author: Jackson Benete Ferreira
;; Contact: <jacksonbenete@gmail.com>
;; -----------------------------------------------

;;; Commentary:
;; -----------------------------------------------
;; Configurations for various programming languages.
;; -----------------------------------------------
;;; Code:

;; Include the Language Server Protocol Clients
(package-require 'lsp-mode)

;; Customize prefix for key-bindings
(setq lsp-keymap-prefix "C-l")

;;; Company
;; -----------------------------------------------
(add-hook 'after-init-hook 'global-company-mode)
;; -----------------------------------------------

;;; Magit
;; -----------------------------------------------
;; Git inside Emacs!
;; [https://github.com/magit/magit]

;; avoid slowdown
(require 'magit)
(setq magit-commit-show-diff nil
      magit-revert-buffers 1)
;; -----------------------------------------------

;;; flycheck
;; -----------------------------------------------
;; [https://www.flycheck.org/en/latest/]
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))
;; -----------------------------------------------

;; Flycheck-tip
;; (require 'flycheck-tip)
;; (flycheck-tip-use-timer 'verbose)


;;; diff-hl:
;; -----------------------------------------------
;; [https://github.com/dgutov/diff-hl]
;; Highlight uncommited changes
;; (use-package diff-hl
;;   :init
;;   ;; Better looking colours for diff indicators /w spacemacs-light theme
;;   (custom-set-faces
;;    '(diff-hl-change ((t (:background "#3a81c3"))))
;;    '(diff-hl-insert ((t (:background "#7ccd7c"))))
;;    '(diff-hl-delete ((t (:background "#ee6363")))))
;;   ;; On-the-fly diff updates
;;   (diff-hl-flydiff-mode)
;;   ;; Enable diff-hl globally
;;   (global-diff-hl-mode 1))
;; ;; -----------------------------------------------


;;; Programming Languages
;; -----------------------------------------------
(add-to-list 'load-path "~/.emacs.d/lm-emacs/languages/")

;; (load-library "clojure")
;; (load-library "common-lisp")
;; (load-library "elisp")
;; (load-library "javascript")
;; (load-library "racket")
;; (load-library "scheme")

;;; Erlang
;; -----------------------------------------------
;; (load-library "erlang")
;; -----------------------------------------------

;;; lm-programming.el ends here