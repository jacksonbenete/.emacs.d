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
(require 'flycheck-tip)
(flycheck-tip-use-timer 'verbose)


;;; diff-hl:
;; -----------------------------------------------
;; [https://github.com/dgutov/diff-hl]
;; Highlight uncommited changes
(use-package diff-hl
  :init
  ;; Better looking colours for diff indicators /w spacemacs-light theme
  (custom-set-faces
   '(diff-hl-change ((t (:background "#3a81c3"))))
   '(diff-hl-insert ((t (:background "#7ccd7c"))))
   '(diff-hl-delete ((t (:background "#ee6363")))))
  ;; On-the-fly diff updates
  (diff-hl-flydiff-mode)
  ;; Enable diff-hl globally
  (global-diff-hl-mode 1))
;; -----------------------------------------------


;;; Programming Languages
;; -----------------------------------------------
(add-to-list 'load-path "~/.emacs.d/lm-emacs/languages/")

;;; racket-mode:
;; -----------------------------------------------
;; [https://github.com/greghendershott/racket-mode]
;; Racket IDE and REPL
(require 'racket-xp)
(add-hook 'racket-mode-hook #'racket-xp-mode)
;; -----------------------------------------------


;;; Slime
;; -----------------------------------------------
;; Superior Lisp Interacion Mode for Emacs
;; [https://github.com/slime/slime]
;; (setq inferior-lisp-program "sbcl")

;; (use-package slime
;;   :init
;;   ;; check if folder is /.quicklisp or /quicklisp
;;   (if (file-exists-p "~/.quicklisp/slime-helper.el")
;;       (load (expand-file-name "~/.quicklisp/slime-helper.el"))
;;     (load (expand-file-name "~/quicklisp/slime-helper.el")))
;;   :custom
;;   (inferior-lisp-program "sbcl")
;;   :config
;;   (setq slime-lisp-implementations
;; 	'((sbcl  ("/usr/bin/sbcl" "--dynamic-space-size" "2GB") :coding-system utf-8-unix)
;; 	  (mlisp10 ("/Applications/AllegroCL-10/mlisp"))
;; 	  (mlisp9  ("/Applications/AllegroCL-9/mlisp"))
;; 	  (abcl  ("/usr/local/bin/abcl"))
;; 	  (ccl   ("/opt/local/bin/ccl64")))
;; 	slime-net-coding-system 'utf-8-unix
;; 	slime-export-save-file t
;; 	slime-contribs '(slime-fancy slime-repl slime-scratch slime-trace-dialog)
;; 	lisp-simple-loop-indentation  1
;; 	lisp-loop-keyword-indentation 6
;; 	lisp-loop-forms-indentation   6
;; 	completion-auto-help nil)
;;   ;; (global-set-key "\C-z" 'slime-selector)
;;   (add-hook 'slime-load-hook            (lambda () (require 'slime-fancy)))
;;   (show-paren-mode 1))

;; (load (expand-file-name "~/.quicklisp/slime-helper.el"))
;; (load (expand-file-name "~/quicklisp/slime-helper.el"))

;; -----------------------------------------------


;;; clojure-mode
;; -----------------------------------------------
;; [https://github.com/clojure-emacs/clojure-mode]
(require 'clojure-mode)
;; -----------------------------------------------


;;; cider
;; -----------------------------------------------
;; [https://github.com/clojure-emacs/cider]
;; Cider is the "Slime" for Clojure.
(require 'cider)
;; -----------------------------------------------


;;; elisp-slime-nave
;; -----------------------------------------------
;; [https://github.com/purcell/elisp-slime-nav]
(require 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))
;; -----------------------------------------------


;;; geiser
;; -----------------------------------------------
;; [https://www.nongnu.org/geiser/]
;; Geiser is the "Slime" for Scheme.
(require 'geiser)
(setq geiser-repl-use-other-window nil)
;; -----------------------------------------------


;;; Erlang
;; -----------------------------------------------
(load-library "erlang")
;; -----------------------------------------------


;;; Javascript
;; -----------------------------------------------
;; [https://gist.github.com/CodyReichert/9dbc8bd2a104780b64891d8736682cea]

;; web-mode for underlying functionality.
;; auto-enable for .js/.jsx files
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

;; jsx syntax highlighting
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

;; indentation
(defun web-mode-init-hook ()
  "Hooks for Web mode.  Adjust indent."
  (setq web-mode-markup-indent-offset 4))
(add-hook 'web-mode-hook  'web-mode-init-hook)

;; flycheck show eslint erros
;; so we will disable the defaul jslint
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint json-jsonlist)))

;; Enable eslint checker for web-mode
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; add-node-modules-path (this is a package)
(add-hook 'flycheck-mode-hook 'add-node-modules-path)

;; emmet-mode for html tag expansion
(add-hook 'web-mode-hook  'emmet-mode)

;; tide-mode is a typescript support (this is a package)
(defun setup-tide-mode ()
  "Setup the tide-mode at hook."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; (company-mode +1)
  )

;; aligns annotation to the right hand side
;; (setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
(flycheck-add-mode 'typescript-tslint 'web-mode)

;;; Indium
;; [https://github.com/NicolasPetton/Indium]
;; npm install -g indium

;; -----------------------------------------------

