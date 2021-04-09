;;; luna-third-packages --- Third party packages installed by Elpa/Melpa

;; -----------------------------------------------
;; File: Luna Emacs Third Packages
;; Project: Luna Emacs
;; Author: Jackson Benete Ferreira
;; Contact: <jacksonbenete@gmail.com>

;;; Commentary:
;; List source and configure third-party packages.
;;

;;; Code:
;; -----------------------------------------------

;;; undo-tree
;; -----------------------------------------------
;; [https://elpa.gnu.org/packages/undo-tree.html]
(require 'undo-tree)
(global-undo-tree-mode)
;; -----------------------------------------------


;;; easy-kill
;; -----------------------------------------------
;; [https://github.com/leoliu/easy-kill]
(global-set-key [remap kill-ring-save] 'easy-kill)
;; -----------------------------------------------


;;; flycheck
;; -----------------------------------------------
;; [https://www.flycheck.org/en/latest/]
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))
;; -----------------------------------------------


;;; which-key
;; -----------------------------------------------
;; [https://github.com/justbur/emacs-which-key]
(require 'which-key)
(setq which-key-idle-delay 1.0)
(setq which-key-idle-secondary-delay 0.05)
(which-key-mode)
;; -----------------------------------------------


;;; icomplete-vertical
;; -----------------------------------------------
;; [https://github.com/oantolin/icomplete-vertical]
(use-package icomplete-vertical
  :ensure t
  :demand t
  :custom
  (completion-styles '(partial-completion substring))
  (completion-category-overrides '((file (styles basic substring))))
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  :config
  (setq completion-auto-help nil)
  (icomplete-mode)
  (fido-mode)
  (savehist-mode)
  (icomplete-vertical-mode)
  :bind (:map icomplete-minibuffer-map
              ("<down>" . icomplete-forward-completions)
              ("C-n" . icomplete-forward-completions)
              ("<up>" . icomplete-backward-completions)
              ("C-p" . icomplete-backward-completions)
              ("C-v" . icomplete-vertical-toggle)
	      ("C-j" . minibuffer-force-complete-and-exit)))
;; -----------------------------------------------


;;; Consult
;; -----------------------------------------------
;; Improves icomplete.
;; [https://github.com/minad/consult]
;; (use-package consult
;;   ;; Replace bindings. Lazily loaded due by `use-package'.
;;   :bind (("C-x M-:" . consult-complex-command)
;;          ("C-c h" . consult-history)
;;          ("C-c m" . consult-mode-command)
;;          ;; ("C-x b" . consult-buffer)
;;          ("C-x r x" . consult-register)
;;          ("C-x r b" . consult-bookmark)
;;          ("M-g i" . consult-imenu)
;;          ("M-g e" . consult-error)
;;          ("M-s m" . consult-multi-occur)
;;          ("M-y" . consult-yank-pop)
;;          ("<help> a" . consult-apropos))
;;   :init
;;   (fset 'multi-occur #'consult-multi-occur)
;;   :config
;;   (setq consult-narrow-key "<") ;; (kbd "C-+")
;;   (consult-preview-mode))

(defun consult-completion-in-region (start end collection &optional predicate)
  "Prompt for completion of region in the minibuffer if non-unique.

The function is called with 4 arguments: START END COLLECTION PREDICATE.
The arguments and expected return value are as specified for
`completion-in-region'.  Use as a value for `completion-in-region-function'."
  (let* ((initial (buffer-substring-no-properties start end))
         (limit (car (completion-boundaries initial collection predicate "")))
         (metadata (completion-metadata initial collection predicate))
         (category (completion-metadata-get metadata 'category))
         (all (completion-all-completions initial collection predicate
                                          (length initial)))
         (exit-status 'finished)
         (completion
          (cond
           ((atom all) nil)
           ((and (consp all) (atom (cdr all)))
            (setq exit-status 'sole)
            (concat (substring initial 0 limit) (car all)))
           (t (let ((enable-recursive-minibuffers t))
                (if (eq category 'file)
                    ;; When completing files with consult-completion-in-region, the point in the
                    ;; minibuffer gets placed initially at the beginning of the last path component.
                    ;; By using the filename as DIR argument (second argument of read-file-name), it
                    ;; starts at the end of minibuffer contents, as for other types of completion.
                    ;; However this is undefined behavior since initial does not only contain the
                    ;; directory, but also the filename.
                    (read-file-name
                     "Completion: " initial initial t nil predicate)
                  (completing-read
                   "Completion: " collection predicate t initial)))))))
    (if (null completion)
        (progn (message "No completion") nil)
      (delete-region start end)
      (insert (substring-no-properties completion))
      (when-let ((exit (plist-get completion-extra-properties :exit-function)))
        (funcall exit completion exit-status))
      t)))
(setq completion-in-region-function #'consult-completion-in-region)
;; -----------------------------------------------


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


;;; racket-mode:
;; -----------------------------------------------
;; [https://github.com/greghendershott/racket-mode]
;; Racket IDE and REPL
(require 'racket-xp)
(add-hook 'racket-mode-hook #'racket-xp-mode)
;; -----------------------------------------------


;;; Move-Text:
;; -----------------------------------------------
;; [https://github.com/emacsfodder/move-text]
;; Move current line up or down
(use-package move-text
  :config (move-text-default-bindings))
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)
;; -----------------------------------------------


;;; Helpful:
;; -----------------------------------------------
;; [https://github.com/Wilfred/helpful]
;; Beautify help functions
;; (setq counsel-describe-function-function #'helpful-callable)
;; (setq counsel-describe-variable-function #'helpful-variable)
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
;; -----------------------------------------------


;;; pdf-tools:
;; -----------------------------------------------
;; Various nice tools such as highlighting and taking notes
;; [https://github.com/politza/pdf-tools]
(require 'pdf-tools)
(when (not window-system)
  (pdf-loader-install))
(add-hook 'pdf-tools-enabled-hook #'auto-save-mode)
(add-hook 'pdf-tools-enabled-hook
	  (and
	   (set (make-local-variable 'auto-save-interval) 2)
	   (set (make-local-variable 'auto-save-visited-mode) t)))
(with-eval-after-load 'pdf-tools
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (define-key pdf-view-mode-map (kbd "M-h") 'pdf-annot-add-highlight-markup-annotation)
  (define-key pdf-view-mode-map (kbd "M-l") 'pdf-annot-list-annotations)
  (define-key pdf-view-mode-map (kbd "M-k") 'pdf-annot-delete))
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


;;; Deft
;; -----------------------------------------------
;; Notes repository
;; [https://github.com/jrblevin/deft]

;; changes ~/.deft to ~/.notes
(require 'deft)
(setq deft-directory "~/.notes")

(with-eval-after-load "deft"
  (and
   ;; Save temp variable to recover configurations later.
   (defvar deft-default-directory deft-directory)
   (defun deft-change-directory ()
     "Run Deft in selected directory from prompt.
      If directory doesn't exists, create."
     (interactive)
     (setq deft-directory (expand-file-name (read-directory-name "Deft directory: ")))
     (unless (file-directory-p deft-directory)
       (make-directory deft-directory t))
     (if (get-buffer deft-buffer)
	 (and (switch-to-buffer deft-buffer) (deft-refresh))
       (deft)))
   (defun restore-deft ()
     "When Deft buffer is killed, recover default deft-directory."
     (interactive)
     (if (string= (buffer-name) deft-buffer)
	 (setq deft-directory deft-default-directory)
       nil)))
  (add-hook 'kill-buffer-hook 'restore-deft)
  (define-key deft-mode-map (kbd "C-x C-g") 'deft-change-directory))
;; -----------------------------------------------


;;; God mode
;; -----------------------------------------------
;; Minor mode for "modal editing"
;; [https://github.com/emacsorphanage/god-mode]

(require 'god-mode)
(global-set-key (kbd "<escape>") #'god-local-mode)
(define-key god-local-mode-map (kbd "i") #'god-local-mode)

;; To prevend god-mode in a major-mode like in dired, you would
;; do the following:
;; (add-to-list 'god-exempt-major-modes 'dashboard-mode)
;;
;; Instead, this is done in god-mode-enabled-if-file
;; Every major-mode will have god-mode ignored unless it's a file.
;; Sometimes you want to ignore god-mode even if it's a file:
;; E.g. pdf-view-mode, image-mode...

(defun god-mode-update-cursor ()
  "Cursor it's a box in god-mode or a bar in insert-mode."
  (setq cursor-type
	(if (or god-local-mode buffer-read-only) 'box 'bar)))

(defun god-mode-enabled-if-file ()
  "Enable god-mode only if in a file, never on modes (Slime, speedbar, etc)."
  (interactive)
  (unless (member major-mode '(pdf-view-mode image-mode))
    (if (buffer-file-name)
	(god-local-mode t)
      (god-local-mode 0))
    ;; Some modes like pdf-view-mode set it's own cursor
    ;; So it's good to update it again
    (god-mode-update-cursor)))


(add-hook 'god-mode-enabled-hook #'god-mode-update-cursor)
(add-hook 'god-mode-disabled-hook #'god-mode-update-cursor)
(add-hook 'after-change-major-mode-hook #'god-mode-enabled-if-file)

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


;;; Company:
;; -----------------------------------------------
;; [https://github.com/company-mode/company-mode]
;; Auto-completion in buffer.
;; (add-hook 'after-init-hook 'global-company-mode)
;; (use-package company
  ;; :config (setq company-frontends nil)
  ;; :hook (after-init . global-company-mode))

;; -----------------------------------------------


;;; Slime-company
;; -----------------------------------------------
;; More informative slime autocomplete
;; [https://github.com/anwyn/slime-company]
;; (slime-setup '(slime-fancy slime-company))
;; (use-package slime-company
;;   :after (slime company)
;;   :config (setq slime-company-completion 'fuzzy
;;                 slime-company-after-completion 'slime-company-just-one-space))
;; (define-key company-active-map (kbd "\C-n") 'company-select-next)
;; (define-key company-active-map (kbd "\C-p") 'company-select-previous)
;; (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
;; (define-key company-active-map (kbd "M-.") 'company-show-location)
;; -----------------------------------------------


;;; Tramp
;; -----------------------------------------------
;; Easy connection to remote using Emacs
;; [http://savannah.gnu.org/projects/tramp]
(require 'tramp)
(setq tramp-default-method "ssh")
;; -----------------------------------------------


;;; Eshell-bookmark
;; -----------------------------------------------
;; Eshell and bookmark integration
;; [https://github.com/Fuco1/eshell-bookmark]
;; (use-package eshell-bookmark
;;   :after eshell
;;   :config
;;   (add-hook 'eshell-mode-hook #'eshell-bookmark-setup))

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



;;; luna-third-packages.el ends here