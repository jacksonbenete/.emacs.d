;;; luna-third-packages --- Third party packages installed by Elpa/Melpa

;; -----------------------------------------------
;; Project: lm-emacs
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


;;; which-key
;; -----------------------------------------------
;; [https://github.com/justbur/emacs-which-key]
(require 'which-key)
(setq which-key-idle-delay 1.0)
(setq which-key-idle-secondary-delay 0.05)
(which-key-mode)
;; -----------------------------------------------


;;; ivy
;; -----------------------------------------------
(ivy-mode)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
;; -----------------------------------------------

;;; icomplete-vertical
;; -----------------------------------------------
;; [https://github.com/oantolin/icomplete-vertical]
;; (use-package icomplete-vertical
;;   :ensure t
;;   :demand t
;;   :custom
;;   (completion-styles '(partial-completion substring))
;;   (completion-category-overrides '((file (styles basic substring))))
;;   (read-file-name-completion-ignore-case t)
;;   (read-buffer-completion-ignore-case t)
;;   (completion-ignore-case t)
;;   :config
;;   (setq completion-auto-help nil)
;;   (icomplete-mode)
;;   (when (display-graphic-p) (fido-mode))
;;   (savehist-mode)
;;   (icomplete-vertical-mode)
;;   :bind (:map icomplete-minibuffer-map
;;               ("<down>" . icomplete-forward-completions)
;;               ("C-n" . icomplete-forward-completions)
;;               ("<up>" . icomplete-backward-completions)
;;               ("C-p" . icomplete-backward-completions)
;;               ("C-v" . icomplete-vertical-toggle)
;; 	      ("C-j" . minibuffer-force-complete-and-exit)))
;; ;; -----------------------------------------------


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
(when (display-graphic-p)
  (require 'pdf-tools)
  (when (eq window-system 'x)
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
    (define-key pdf-view-mode-map (kbd "M-k") 'pdf-annot-delete)))
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


;;; luna-third-packages.el ends here