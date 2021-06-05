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

