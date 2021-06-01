;;; erlang.el --- Erlang configuration

;; Project: lm-emacs
;; Author: Jackson Benete Ferreira
;; Contact: <jacksonbenete@gmail.com>
;; -----------------------------------------------

;;; Commentary:
;; -----------------------------------------------
;; Configuration for Erlang programming.
;; https://erlang-ls.github.io/editors/emacs/
;; -----------------------------------------------
;;; Code:

;; Erlang-mode

(setq use-erlang-lsp-p 0)
(setq use-erlang-distel-p 0) ; avoid distel and edts at the same time
(setq use-erlang-edts-p 0)
(setq use-erlang-flycheck-p 1)
(setq use-erlang-flycheck-tip-p 0)
(setq use-erlang-yasnippet-p 0)
(setq use-erlang-which-key-p 1)

;; Install the official Erlang mode
(package-require 'erlang)

;; Find Erlang and load Emacs files (basic configuration)
(defun load-erlang (erl-dir tools-ver)
  (setq load-path (cons (erl-dir "/lib/tools-"  tools-ver "/emacs") load-path))
  (require 'erlang-start)
  (setq erlang-root-dir (concat erl-dir "/"))
  (setq exec-path (cons (concat erl-dir "/bin") exec-path))
  (setq erlang-man-root-dir (concat erl-dir "/man")))

(let* ((erl-ver "23.3")
       (tools-ver "3.4.4")
       (docker "/opt/erlang/lfe")
       (unix (concat "/usr/local/otp"))
       (win10 (concat "C:\Program Files\erl-" erl-ver)))
  (cond ((file-directory-p docker) (load-erlang docker tools-ver))
	((file-directory-p win10) (load-erlang win10 tools-ver))
	((file-directory-p unix) (load-erlang unix tools-ver))))

;;; EDTS
(when (> use-erlang-edts-p 0)
  (add-hook 'after-init-hook 'my-after-init-hook)
  (defun my-after-init-hook ()
    (require 'edts-start)))

;;; LSP
(when (> use-erlang-lsp-p 0)
  ;; Enable LSP for Erlang files
  (message "If LSP not working, need to Make and Make Install the erlang-ls on c:/bin or /usr/local/bin")
  (add-hook 'erlang-mode-hook #'lsp)

  ;; Enable logging for lsp-mode
  (setq lsp-log-io t)

  ;; Enable and configure the LSP UI Package
  (package-require 'lsp-ui)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'bottom)

  ;; Enable LSP Origami Mode (for folding ranges)
  ;; (package-require 'lsp-origami)
  ;; (add-hook 'origami-mode-hook #'lsp-origami-mode)
  ;; (add-hook 'erlang-mode-hook #'origami-mode)

  (with-eval-after-load 'lsp-mode
    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)))

;;; Flycheck Erlang
(when (> use-erlang-flycheck-p 1)
  (flycheck-define-checker erlang-otp
    "An Erlang syntax checker using the Erlang interpreter."
    :command ("erlc" "-o" temporary-directory "-Wall"
              "-I" "../include" "-I" "../../include"
              "-I" "../../../include" source)
    :error-patterns
    ((warning line-start (file-name) ":" line ": Warning:" (message) line-end)
     (error line-start (file-name) ":" line ": " (message) line-end))
    :modes erlang-mode)

  (add-hook 'erlang-mode-hook
            (lambda ()
              (flycheck-select-checker 'erlang-otp)
              (flycheck-mode)))

  ;; Always show diagnostics at the bottom, using 1/3 of the available space
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
		 (display-buffer-reuse-window
		  display-buffer-in-side-window)
		 (side            . bottom)
		 (reusable-frames . visible)
		 (window-height   . 0.33)))


  ;; Flycheck-tip
  (when (> use-erlang-flycheck-tip-p 0)
    (require 'flycheck-tip)
    (flycheck-tip-use-timer 'verbose)))


;;; Yasnippet (templating system)
(when (> use-erlang-yasnippet-p 0)
  (package-require 'yasnippet)
  (yas-global-mode t))


;;; Which-key integration
(when (> use-erlang-which-key-p 0)
  (package-require 'which-key)
  (add-hook 'erlang-mode-hook 'which-key-mode))

;; prevent annoying hang-on-compile
(defvar inferior-erlang-prompt-timeout t)
;; default node name to emacs@localhost
(setq inferior-erlang-machine-options '("-sname" "emacs"))
;; tell distel to default to that node
(setq erl-nodename-cache
      (make-symbol
       (concat
        "emacs@"
        ;; Mac OS X uses "name.local" instead of "name", this should work
        ;; pretty much anywhere without having to muck with NetInfo
        ;; ... but I only tested it on Mac OS X.
                (car (split-string (shell-command-to-string "hostname"))))))

;;; Distel
(when (> use-erlang-distel-p 0)
  ;; Distel (need to clone and make)
  (when (not (eq system-type 'windows-nt))
    (push "~/.emacs.d/distel/elisp/" load-path)
    (require 'distel)
    (distel-setup)
    (require 'company-distel)
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'company-distel))
    (require 'company-distel-frontend)
    nil)

  ;; Distel Windows
  ;; Problema: deixa lento e distel falha em dar "make"
  (when (file-exists-p "C:/distel-master")
    (push "C:/distel-master/elisp" load-path)
    (require 'distel)
    (distel-setup)
    ;; (require 'company-distel)
    ;; (with-eval-after-load 'company
    ;; (add-to-list 'company-backends 'company-distel))
    ;; (require 'company-distel-frontend)
    nil)


  (add-hook 'erlang-mode-hook
            (lambda ()
              (setq company-backends '(company-distel)))))
