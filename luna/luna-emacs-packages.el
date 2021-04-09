;; -------------------------------------------------------------------
;; File: Luna Emacs Native Packages Customization
;; Project: Luna Emacs
;; Author: Jackson Benete Ferreira
;; Contact: <jacksonbenete@gmail.com>
;; -------------------------------------------------------------------


;;; Dired
;; -----------------------------------------------
;; Extra configurations for dired
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first")
	   (dired-dwim-target t)
	   (delete-by-moving-to-trash t)))
;; -----------------------------------------------


;;; Icomplete
;; -----------------------------------------------
;; (icomplete-mode t)
;; (setq completion-auto-help nil)
;; (defun icomplete-styles ()
;;   (setq-local completion-styles '(substring flex))
;;   (setq completion-ignore-case t))
;; (add-hook 'icomplete-minibuffer-setup-hook 'icomplete-styles)
;; ;; (add-hook 'icomplete-minibuffer-setup-hook #'visual-line-mode)
;; (savehist-mode 1)

;; -----------------------------------------------


;;; Fido-mode
;; -----------------------------------------------
;; Fido turns icomplete to be more like ido
;; (fido-mode 1)
;; -----------------------------------------------


;;; Mail-mode:
;; -------------------------------------------------------------------
;; If another public key is needed (expires 2022), do the following:
;; gpg --gen-keys
;; gpg --fingerprint jacksonbenete@gmail.com
;; the file is configured at ~/.emacs.d/.gnus.el
;; mail-mode (Rmail)
(setq send-mail-command 'smtpmail-send-it)
;; message-mode (Gnus)
(setq message-send-mail-function 'smtpmail-send-it)

(setenv "MAILHOST" "imap.gmail.com")
(setenv "MAIL" (concat "imaps://jacksonbenete"
			 ":cvsbyhkgxwupmjgy"
			 "@gmail.com"))

(setq
 send-mail-function 'smtpmail-send-it
 rmail-preserve-inbox t
 rmail-file-name "~/mail/rmail"

 user-full-name "Jackson Benete Ferreira"
 user-mail-address "jacksonbenete@gmail.com"
 mml2015-signers '("3032E520") ;; last 8 digits from fingerprint
 gnus-select-method
 '(nnimap "gmail"
          (nnimap-address "imap.gmail.com")
          (nnimap-server-port 993)
          (nnimap-stream ssl))
 smtpmail-smtp-server "smtp.gmail.com"
 smtpmail-smtp-service 587
 message-send-mail-function 'smtpmail-send-it

 nntp-authinfo-file "~/.authinfo.gpg"

 gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"

 ;; The agent seems to confuse nnimap, therefore we'll disable it.
 gnus-agent nil

 ;; We don't want local, unencrypted copies of emails we write.
 gnus-message-archive-group nil

 ;; We want to be able to read the emails we wrote.
 mml2015-encrypt-to-self t)

;; -------------------------------------------------------------------


;;; Linum and Column-Number:
;; -------------------------------------------------------------------
;; [https://www.emacswiki.org/emacs/LineNumbers]
;; Display Line Numbers
(require 'display-line-numbers)
(defcustom display-line-numbers-exempt-modes '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode)
  "Major modes on which to disable the linum mode, exempts them from global requirement"
  :group 'display-line-numbers
  :type 'list
  :version "green")
(defun display-line-numbers--turn-on ()
  "turn on line numbers but excempting certain majore modes defined in `display-line-numbers-exempt-modes'"
  (if (and
       (not (member major-mode display-line-numbers-exempt-modes))
       (not (minibufferp)))
      (display-line-numbers-mode)))

(global-display-line-numbers-mode)

;; Display column number
(column-number-mode)
;; -------------------------------------------------------------------


;;; recentf-mode:
;; -------------------------------------------------------------------
;; [https://www.emacswiki.org/emacs/RecentFiles]
;; List recently opened files
(setq recentf-auto-cleanup 'never)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
;; -------------------------------------------------------------------


;;; hl-line
;; -------------------------------------------------------------------
;; [https://www.emacswiki.org/emacs/HighlightCurrentLine]
;; Highlight current line
(global-hl-line-mode 1)
;; -------------------------------------------------------------------


;;; iBuffer
;; -------------------------------------------------------------------
;; [https://www.emacswiki.org/emacs/IbufferMode]
;; Improved version of "buffer" (C-x C-b)
(global-set-key (kbd "C-x b") 'ibuffer) ; C-x b instead, C-x C-b is for horn.
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("org" (name . "^.*org$"))
               ("magit" (mode . magit-mode))
               ("IRC" (or (mode . circe-channel-mode) (mode . circe-server-mode)))
               ("web" (or (mode . web-mode) (mode . js2-mode)))
               ("shell" (or (mode . eshell-mode) (mode . shell-mode)))
               ("mu4e" (or

                        (mode . mu4e-compose-mode)
                        (name . "\*mu4e\*")))
                        
               ("programming" (or
                               (mode . clojure-mode)
                               (mode . clojurescript-mode)
                               (mode . python-mode)
                               (mode . c++-mode)))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))))))
               
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "default")))

;; don't show these
;;(add-to-list 'ibuffer-never-show-predicates "zowie")
;; Don't show filter groups if there are no buffers in that group
(setq ibuffer-show-empty-filter-groups nil)

;; Don't ask for confirmation to delete marked buffers
(setq ibuffer-expert t)
;; -------------------------------------------------------------------


;;; Paren-mode
;; -------------------------------------------------------------------
(show-paren-mode 1)
;; -------------------------------------------------------------------


;;; Eletric-Pair
;; -------------------------------------------------------------------
;; Alternative to autopair/smartparens/parinfer, etc.
;; Non-intrusive. Will only open parens, not barf or anything else.
(electric-pair-mode 1)
;; -------------------------------------------------------------------


;; whitespace-mode
;; -------------------------------------------------------------------
(setq-default whitespace-style
        '(face           ; needed for the rest to take effect
          ;; newline-mark   ; for the newline mark
          lines-tail))   ; for the line-column

;; this is to change the newline-mark icon
(setq whitespace-display-mappings
      '((newline-mark 10 [183 10])))

(setq-default whitespace-line-column 80)
(global-whitespace-mode 1)

;; (set-face-attribute 'whitespace-newline nil
        ;; :foreground "slate grey")
;; -------------------------------------------------------------------


;; newsticker
;; -------------------------------------------------------------------
;; rss viewer
;; (add-hook 'newsticker-mode-hook 'imenu-add-menubar-index)
;; -------------------------------------------------------------------


;; End: