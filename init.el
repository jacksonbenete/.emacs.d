;;; init.el --- Emacs configuration file.

;; -------------------------------------------------------------------
;; File: Luna Emacs Configuration
;; Project: Luna Emacs
;; Author: Jackson Benete Ferreira
;; Contact: <jacksonbenete@gmail.com>
;; -------------------------------------------------------------------

;;; Commentary:
;; -------------------------------------------------------------------
;; This configuration is based on:
;; [https://github.com/rememberYou/.emacs.d]
;; [https://emacs.nasy.moe]
;; [https://github.com/zamansky/dot-emacs]
;; And is now wrote on top of elegant-emacs and nano-emacs:
;; [https://github.com/rougier/elegant-emacs]
;; [https://github.com/rougier/nano-emacs]
;; -------------------------------------------------------------------

;;; Code:

;;; Garbage Collection
;; -------------------------------------------------------------------
;; Read more on: https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
;; Reduce garbage collection frequency to speed up startup

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq gc-cons-threshold (* 50 1000 1000))
(add-to-list 'load-path "~/.emacs.d/gcmh")
(require 'gcmh)
(gcmh-mode 1)
;; -------------------------------------------------------------------


;;; Speed up
;; -------------------------------------------------------------------
;; don't require or load but instead, they should just
;; use setq, add-hook, and eval-after-load.
;; Inhibit font compacting for Windows speed up
(setq inhibit-compacting-font-caches t)
(setq frame-inhibit-implied-resize t)
;; (setq initial-major-mode 'fundamental-mode)
;; -------------------------------------------------------------------


;;; Initialization:
;; -------------------------------------------------------------------
;;;; Packages:
;; (require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
;; -------------------------------------------------------------------


;;; Quality of Life
;; -------------------------------------------------------------------
;;; Remove GUI toolbar
(when window-system ; will not if `emacs -nw`
  (menu-bar-mode -1)
  (tool-bar-mode -1))

;; Echo key sequences in minibuffer without delay
(setq echo-keystrokes 0.1)

;;; Change the default directory (for when using on Windows)
(setq default-directory (concat (getenv "HOME") "/"))


;;; Luna Libraries:
(add-to-list 'load-path "~/.emacs.d/luna/")


;;; Luna Theme
;; -----------------------------------------------
(require 'luna-layout)
(setq custom-theme-directory "~/.emacs.d/luna/")
(load-theme 'luna t)
(require 'luna-modeline)
;; -----------------------------------------------

;;; Luna Dashboard
(add-to-list 'load-path "~/.emacs.d/lm-dashboard")
(require 'lm-dashboard)
(lm-dashboard-startup-hook)

;;; Emacs Libraries:
(load-library "luna-emacs-packages")

;;; Third Packages:
(load-library "luna-third-packages")

;;; Custom Keybindings
(load-library "luna-keys")

;;; RPG modes
;;(add-to-list 'load-path "~/.emacs.d/rpg")
;;(require 'roll)
;;(global-set-key (kbd "C-0") 'roll-save)

;;; Checksum
;;(add-to-list 'load-path "~/.emacs.d/emacs-checksum")
;;(require 'checksum)

;;; Horn
;;(add-to-list 'load-path "~/.emacs.d/emacs-horn")
;;(require 'horn)
;;(setq horn-default-list
;;      '(("Dashboard" . lm-dashboard-refresh)
;;	("New Scratch" . create-scratch-buffer)
	;; ("Shell" . shell)
;;	("Shell" . eshell)
;;	("Deft" . deft)
;;	("Magit" . magit)
;;	("roll-save" . roll-save)
;;	("rss" . newsticker-treeview)
;;	("Slime" . slime)
;;	("checksum" . checksum)
;;	("gnus" . gnus)))
;;(global-set-key (kbd "C-x C-x") 'horn-call-mode)

;;; Ignore active buffer while leaving
(setq confirm-kill-processes nil)

;;; Prevent new line when saving file
(setq require-final-newline nil)
(setq mode-require-final-newline nil)

;;; Swap [] to ()
(keyboard-translate ?\[ ?\()
(keyboard-translate ?\] ?\))
(keyboard-translate ?\{ ?\[)
(keyboard-translate ?\} ?\])
(keyboard-translate ?\( ?\{)
(keyboard-translate ?\) ?\})
;; -------------------------------------------------------------------


;;; Garbage Collection:
;; -------------------------------------------------------------------
;; Stabilize Garbage collection (decrease threshold)
(setq gc-cons-threshold (* 10 1000 1000))
;; -------------------------------------------------------------------


;;; Emacs Daemon
;; -------------------------------------------------------------------
;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server."
  (interactive)
  (save-some-buffers)
  (kill-emacs)
  )
;; -------------------------------------------------------------------


;;; End:
(require 'use-package)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(markdown-preview-mode indium tide emmet-mode add-node-modules-path web-mode racket-mode geiser elisp-slime-nav cider clojure-mode undo-tree which-key flycheck easy-kill hyperbole icomplete-vertical slime-company eshell-bookmark tramp use-package tracking pdf-tools move-text magit helpful god-mode elpher diff-hl deft dashboard)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-hl-change ((t (:background "#3a81c3"))))
 '(diff-hl-delete ((t (:background "#ee6363"))))
 '(diff-hl-insert ((t (:background "#7ccd7c"))))
 '(mode-line ((t (:box (:line-width 2 :color "white"))))))

;;; init.el ends here