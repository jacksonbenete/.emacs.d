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


;;; Docker CLI Emacs
;; -----------------------------------------------
(when (not (display-graphic-p))
  (load-library "luna-docker"))


;;; Luna Theme
;; -----------------------------------------------
(require 'luna-layout)
(setq custom-theme-directory "~/.emacs.d/luna/")
(load-theme 'luna t)
(require 'luna-modeline)
;; -----------------------------------------------

;;; Luna Dashboard
(add-to-list 'load-path "~/.emacs.d/lm-dashboard")
(when (display-graphic-p)
  (require 'lm-dashboard)
  (lm-dashboard-startup-hook))

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
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (luna)))
 '(custom-safe-themes
   (quote
    ("a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" "0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" "7d0bc790f1577ae0e4a35db83412e4f0278381e499d8cebc3e6caa0b5287efd4" "183a60ea52dc02a766f2f00d2eceb076d9ab0e1cc522e03f984bb536b6b50b42" "0a92739657fcf19549383841bccfdb604569f7d8e156a6f2ded7eefe4e134271" "197cefea731181f7be51e9d498b29fb44b51be33484b17416b9855a2c4243cb1" default)))
 '(fci-rule-color "#eee8d5")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(hl-paren-colors (quote ("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900")))
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (docker-tramp lfe-mode pdf-tools markdown-preview-mode indium tide emmet-mode add-node-modules-path web-mode racket-mode geiser elisp-slime-nav cider clojure-mode undo-tree which-key flycheck easy-kill hyperbole icomplete-vertical slime-company eshell-bookmark tramp use-package tracking move-text magit helpful god-mode elpher diff-hl deft dashboard)))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#e52a72950000")
     (60 . "#e52aabe00000")
     (80 . "#b58900")
     (100 . "#e52ae52a0000")
     (120 . "#e52ae52a0000")
     (140 . "#e52ae52a0000")
     (160 . "#e52ae52a0000")
     (180 . "#859900")
     (200 . "#98c7e52a4c63")
     (220 . "#7295e52a7295")
     (240 . "#4c63e52a98c7")
     (260 . "#2631e52abef8")
     (280 . "#2aa198")
     (300 . "#0000e52ae52a")
     (320 . "#0000e52ae52a")
     (340 . "#0000e52ae52a")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
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