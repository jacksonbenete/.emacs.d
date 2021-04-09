;;; luna-layout.el --- Layout configurations.

;; -----------------------------------------------
;; File: Luna Emacs Style Configuration <luna-layout.el>
;; Project: Luna Emacs
;; Author: Jackson Benete Ferreira
;; Contact: <jacksonbenete@gmail.com>
;; -----------------------------------------------

;;; Commentary:
;; -----------------------------------------------
;; This configuration is inspired on the early days
;; of nano-emacs: [https://github.com/rougier/nano-emacs]
;; -----------------------------------------------

;;; Code:

(defgroup luna nil
  "Faces for the luna customization"
  :group 'emacs)

(defface luna-neutral-gray nil
  ""
  :group 'luna)
(set-face-attribute 'luna-neutral-gray nil
		    :foreground "#b5d1cc")

(defface luna-mineral-gray nil
  ""
  :group 'luna)
(set-face-attribute 'luna-mineral-gray nil
		    :foreground "#9fc2b2")

(defface luna-warm-gray nil
  ""
  :group 'luna)
(set-face-attribute 'luna-warm-gray nil
		    :foreground "#9cb29e")
(defface luna-peach-red nil
  ""
  :group 'luna)
(set-face-attribute 'luna-peach-red nil
		    :foreground "#ff3319")
(defface luna-deep-state-olive nil
  ""
  :group 'luna)
(set-face-attribute 'luna-deep-state-olive nil
		    :foreground "#172713")

(defface luna-deep-state-olive-bold nil
  ""
  :group 'luna)
(set-face-attribute 'luna-deep-state-olive-bold nil
		    :foreground "#172713"
		    :weight (if (display-graphic-p) 'medium 'bold))

(defface luna-andover-green nil
  ""
  :group 'luna)
(set-face-attribute 'luna-andover-green nil
		    :foreground "#5c8a73")

;;; Layout definitions
;; -----------------------------------------------
;; -> Change left-fringe from 0 to 4,
;;    or else, diff-hl doesn't work
;;    alternatively enable (diff-hl-margin-mode t)
;;    and let fringe be zero.
(setq default-frame-alist
      (append (list
	       (if window-system (quote (font . "InputMono 9")) (quote (font . "Input Mono 9")))
	       '(min-height . 1)  '(height     . 45)
	       '(min-width  . 40) '(width      . 81)
               '(vertical-scroll-bars . nil)
               '(internal-border-width . 24)
               '(left-fringe    . 0)
               '(right-fringe   . 0)
               '(tool-bar-lines . 0)
               '(menu-bar-lines . 0))))
(diff-hl-margin-mode t)
;; -----------------------------------------------


;;; Default Startup Inhibition
;; -----------------------------------------------
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil)
;; -----------------------------------------------


;;; Frame Configuration
;; -----------------------------------------------
;; Don't show bar of icons on top of frame
(tool-bar-mode 0)

;; Don't show tooltip on mouse over
(tooltip-mode 0)

;; Highlight current line
(global-hl-line-mode 1)

;; Create aditional space on modeline/headerline
;; See images on https://github.com/bbatsov/solarized-emacs/issues/113
(setq x-underline-at-descent-line t)
;; -----------------------------------------------


;;; Vertical window divider
;; -----------------------------------------------
;; Create a space between windows in the same frame
;; Used to be 24
(setq window-divider-default-right-width 12)
(setq window-divider-default-places 'right-only)
(window-divider-mode 1)
;; -----------------------------------------------


;;; Minibuffer
;; -----------------------------------------------
(dolist (buffer (list " *Minibuf-0*" " *Echo Area 0*"
                      " *Minibuf-1*" " *Echo Area 1*"))
  (when (get-buffer buffer)
    (with-current-buffer buffer
      (face-remap-add-relative 'default 'luna-andover-green))))

;;; Minibuffer background
;; (dolist (buf (list " *Minibuf-0*" " *Echo Area 0*"
;;                       " *Minibuf-1*" " *Echo Area 1*"))
;;   (with-current-buffer (get-buffer buf)
;;     (make-local-variable 'face-remapping-alist)
;;     (add-to-list 'face-remapping-alist '(default (:background "green")))))
;; -----------------------------------------------


(provide 'luna-layout)

;;; luna-layout.el ends here