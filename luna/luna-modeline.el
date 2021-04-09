;;; luna-modeline.el --- A headerline and a modeline style.
;; -----------------------------------------------
;; File: Luna Emacs Style Configuration <luna-modeline.el>
;; Project: Luna Emacs
;; Author: Jackson Benete Ferreira
;; Contact: <jacksonbenete@gmail.com>
;; -----------------------------------------------

;;; Commentary:
;; -----------------------------------------------
;; This configuration rewrites the nano-emacs
;; modeline.
;; [https://github.com/rougier/nano-emacs]
;;
;; Although the code is inspired by nano-emacs,
;; the modeline functions are not separeted and
;; suffered a rewrite in style.
;;
;; The header-line is black and white instead
;; of gray with colorfull status.
;; And the mode-line isn't totally unused.
;; -----------------------------------------------

;;; Code:

(require 'subr-x)

;;; Header-line Style
;; -----------------------------------------------
;; Black and White header-line
(set-face-attribute 'header-line nil
		    :weight 'light
		    ;; make a white background
                    :foreground (face-foreground 'default)
                    :background (face-background 'default)
                    :overline nil
                    :underline nil
		    ;; make a box with a line around
                    :box nil
                    :box `(:line-width 1
				       :color ,(face-foreground 'default)
				       :style nil)
		    :inherit nil)

;; Reworking mode-line
;; It will always be alike doesn't matter if active or inactive
(set-face-attribute 'mode-line nil
                    :height 0.75
                    :foreground (face-foreground 'luna-neutral-gray)
                    :background (face-background 'luna-neutral-gray)
                    :overline nil
		    :underline nil
		    :box nil)
(set-face-attribute 'mode-line-inactive nil
                    :height 0.75
                    :foreground (face-foreground 'luna-neutral-gray)
                    :background (face-background 'luna-neutral-gray)
                    :overline nil
                    :underline nil
		    :inherit nil
                    :box nil)
;; -----------------------------------------------


(defun vcs-get-branch ()
  "Return branch name if file is part of a repository.
Else return nil."
  (if vc-mode
      (let* ((vcs (vc-backend buffer-file-name))
	     (branch-substring
	      (+ 2 (cond ((eq vcs 'Git) 3)
			 ((eq vcs 'Hg) 2))))
	     (branch (concat "#"
			     (substring-no-properties
			      vc-mode branch-substring))))
	branch) nil))

(defun modal-mode-p ()
  "Check for multiple modal modes packages."
  (cond (god-local-mode t)
	(t nil)))

(defun modal-mode-string ()
  "Return modal mode name as string."
  (cond (god-local-mode "GOD")
	(t nil)))

(defun lm-headerline-compose (&optional status status-face pad right-info)
  "Compose a default header-line accepting optional status and information.
Optional: use alternative status and status-face on left side.
Optional: use other information than line:column on right side.
STATUS: string.
STATUS-FACE: face.
PAD: number.
RIGHT-INFO: string."
  (let* ((flag (cond (status
		      (propertize status 'face status-face))
		     (buffer-read-only
		      (propertize " RO " 'face 'luna-warm-gray))
		     ((and buffer-file-name (buffer-modified-p))
		      (propertize " ** " 'face 'luna-peach-red))
		     (t (propertize " RW " 'face 'luna-warm-gray))))
	 (buffer-name (propertize (format-mode-line "%b")
				  'face 'luna-deep-state-olive-bold))
         (mode-name   (format-mode-line "%m"))
         (branch      (vcs-get-branch))
	 (branch-name (if branch (concat ", "
                       (propertize branch 'face 'luna-neutral-gray))))
	 (modal-mode (if (modal-mode-p)
			 (concat " [" (propertize (modal-mode-string)
						  'face 'luna-andover-green)
				 "]") ""))
	 (position    (format-mode-line "%l:%c"))
	 (current-mode (concat "(" mode-name branch-name ")" modal-mode))
	 ;; header-line configuration
	 (pad            (or pad 1))
         (space-up       +0.15)
         (space-down     -0.20)
	 ;; build header
	 (left (concat " "
		       flag
		       (propertize " " 'display `(raise ,space-up))
		       buffer-name
		       (propertize " " 'display `(raise ,space-down))
		       current-mode))
	 (right (or right-info position))
	 ;; it's important to build left and right first so you can measure
	 (available-width (- (window-body-width) (length left) pad)))
    ;; return a string which is composed of all the left arguments,
    ;; as many spaces as needed, and the right argument
    (format (format "%%s%%%ds" available-width) left right)))


(defun lm-headerline ()
  "Call the correct header depending on major mode."
  (interactive)
  (setq-default header-line-format
  '((:eval
     (cond ((lm-pdf-view-mode-p) (lm-pdf-view-mode))
	   (t (lm-headerline-compose)))))))

;;; TODO: I think doesn't need this.
;;; Because it will use both headerline and modeline?
;;; -----

(defun lm-headerline-update-windows ()
  "Refresh modeline and headerline when window configuration change."
  (dolist (window (window-list))
    (with-selected-window window
      (if (or (one-window-p t)
	      (eq (window-in-direction 'below) (minibuffer-window))
	      (not (window-in-direction 'below)))
	  (with-current-buffer (window-buffer window)
	    (setq mode-line-format (luna-modeline)))
	(with-current-buffer (window-buffer window)
 	  (setq mode-line-format (luna-modeline)))))))
(add-hook 'window-configuration-change-hook 'lm-headerline-update-windows)

;; (defun luna-modeline ()
;;   "Modeline."
;;   ;; Set modeline
;;   (setq
;;    mode-line-format
;;    '((:eval
;;       (simple-mode-line-render
;;        ;; Left.
;;        (quote (""))
;;        ;; Middle.
;;        (quote '(:eval
;; 		(if (region-active-p)
;; 		    (format
;; 		     (concat "[Selected: "
;; 			     (number-to-string (- (region-end) (region-beginning)))
;; 			     "]"))
;; 		  "")))
;;        ;; Right.
;;        (quote ("[%p]"))))))
;;   (defun simple-mode-line-render (left middle right)
;;   "Return a string of `window-width' length.
;; Containing LEFT, MIDDLE and RIGHT aligned respectively."
;;   (let* ((max-size (- (window-body-width) 6))
;; 	 (half-size (/ max-size 2))
	 
;; 	 (left-len (length (format-mode-line left)))
;; 	 (middle-len (length (format-mode-line middle)))
;; 	 (right-len (length (format-mode-line right)))

;; 	 (left-space (- half-size left-len middle-len))
;; 	 (right-space (- left-space right-len)))
;;     (append left
;; 	    (list (format (make-string left-space ?\-) ""))
;; 	    middle
;;             (list (format (make-string right-space ?\-) ""))
;;             right))))
;; (luna-modeline)

(defun luna-modeline ()
  "Modeline."
  (list '(:eval
	  (if (region-active-p)
	      (let* ((max-size (- (window-body-width) 6))
		     (half-size (/ max-size 2))
		     (modeline-string (format
				       (concat "[Selected: "
					       (number-to-string (- (region-end) (region-beginning)))
					       "]")))
		     (string-len (length modeline-string)))
		(list (list (format (make-string (- half-size 0) ?\-) ""))
		      modeline-string
		      "%-"))
	    "%-"))))

(setq eshell-status-in-modeline nil)
(setq-default mode-line-format (luna-modeline))

(lm-headerline)


;;; Extensions
;; -----------------------------------------------
(defun lm-pdf-view-mode-p ()
  "Return non-nil if pdf-view-mode is active."
  (derived-mode-p 'pdf-view-mode))

(defun lm-pdf-view-mode ()
  "Format headerline for pdf-view-mode."
  (let ((page-number (concat " P "
		      (number-to-string (pdf-view-current-page)) "/"
		      (or (ignore-errors
			    (number-to-string (pdf-cache-number-of-pages)))
			  "???"))))
    (lm-headerline-compose " PDF " nil nil page-number)))
;; -----------------------------------------------


(provide 'luna-modeline)
;;; luna-modeline.el ends here