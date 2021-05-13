;;; lm-theme.el --- Provides a theme for 256+ colors
;; -----------------------------------------------
;; Project: lm-emacs
;; Author: Jackson Benete Ferreira
;; Contact: <jacksonbenete@gmail.com>
;;
;;; Commentary:
;; This theme offers very clear colors.
;; White background and gray default foreground.
;; -----------------------------------------------

;;; Code:

(deftheme lm-cli
  "Theme for lm-emacs.")

;;; Theme
;; -----------------------------------------------
;; Set foreground to Blue Grey / L800
;; Easier on eyes than pure black letters.
(set-foreground-color "#37474F")
;; Set background to White
(set-background-color "#000000")
;; -----------------------------------------------

;;; Temporary remove hl-line (highlight line)
(set-face-attribute 'hl-line nil :inherit nil :background "gray6")

(let ((class '((class color) (min-colors 89)))
      (font (if window-system "InputMono 9" "Input Mono 9"))
      (background "#FFFFFF")
      (foreground "#37474F")
      (highlight-line "#f0f0f0")
      (cli-highlight-line-foreground "#000000")
      (cli-highlight-line-background "#FFFFFF")
      (highlight-region-fg "#99cccc")
      (highlight-region-bg "#aec6cf")
      (paren-match-fg "#00008b")
      (paren-match-bg "#aac9db")
      (paren-mismatch-fg "#ffffff")
      (paren-mismatch-bg "#ce9b9b")
      (neutral-gray "#b5d1cc")
      (mineral-gray "#9fc2b2")
      (warm-gray "#9cb29e")
      (slate-color "#1b3644")
      (eugenia-red "#ed3d66")
      (raw-sienna "#b85e00")
      (etruscan-red "#c9303e")
      (maple "#c2975a")
      (peach-red "#ff3319")
      (light-grayish-olive "#76844e")
      (dask-greenish-glaucous "#b3d9a3")
      (artemesia-green "#65a98f")
      (andover-green "#5c8a73")
      (blackish-olive "#324e2a")
      (deep-state-olive "#172713")
      (light-glaucous-blue "#a6e6db")
      (salvia-blue "#96bfe6")
      (olympic-blue "#4f8fe6")
      (tyrian-blue "#0d2b52")
      (laelia-pink "#cc85d1")
      (veronia-purple "#7e3075"))
  (custom-theme-set-faces
   'lm-cli
   ;; Text
   `(default ((,class :font ,font :background ,background :foreground ,foreground)))
   `(cursor ((,class :foreground ,foreground)))
   ;; Highlight and Selection
   `(hl-line ((,class :foreground ,cli-highlight-line-foreground
		      :background ,cli-highlight-line-background)))
   `(region ((,class :foreground ,highlight-region-fg
		     :background ,highlight-region-bg)))
   ;; What is this for?
   `(internal-border ((,class :foreground ,raw-sienna)))
   ;; Window dividers
   `(window-divider ((,class :foreground ,background
			     :background ,background)))
   `(window-divider-first-pixel ((,class :foreground ,neutral-gray
					  :background ,background)))
   `(window-divider-last-pixel ((,class :foreground ,neutral-gray
					 :background ,background)))
   ;; Programming
   `(show-paren-match ((,class :foreground ,paren-match-fg
			       :background ,paren-match-bg)))
   `(show-paren-mismatch ((,class :foreground ,paren-mismatch-fg
				  :background ,paren-mismatch-bg)))
   `(info-quoted ((,class :foreground ,warm-gray)))
   `(completions-annotations ((,class :foreground ,light-grayish-olive)))
   `(completions-common-part ((,class :foreground ,deep-state-olive)))
   `(help-argument-name ((,class :foreground ,slate-color)))
   `(line-number-current-line ((,class :foreground ,mineral-gray)))
   ;; Font-lock
   `(font-lock-comment-face ((,class :foreground ,warm-gray)))
   `(font-lock-comment-delimiter-face ((,class :foreground ,warm-gray)))
   `(font-lock-doc-face ((,class :foreground ,andover-green)))
   `(font-lock-function-name-face ((,class :foreground ,olympic-blue)))
   `(font-lock-variable-name-face ((,class :foreground ,maple)))
   `(font-lock-string-face ((,class :foreground ,artemesia-green)))
   `(font-lock-keyword-face ((,class :foreground ,laelia-pink)))
   `(font-lock-builtin-face ((,class :foreground ,laelia-pink)))
   `(font-lock-constant-face ((,class :foreground ,olympic-blue)))
   `(font-lock-warning-face ((,class :foreground ,etruscan-red)))
   `(font-lock-type-face ((,class :foreground ,light-grayish-olive)))
   ))
   

(provide-theme 'lm-cli)
(provide 'lm-cli-theme)
