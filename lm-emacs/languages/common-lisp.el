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

