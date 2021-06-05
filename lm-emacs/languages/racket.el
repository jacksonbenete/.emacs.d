;;; racket-mode:
;; -----------------------------------------------
;; [https://github.com/greghendershott/racket-mode]
;; Racket IDE and REPL
(require 'racket-xp)
(add-hook 'racket-mode-hook #'racket-xp-mode)
;; -----------------------------------------------

