;;; luna-docker.el --- Configuration for CLI on Docker lfex/lfe
;;; Commentary:
;;
;; For use with lfex/lfe docker container
;;
;;; Code:

(require 'use-package)

(use-package ivy-erlang-complete
  :ensure t)

(when (file-directory-p "/opt/erlang/lfe")
  (use-package erlang
    :load-path ("/opt/erlang/lfe/emacs/")
    :hook (after-save . ivy-erlang-complete-reparse)
    :custom (ivy-erlang-complete-erlang-root "/opt/erlang/lfe")
    :config (ivy-erlang-complete-init)
    :mode (("\\.erl?$" . erlang-mode)
	   ("rebar\\.config$" . erlang-mode)
	   ("relx\\.config$" . erlang-mode)
	   ("sys\\.config\\.src$" . erlang-mode)
	   ("sys\\.config$" . erlang-mode)
	   ("\\.config\\.src?$" . erlang-mode)
	   ("\\.config\\.script?$" . erlang-mode)
	   ("\\.hrl?$" . erlang-mode)
	   ("\\.app?$" . erlang-mode)
	   ("\\.app.src?$" . erlang-mode)
	   ("\\Emakefile" . erlang-mode))))