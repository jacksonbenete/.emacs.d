;; -----------------------------------------------
;; File: Luna Emacs Key Bindings
;; Project: Luna Emacs
;; Author: Jackson Benete Ferreira
;; Contact: <jacksonbenete@gmail.com>
;; -----------------------------------------------

;;; About:
;; -----------------------------------------------
;; Key Bindings for QoL Improvements
;; every keybinding is first unset
;; So it will prevent conflict and also offer
;; an overview of all keys in use.
;; -----------------------------------------------

;;; Unset Keys:
;; -----------------------------------------------
(global-unset-key (kbd "C-x C-k"))        ; Kill current buffer without prompt
(global-unset-key (kbd "C-,"))            ; Jump to BoF
(global-unset-key (kbd "C-."))            ; Jump to EoF
(global-unset-key (kbd "C-y"))            ; Yank and newline
(global-unset-key (kbd "M-y"))            ; Yank
(global-unset-key (kbd "C-`"))            ; Delete other windows
(global-unset-key (kbd "C-<dead-grave>")) ; Delete other windows (intl layout)
(global-unset-key (kbd "C-="))            ; Increase Text Size
(global-unset-key (kbd "C-+"))            ; Increase Text Size
(global-unset-key (kbd "C--"))            ; Decrease Text Size
(global-unset-key (kbd "C-;"))            ; Comment Line
(global-unset-key (kbd "<f5>"))           ; Set bookmark
(global-unset-key (kbd "<f6>"))           ; List bookmarks
(global-unset-key (kbd "<f12>"))          ; Eval-buffer
(global-unset-key (kbd "C-<f5>"))         ; Set bookmark
(global-unset-key (kbd "C-<f6>"))         ; List bookmarks
(global-unset-key (kbd "C-<f12>"))        ; Eval-buffer
(global-unset-key (kbd "<C-tab>"))        ; Change other window C-x o
(global-unset-key (kbd "<backtab>"))      ; Change other window C-x o
(global-unset-key (kbd "C-<backspace>"))  ; Custom delete function
(global-unset-key (kbd "C-_"))            ; Create block comment ;;--------- etc
(global-unset-key (kbd "C-x C-x"))        ; Alternative to M-x
;; (global-unset-key (kbd ""))

;;; Unsetting for being annoying
(global-unset-key (kbd "<insert>"))       ; Cancel overwrite-mode
(global-unset-key (kbd "<insertchar>"))   ; Cancel overwrite-mode
(global-unset-key (kbd "C-1"))            ; 
(global-unset-key (kbd "C-2"))            ; 
(global-unset-key (kbd "C-3"))            ; 
(global-unset-key (kbd "C-4"))            ; 
(global-unset-key (kbd "C-5"))
(global-unset-key (kbd "C-6"))           
(global-unset-key (kbd "C-7"))
(global-unset-key (kbd "C-8"))            ; 
(global-unset-key (kbd "C-9"))
(global-unset-key (kbd "M-0"))
(global-unset-key (kbd "M-1"))            
(global-unset-key (kbd "M-2"))
(global-unset-key (kbd "M-3"))            
(global-unset-key (kbd "M-4"))            
(global-unset-key (kbd "M-5"))
(global-unset-key (kbd "M-6"))           
(global-unset-key (kbd "M-7"))
(global-unset-key (kbd "M-8"))
(global-unset-key (kbd "M-9"))
(global-unset-key (kbd "M-0"))
;; -----------------------------------------------


;;; Emacs Functions:
;;; QoL Improvements:


;;; eval-buffer shortcut
;; -----------------------------------------------
(global-set-key (kbd "<f12>") 'eval-buffer)
(global-set-key (kbd "C-<f12>") 'eval-buffer)
;; -----------------------------------------------


;;; Bookmark
;; -----------------------------------------------
(global-set-key (kbd "<f5>") 'bookmark-set)
(global-set-key (kbd "C-<f5>") 'bookmark-set)
;; -----------------------------------------------


;;; Prevent C-z
;; -----------------------------------------------
(put 'suspend-frame 'disabled t)
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-z"))
;; -----------------------------------------------


;;; Remap Jump to EOF/BOF
;; -----------------------------------------------
(global-set-key (kbd "C-,") 'beginning-of-buffer)
(global-set-key (kbd "C-.") 'end-of-buffer)
;; -----------------------------------------------


;;; Remap Delete Other Windows
;; -----------------------------------------------
;; Instead of C-x 1
(global-set-key (kbd "C-`") 'delete-other-windows)
(global-set-key (kbd "C-<dead-grave>") 'delete-other-windows)
;; -----------------------------------------------


;;; Increase and Decrease Text Size
;; -----------------------------------------------
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
;; -----------------------------------------------


;;; Remap Jump to Other Window
;; -----------------------------------------------
(global-set-key (kbd "<C-tab>") 'other-window)
(global-set-key (kbd "<backtab>") 'other-window)
;; -----------------------------------------------


;;; Remap Comment Line
;; -----------------------------------------------
(global-set-key (kbd "C-;") 'comment-line)
;; -----------------------------------------------


;;; Open Shell or PowerShell
;; -----------------------------------------------
(defun choose-a-shell nil
  (interactive)
  (if (string-equal system-type "windows-nt")
      (powershell)
    (shell)))
;; (global-set-key (kbd "C-3") 'choose-a-shell)
;; (global-set-key (kbd "C-4") 'eshell)
;; -----------------------------------------------


;;; Luna Functions


;;; Insert Comment Block
;; -----------------------------------------------
(defun comment-style-separation-block ()
  (interactive)
  (beginning-of-line)
  (insert ";; -----------------------------------------------")
  (newline))
(global-set-key (kbd "C-_") 'comment-style-separation-block)
;; -----------------------------------------------


;;; Remap yank/paste (C-y)/(M-y)
;; -----------------------------------------------
(defun mimic-vscode-paste ()
  (interactive)
  (open-line 1)
  (yank)
  (move-beginning-of-line 1)
  (next-line 1))
(global-set-key (kbd "C-y") 'mimic-vscode-paste)
(global-set-key (kbd "M-y") 'yank)
;; -----------------------------------------------


;;; Remap New Scratch Window
;; -----------------------------------------------
;; Instead of C-x 4 f
(defun create-scratch-buffer nil
  "create a scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))
;; (global-set-key (kbd "C-2") 'create-scratch-buffer)
;; -----------------------------------------------


;;; Remap backward-kill-word (C-<backspace>)
;; -----------------------------------------------
(defun aborn/backward-kill-word ()
  "Customize/Smart backward-kill-word."
  (interactive)
  (let* ((cp (point))
         (backword)
         (end)
         (space-pos)
         (backword-char (if (bobp)
                            ""           ;; cursor in begin of buffer
                          (buffer-substring cp (- cp 1)))))
    (if (equal (length backword-char) (string-width backword-char))
        (progn
          (save-excursion
            (setq backword (buffer-substring (point) (progn (forward-word -1) (point)))))
          (setq ab/debug backword)
          (save-excursion
            (when (and backword          ;; when backword contains space
                       (s-contains? " " backword))
              (setq space-pos (ignore-errors (search-backward " ")))))
          (save-excursion
            (let* ((pos (ignore-errors (search-backward-regexp "\n")))
                   (substr (when pos (buffer-substring pos cp))))
              (when (or (and substr (s-blank? (s-trim substr)))
                        (s-contains? "\n" backword))
                (setq end pos))))
          (if end
              (kill-region cp end)
            (if space-pos
                (kill-region cp space-pos)
              (backward-kill-word 1))))
      (kill-region cp (- cp 1)))))         ;; word is non-english word
    
(global-set-key (kbd "C-<backspace>") 'aborn/backward-kill-word)
;; -----------------------------------------------


;;; Kill buffer
;; -----------------------------------------------
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
;; -----------------------------------------------


;;; Find file and insert
;; -----------------------------------------------
(defun get-path-name ()
  (interactive)
  "Find a file or folder and insert path as text on cursor."
  (insert (expand-file-name (read-file-name "Select path: "))))
(global-set-key (kbd "C-x C-p") 'get-path-name)
;; -----------------------------------------------


;;; End:
