;; Key bindings

(setq x-super-keysym 'alt)

; Changing arrow key functionality to scrolling
(global-set-key [up] (lambda () (interactive) (scroll-down 10)))
(global-set-key [down] (lambda () (interactive) (scroll-up 10)))
(global-set-key [left] (lambda () (interactive) (scroll-right tab-width t)))
(global-set-key [right] (lambda () (interactive) (scroll-left tab-width t)))

; moving between windows
(windmove-default-keybindings 'control)
(setq windmove-wrap-around t)

; backwards delete
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key (kbd "C-?") 'help-command)

; disable return and backspace
(global-set-key (kbd "<return>") (lambda () ))
(global-set-key (kbd "<backspace>") (lambda () ))

; search and replace
;; (global-set-key "\C-\M-s" 'search-forward-regexp)

(global-set-key [f1] 'compile)
(global-set-key [f2] 'next-error)
; eshell!!!
(global-set-key [f3] 'eshell)
; Standard terminal
(global-set-key [f4] 'ansi-term)

; Refresh buffer
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive) (revert-buffer t t))
(global-set-key [f5] 'revert-buffer-no-confirm)

(global-set-key [f6] 'athena)

; Macro quick button
(global-set-key [f7] 'kmacro-end-and-call-macro)

;; [f8] is used by reftex (org mode)

; On Mac use cmd key as meta (as in M-x)
(when (string= system-type "darwin")
  (setq x-super-keysym 'meta))


; org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
; TODO make these part of a hook, they only do stuff when in org-mode
(global-set-key "\M-M" 'org-insert-todo-heading)
(global-set-key "\M-p" 'org-up-element)
(global-set-key "\M-n" 'org-forward-element)



;;; Mac Os X stuff
;; mac cmd key as Meta (also fixes ubuntu to make meta key alt apparently)
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)
; separate tutorial [[http://mcclanahoochie.com/blog/2011/08/remapping-macbook-pro-keys-for-emacs/][separate tutorial]] 

; Go to a specific line number in the file
(global-set-key "\C-x\C-g" 'goto-line)

; Comment and uncomment selected regions
(global-set-key (kbd "C-.") 'comment-region)
(global-set-key (kbd "C-,") 'uncomment-region)


;; Shortcuts/Aliases 
(defalias 'eb 'eval-buffer)
(defalias 'er 'eval-region)
(defalias 'rb 'revert-buffer)
(defalias 'rep 'replace-string) 
(defalias 'qrep 'query-replace) 
(defalias 'erep 'replace-regexp)
(defalias 'qerep 'query-replace-regexp)
(defalias 'ff 'find-file)






