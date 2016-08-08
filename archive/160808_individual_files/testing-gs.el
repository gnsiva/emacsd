;; taken from: http://www.emacswiki.org/emacs/SwitchingBuffers
;; problem is it reverts to ones which are still open and displayed
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(global-font-lock-mode 1)

; Disable the splash screen (to enable it agin, replace the t with 0)
;(setq org-log-done t) ; think this puts a time stamp when you complete a TODO item
(setq inhibit-splash-screen t)
(setq org-agenda-files '("~/Dropbox/musings/org_mode_notes"))
;(setq org-agenda-tex-search-extra-files 
;      (append (find-org-file-recursively "~/Dropbox/musings/org_mode_notes" "org")))

; remap suggest tag
;(global-set-key (kbd "<f12>") 'complete-tag)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PYTHON
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Use iPython
;; (setq
;;  python-shell-interpreter "ipython"
;;  py-shell-name "ipython"
;;  python-shell-interpreter-args ""
;;  python-shell-prompt-regexp "In \\[[0-9]+\\]: "
;;  python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
;;  python-shell-completion-setup-code
;;    "from IPython.core.completerlib import module_completion"
;;  python-shell-completion-module-string-code
;;    "';'.join(module_completion('''%s'''))\n"
;;  python-shell-completion-string-code
;;    "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
;; ;(setq python-shell-interpreter "ipython")


(defun gish-set-ipython-shortcuts ()
  (local-set-key (kbd "C-c C-c") 'py-execute-buffer-ipython)
  (local-set-key (kbd "C-c |") 'py-execute-region-ipython)  
)
(add-hook 'python-mode-hook 'gish-set-ipython-shortcuts)

(show-paren-mode t)

;;;;;;;;;;;;;;;;
;; Mac only
(if (equal system-type "darwin")
    ; See: http://ergoemacs.org/emacs/emacs_env_var_paths.html
    (setenv "PYTHONPATH"
	    (concat
	     "/Users/ganesh/Dropbox/workspaces/Amphitrite_2.1" ":"
	     (getenv "PYTHONPATH")
	     )
    )
)

;;;;;;;;;;;;;;;;
;; Windows only
(if (equal system-type "windows-nt")
    (if (file-directory-p "c:/cygwin/bin")
	(add-to-list 'exec-path "c:/cygwin/bin")
    )
    ; SSH stuff
    (setq shell-file-name "bash")
    (setq explicit-shell-file-name shell-file-name)
    ; Setup TRAMP
    (cond  ((eq window-system 'w32)
      (setq tramp-default-method "scpx"))
      (t
      (setq tramp-default-method "scpc")))

)

