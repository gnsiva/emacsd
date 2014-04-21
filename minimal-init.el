(load "~/.emacs.d/org-mode-gs.el")
(load "~/.emacs.d/shortcuts-gs.el")
(load "~/.emacs.d/functions-gs.el")
(load "~/.emacs.d/general-gs.el")


(defun org-export-region-to-pdf ()
  (interactive)
  (kill-ring-save)
  (switch-to-buffer (make-temp-name "tempExport"))
  ;; (yank)
  (org-mode)
) 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/Dropbox/workspaces/Challenger/Challenger.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

