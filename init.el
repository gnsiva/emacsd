(load "~/.emacs.d/org-mode-gs.el")
(load "~/.emacs.d/shortcuts-gs.el")
(load "~/.emacs.d/functions-gs.el")
(load "~/.emacs.d/packages-gs.el")
(load "~/.emacs.d/general-gs.el")
(load "~/.emacs.d/paths-gs.el")
(load "~/.emacs.d/python-gs.el")

;; Remember open buffers when reopening
(desktop-save-mode 1)
(find-file "~/.emacs.d/init.el")

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
 '(custom-enabled-themes (quote (wombat)))
 '(org-agenda-files (quote ("~/Dropbox/workspaces/Challenger/Challenger.org")))
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 25)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; (setenv "PYTHONPATH" "~/Challenger/python; ~/Amphitrite/")



;; ================================================================
;; Extra stuff from youtube presentation which should be moved
;; (ac-config-default)
(setq ac-show-menu-immediately-on-auto-complete t)

;; projectile (need to add install bit)
;; (require 'projectile)
;; (projectile-global-mode)

;; Jedi setup
;; You only need to install pip and virtualenv to use this and it installs jedi and epc for you
;; M-x jedi:install-server
; also had this line which I don't know the purpose of
; it adds jedi to the autocomplete sources list, but I think I already have something for that
;; (add-to-list 'ac-sources 'ac-source-jedi-direct)

;; add jedi:server-args for showing what your projects are
; C-? v jedi:server-args tells you how to set this up
;; finding a project --sys-path (use this to tell it where your projects are)

;; autofind a project root
;; (defvar jedi-config:vcs-root-sentinel ".git")

; incomplete
;; (defun get-project-root (buf repo-type init-file))

(setq jedi:complete-on-dot t)

;; should look at this to get this whole thing to work properly
;; https://github.com/wernerandrew/jedi-starter/blob/master/jedi-starter.el
; Also downloaded it to ~/Programs on lettie

; have a look at ido-vertical-mode 

(set-cursor-color "Firebrick1")
