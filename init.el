;;; Begin initialization
;; Turn off mouse interface early in startup to avoid momentary display
(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(setq inhibit-startup-message t)

;(require 'let-alist)
;;; Set up package
;; (require 'package)
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.org/packages/") t)
;; (package-initialize)
;; There are extra package repos in the archived packages-gs.el file
(require 'package)
(package-initialize)

(setq package-archives '(("melpa" . "http://melpa.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("gnu" . "http://elpa.gnu.org/packages/")))
(setq package-check-signature nil)
(package-refresh-contents)


;;; Install use-package if not installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; From use-package README
(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)
;; (setq use-package-verbose t)

;; TODO maybe want to remove this
(server-start) 

;; Load my emacs config file
(org-babel-load-file (concat user-emacs-directory "emacs-config.org"))


; had to move this to init.el because it is overwritten
(set-cursor-color "Royal Blue") 
