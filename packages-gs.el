;; Requisites: Emacs >= 24
(require 'package)
(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
	     '("gnu" . "http://elpa.gnu.org/packages/"))

(package-refresh-contents)
(defun install-if-needed (package)
  (unless (package-installed-p package)
    (package-install package)))

(package-refresh-contents)
(defun install-if-needed (package)
  (unless (package-installed-p package)
    (package-install package)))

;; make more packages available with the package installer
(setq to-install
      '(python-mode cl-lib yasnippet jedi auto-complete autopair find-file-in-repository exec-path-from-shell))

; Comment out for offline
(mapc 'install-if-needed to-install)

;; (require 'magit)
;; (global-set-key "\C-xg" 'magit-status)
(global-set-key [f7] 'find-file-in-repository)



;;;;;;;;;;;;;;;;;;;;;;;;;;;
; These parts should be moved
(require 'flymake)

; yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs 
      '("~/Dropbox/musings/config_files/yasnippets"))
(yas-global-mode 1)

; autopair
(require 'autopair)
(autopair-global-mode 1)
(setq autopair-autowrap t)

; auto-complete mode extra settings
(require 'auto-complete)
(setq
 ac-auto-start 2
 ac-override-local-map nil
 ac-use-menu-map t
 ac-candidate-limit 20)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Not this bit though!
(mapc 'install-if-needed to-install)



