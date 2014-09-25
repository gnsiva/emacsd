;; Requisites: Emacs >= 24
(require 'package)
(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))
;; uncomment this, was not working for some reason
;; (add-to-list 'package-archives
;; 	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
	     '("gnu" . "http://elpa.gnu.org/packages/"))

(package-refresh-contents)
(defun install-if-needed (package)
  (unless (package-installed-p package)
    (package-install package)))

;; make more packages available with the package installer
(setq to-install
      '(python-mode cl-lib yasnippet jedi auto-complete autopair find-file-in-repository exec-path-from-shell magit auctex htmlize org emacs-eclim gtags smartscan nxml))

(mapc 'install-if-needed to-install)

(require 'magit)
(global-set-key "\C-xg" 'magit-status)
(global-set-key [f7] 'find-file-in-repository)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; These parts should be moved
;(require 'flymake)

;;;;;;;;;;;;;;;;
; yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs 
      '("~/yasnippets"))

;; ; Different directory for chromebook
;; (if (equal system-name "localhost")
;;     (setq yas-snippet-dirs '("~/Usb/yasnippets"
;; 			     "~/yasnippets")))

(yas-global-mode 1)

; Stop yas completion in term/ansi-term and fix tab complete functionality
(add-hook 'term-mode-hook (lambda ()
                   (yas-minor-mode -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tramp
(require 'tramp)
(setq tramp-default-method "ssh")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Others
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


