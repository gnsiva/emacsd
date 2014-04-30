(add-to-list 'load-path "~/.emacs.d/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Files to automatically load into a buffer

;; ;; Commented out cos files are in a different place now
;; (find-file "~/Dropbox/musings/org_mode_notes/code_notes.org")
;; (find-file "~/Dropbox/musings/org_mode_notes/lab_book.org")
;; (find-file "~/.emacs.d/init.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General
(setq visible-bell t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(transient-mark-mode 1)

; ido
(ido-mode t)
(setq ido-enable-flex-matching t)

; dired
(put 'dired-find-alternate-file 'disabled nil)

; locate
(require 'locate)

;; Put backup files in a central place (stops <fn>~ files in current dir)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invert colours in windowed
; linux
(set-cursor-color "Royal Blue")
;; (when (string= window-system "x")
;;   (invert-face 'default))
; mac version
;; (when (string= window-system "ns")
;;   (invert-face 'default))


; super key in ubuntu on the mac 
(when (string= system-name "lettie")
  (setq x-super-keysym 'meta))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font stuff

;;;;;;;;;;;;;;;;
;; Aphie's ubuntu VM
(if (equal system-name "ubuntu1204")
    (set-frame-font "Ubuntu Mono-11" nil t))

;;;;;;;;;;;;;;;;
;; Ares's ubuntu VM (Lettie)
(if (equal system-name "localhost")
    (set-frame-font "Ubuntu Mono-11" nil t))

; Lettie (macbook air)
(if (equal system-name "lettie")
    (set-frame-font "Ubuntu Mono-11" nil t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eshell
;; Use bash type completions
(setq eshell-cmpl-cycle-completions nil)

;; scroll to the bottom
(setq eshell-scroll-to-bottom-on-output t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C and Cuda programming

(autoload 'cuda-mode "~/.emacs.d/cuda-mode.el" "Cuda-mode" t)
(add-to-list 'auto-mode-alist '("\\.cu$" . cuda-mode))
(add-to-list 'auto-mode-alist '("\\.cuh$" . cuda-mode))


;;;;;;;;;;;;;;;;;;;;
; revive.el  - saves and resumes your window layout
; save open windows
(autoload 'save-current-configuration "revive" "Save status" t)
(autoload 'resume "revive" "Resume Emacs" t)
(autoload 'wipe "revive" "Wipe Emacs" t)

; Keyboard shortcuts
(define-key ctl-x-map "S" 'save-current-configuration)
(define-key ctl-x-map "F" 'resume)
(define-key ctl-x-map "K" 'wipe)

; Auto start
;; (if (file-exists-p "~/.revive.el")
;;   (resume))
