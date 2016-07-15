;;(add-to-list 'load-path "~/.emacs.d/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Files to automatically load into a buffer

;; ;; Commented out cos files are in a different place now
;; (find-file "~/Dropbox/musings/org_mode_notes/code_notes.org")
;; (find-file "~/Dropbox/musings/org_mode_notes/lab_book.org")
;; (find-file "~/.emacs.d/init.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General
;;(setq visible-bell t)
(setq visible-bell nil)
(setq ring-bell-function (lambda ()
   (invert-face 'mode-line)
   (run-with-timer 0.1 nil 'invert-face 'mode-line)))

(tool-bar-mode -1)
(menu-bar-mode -1)
(transient-mark-mode 1)

;; answer warnings with just y or n, rather than typing out yes/no
(fset 'yes-or-no-p 'y-or-n-p)

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
(set-cursor-color "Royal Blue") ; had to move this to init.el because it is overwritten
; linux
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
(set-frame-font "Ubuntu Mono-11" nil t)

;;;;;;;;;;;;;;;;
;; Aphie's ubuntu VM
(if (equal system-name "ubuntu1204")
    (set-frame-font "Ubuntu Mono-11" nil t))

;;;;;;;;;;;;;;;;
;; Ares's ubuntu VM (Lettie)
(if (equal system-name "localhost")
    (set-frame-font "Ubuntu Mono-10" nil t))

; Lettie (macbook air)
(if (equal system-name "lettie")
    (set-frame-font "Ubuntu Mono-11" nil t))

;; (if (equal system-name "anake")
;;     (set-frame-font "Ubuntu Mono-11" nil t))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UNIX

;; Word count (only works on the entire buffer)
;; (defun wc () (interactive) (shell-command (concat "wc " buffer-file-name)))
;; (global-set-key "\C-cw" 'wc)
