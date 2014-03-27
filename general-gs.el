(add-to-list 'load-path "~/.emacs.d/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Files to automatically load into a buffer

(find-file "~/Dropbox/musings/org_mode_notes/code_notes.org")
(find-file "~/Dropbox/musings/org_mode_notes/lab_book.org")
(find-file "~/.emacs.d/init.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General
(setq visible-bell t)
(tool-bar-mode -1)
(menu-bar-mode -1)

; ido
(ido-mode t)
(setq ido-enable-flex-matching t)

; dired
(put 'dired-find-alternate-file 'disabled nil)

; locate
(require 'locate)

;; Remember open buffers when reopening
(desktop-save-mode 1)

;; Put backup files in a central place (stops <fn>~ files in current dir)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))

(transient-mark-mode 1)

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
(when (string= system-name "eos")
  (setq x-super-keysym 'meta))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font stuff

;;;;;;;;;;;;;;;;
;; Aphie's ubuntu VM
(if (equal system-name "ubuntu1204")
    (set-frame-font "Ubuntu Mono-11" nil t))

;;;;;;;;;;;;;;;;
;; Ares's ubuntu VM (Lettie)
(if (equal system-name "lettie")
    (set-frame-font "Ubuntu Mono-12" nil t))


;;;;;;;;;;;;;;;;
;; Fixing PATH and PYTHONPATH ISSUES
;; See here
;; https://github.com/purcell/exec-path-from-shell
;; http://stackoverflow.com/questions/6411121/how-to-make-emacs-to-use-my-bashrc-file
;; Works in ubuntu 13.04
(setq to-install
      '(exec-path-from-shell)) 
; Comment out for offline 

(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "PYTHONPATH")
(exec-path-from-shell-copy-env "PATH")
(exec-path-from-shell-copy-env "LD_LIBRARY_PATH")
;; (when (string= system-name "anake")
;;   (exec-path-from-shell-copy-env "LD_LIBRARY_PATH"))



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
; revive.el 
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

































