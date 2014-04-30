;;================================================================
;; Org mode
 
(require 'org)
(require 'org-install)
(require 'org-html)

; (automatically starts files ending in .org in org mode)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; LaTeX
(require 'org-latex)
(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))

; See part three of this:
; http://orgmode.org/worg/org-tutorials/org-latex-export.html
(add-to-list 'org-export-latex-classes
             '("myarticle"
"\\documentclass[a4paper,20pt]{article}
\\usepackage{lmodern}
\\usepackage[margin=2.5cm]{geometry}
\\usepackage{setspace}
\\usepackage{graphicx}
\\onehalfspacing
"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


;; Show linked images, if this doesn't work automatically do:
; M-x org-display-inline-images RET
(if (equal (window-system) 'x)
    (org-display-inline-images t))


;;================
; Spell checking
; uncomment the region below if you want to start using spell checking
; to get this to work you will need to install aspell.
; Ubuntu:
; sudo apt-get install aspell
; Mac (install homebrew first):
; brew install aspell
; Windows:
; ...no idea sorry!
;;================

;; (setq ispell-program-name "aspell"
;;   ispell-extra-args '("--sug-mode=ultra"))
;; (setq flyspell-issue-message-flag nil)

;; ;; This automatically activates spell checking when in org-mode (I don't use it)
;; ;; just switch it on when necessary M-x flyspell-mode
;; (add-hook 'org-mode-hook
;;   (lambda()
;;     (flyspell-mode 1)))

;;================================================================
;; General usage
(setq visible-bell t) 
(transient-mark-mode 1)

; ido - best thing ever, it's what you see when you do C-x C-f
(ido-mode t)
(setq ido-enable-flex-matching t)

; dired
(put 'dired-find-alternate-file 'disabled nil)

; locate
(require 'locate)

;; Put backup files in a central place (stops <fn>~ files in current dir)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))

;;================
;; Setting the font - uncomment and put the correct name for your system where it says lettie
;; (if (equal system-name "lettie")
;;     (set-frame-font "Ubuntu Mono-11" nil t))

;;================
;; eshell setup (emacs shell - terminal replacement, works on windows properly as well)
;; Use bash type completions
(setq eshell-cmpl-cycle-completions nil)
;; scroll to the bottom
(setq eshell-scroll-to-bottom-on-output t)

;;;;;;;;;;;;;;;;;;;;
; revive.el - saves and resumes your window layout
(autoload 'save-current-configuration "revive" "Save status" t)
(autoload 'resume "revive" "Resume Emacs" t)
(autoload 'wipe "revive" "Wipe Emacs" t)
; Keyboard shortcuts
(define-key ctl-x-map "S" 'save-current-configuration)
(define-key ctl-x-map "F" 'resume)
(define-key ctl-x-map "K" 'wipe)

;; Uncomment below for auto start
;; (if (file-exists-p "~/.revive.el")
;;   (resume))


;;================================================================
;; Extra keyboard shortcuts

; moving between windows by holding control and pressing an arrow key
(windmove-default-keybindings 'control)
(setq windmove-wrap-around t)

; Set Control+H to backspace (same as in linux terminal)
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key (kbd "C-?") 'help-command)

; eshell!!! (emacs shell - works like a real terminal, even in windows!)
(global-set-key [f3] 'eshell)
; Standard terminal
(global-set-key [f4] 'ansi-term)

; Refresh buffer (when the file changes because of an external program)
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive) (revert-buffer t t))
(global-set-key [f5] 'revert-buffer-no-confirm)

; On Mac use cmd key as meta (as in M-x)
(when (string= system-type "darwin")
  (setq x-super-keysym 'meta))

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

;;================================================================
;; Packages

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
; Function for checking if a package needs to be installed (and subsequently installing it)
(defun install-if-needed (package)
  (unless (package-installed-p package)
    (package-install package)))

; These are the packages which need to be installed
(setq to-install
      '(python-mode cl-lib yasnippet auto-complete autopair find-file-in-repository exec-path-from-shell))

(mapc 'install-if-needed to-install)

;;;;;;;;;;;;;;;;
; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

; Stop yas completion in term/ansi-term and fix tab complete functionality
(add-hook 'term-mode-hook (lambda ()
                   (yas-minor-mode -1)))

; autopair - automatically adds closing brackets
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

