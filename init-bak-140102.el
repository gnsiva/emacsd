;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq x-super-keysym 'alt)
(add-to-list 'load-path "~/.emacs.d/")

; Changing arrow key functionality to scrolling
(global-set-key [up] (lambda () (interactive) (scroll-down 10)))
(global-set-key [down] (lambda () (interactive) (scroll-up 10)))
(global-set-key [left] (lambda () (interactive) (scroll-right tab-width t)))
(global-set-key [right] (lambda () (interactive) (scroll-left tab-width t)))

; moving between windows
(windmove-default-keybindings 'control)
(setq windmove-wrap-around t)

; backwards delete
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key (kbd "C-?") 'help-command)

; Comment and uncomment regions
(global-set-key (kbd "C-.") 'comment-region)
(global-set-key (kbd "C-,") 'uncomment-region)


; Toolbars etc
;(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

; disable return and backspace
;(global-set-key (kbd "<return>") (lambda () ))
;(global-set-key (kbd "<backspace>") (lambda () ))

; search and replace
(global-set-key (kbd "C-M-s") 'replace-string) ; doesn't work ubuntu on mba

; eshell!!!
(global-set-key [f3] 'eshell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Files to automatically load into a buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(find-file "~/Dropbox/musings/org_mode_notes/code_notes.org")
(find-file "~/Dropbox/musings/org_mode_notes/lab_book.org")
(find-file "~/.emacs.d/init.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Display options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invert colours in windowed
; linux
(set-cursor-color "Royal Blue")
;; (when (string= window-system "x")
;;   (invert-face 'default))
; mac version
(when (string= window-system "ns")
  (invert-face 'default))

; super key in ubuntu on the mac 
(when (string= system-name "eos")
  (setq x-super-keysym 'meta))

(setq visible-bell t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ido
(ido-mode t)
(setq ido-enable-flex-matching t)

; dired
(put 'dired-find-alternate-file 'disabled nil)

; locate
(require 'locate)

;; Remember open buffers when reopening
(desktop-save-mode 1)

;; Put backup files in a central place
;; Stops your directories filling up with <fn>~ files
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))

;; taken from: http://www.emacswiki.org/emacs/SwitchingBuffers
;; problem is it reverts to ones which are still open and displayed
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org-mode settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org)
(require 'org-install)
(require 'org-html)

;; LaTeX
(require 'org-latex)
(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))
(add-to-list 'org-export-latex-classes
             '("article"
               "\\documentclass{article}"
               ("\\section{%s}" . "\\section*{%s}")))

;; Programming languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))
   ;; (python . t)
   ;; (C . t)
   ;; (js . t)))

; Don't show line numbers (lab_book.org is too big)
(add-hook 'org-mode-hook 
	  (global-linum-mode 0))

; (automatically starts files ending in .org in org mode)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

(global-font-lock-mode 1)
; Disable the splash screen (to enable it agin, replace the t with 0)
;(setq org-log-done t) ; think this puts a time stamp when you complete a TODO item
(setq inhibit-splash-screen t)
(setq org-agenda-files '("~/Dropbox/musings/org_mode_notes"))
;(setq org-agenda-tex-search-extra-files 
;      (append (find-org-file-recursively "~/Dropbox/musings/org_mode_notes" "org")))
; Enable transient mark mode
(transient-mark-mode 1)

;(global-set-key "\C-cl" 'org-store-link)

; remap suggest tag
;(global-set-key (kbd "<f12>") 'complete-tag)
;;;$Id: revive.el,v 2.19 2008/05/13 01:19:16 yuuji Exp $


(global-set-key "\M-M" 'org-insert-todo-heading)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mac Os X stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; mac cmd key as Meta (also fixes ubuntu to make meta key alt apparently)
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)
; separate tutorial [[http://mcclanahoochie.com/blog/2011/08/remapping-macbook-pro-keys-for-emacs/][separate tutorial]] 


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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Download from
;git clone https://github.com/AndreaCrotti/minimal-emacs-configuration
;; Youtube video
; http://www.youtube.com/watch?v=0cZ7szFuz18
;; IMPORTANT
; You also need to install pylint (apt-get)
; also virtualenv (apt-get python-virtualenv or pip install virtualenv) ; not sure if i need this one
;; It's also required to run "pip install --user jedi" and "pip
;; install --user epc" to get the Python side of the library work
;; correctly.;; Shortcuts
;


;; Requisites: Emacs >= 24
(require 'package)
(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
	     '("gnu" . "http://elpa.gnu.org/packages/"))

; Comment out for offline
(package-refresh-contents)
(defun install-if-needed (package)
  (unless (package-installed-p package)
    (package-install package)))

;; make more packages available with the package installer
(setq to-install
      '(python-mode cl-lib yasnippet jedi auto-complete autopair find-file-in-repository))

; Comment out for offline
(mapc 'install-if-needed to-install)

;; Needed for magit
;replace the line a few up
;; (setq to-install
;;       '(python-mode magit yasnippet jedi auto-complete autopair find-file-in-repository))

;; (require 'magit)
;; (global-set-key "\C-xg" 'magit-status)

(require 'autopair)
(require 'flymake)
(require 'yasnippet)

; I added this line cos yas wasnt starting automatically
; and had to move the require auto-complete from above
(yas-global-mode 1)

(global-set-key [f7] 'find-file-in-repository)


(require 'auto-complete)

; auto-complete mode extra settings
(setq
 ac-auto-start 2
 ac-override-local-map nil
 ac-use-menu-map t
 ac-candidate-limit 20)

;; ;; Python mode settings
(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(setq py-electric-colon-active t)
(add-hook 'python-mode-hook 'autopair-mode)
(add-hook 'python-mode-hook 'yas-minor-mode)

(defun gish-set-ipython-shortcuts ()
  (local-set-key (kbd "C-c C-c") 'py-execute-buffer-ipython)
  (local-set-key (kbd "C-c |") 'py-execute-region-ipython)  
)
(add-hook 'python-mode-hook 'gish-set-ipython-shortcuts)

;; ;; Jedi settings
;(require 'epc)
(require 'jedi)
;; It's also required to run "pip install --user jedi" and "pip
;; install --user epc" to get the Python side of the library work
;; correctly.
;; With the same interpreter you're using.

;; if you need to change your python intepreter, if you want to change it
;; (setq jedi:server-command
;;       '("python2" "/home/andrea/.emacs.d/elpa/jedi-0.1.2/jediepcserver.py"))



;; (if (equal system-name "eos")
;;     (setq jedi:server-command
;; 	  '("/usr/bin/python" "/home/gns/.emacs.d/elpa/jedi-20130714.1415/jediepcserver.py")))

;; Have to use system-type as for some reason it still uses the ubuntu name in mac on the mba
(if (equal system-type "darwin")
    (setq jedi:server-command
	  '("/Library/Frameworks/Python.framework/Versions/Current/bin/python" "/Users/ganesh/.emacs.d/elpa/jedi-20130714.1415/jediepcserver.py")))

;; (if (equal system-name "anake")
;;     (setq jedi:server-command
;; 	  '("/usr/bin/python" "/home/ganesh/.emacs.d/elpa/jedi-20130714.1415/jediepcserver.py")))
(if (equal system-name "nyx")
    (setq jedi:server-command
	  '("/usr/bin/python" "/home/ganesh/.emacs.d/elpa/jedi-20130714.1415/jediepcserver.py")))

;; (setq jedi:server-command
;;       '("/usr/bin/python" "/home/gns/.emacs.d/elpa/jedi-20130714.1415/jediepcserver.py"))
;(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook
	  (lambda ()
	    (jedi:setup)
	    (jedi:ac-setup)
            (local-set-key "\C-cd" 'jedi:show-doc)
            (local-set-key (kbd "A-SPC") 'jedi:complete)
            (local-set-key (kbd "M-.") 'jedi:goto-definition)))

;; Flymake settings for Python
(defun flymake-python-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    ;(list "epylint" (list local-file))
))

(defun flymake-activate ()
  "Activates flymake when real buffer and you have write access"
  (if (and
       (buffer-file-name)
       (file-writable-p buffer-file-name))
      (progn
        (flymake-mode t)
        ;; this is necessary since there is no flymake-mode-hook...
        (local-set-key (kbd "C-c n") 'flymake-goto-next-error)
        (local-set-key (kbd "C-c p") 'flymake-goto-prev-error))))

(defun ca-flymake-show-help ()
  (when (get-char-property (point) 'flymake-overlay)
    (let ((help (get-char-property (point) 'help-echo)))
      (if help (message "%s" help)))))

(add-hook 'post-command-hook 'ca-flymake-show-help)

(add-to-list 'flymake-allowed-file-name-masks
             '("\\.py\\'" flymake-python-init))

(add-hook 'python-mode-hook 'flymake-activate)
(add-hook 'python-mode-hook 'auto-complete-mode)

(ido-mode t)
;; -------------------- extra nice things --------------------
;; use shift to move around windows
;(windmove-default-keybindings 'shift)
(show-paren-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Suggestions from a youtube clip
;; http://www.youtube.com/watch?v=EHvQG7dbk_8

;;; Emacs Server
; Don't close the file (buffer) when C-x # is t¯yped
(setq server-kill-new-buffers nil)

; Start the emacs server for emacsclient
(server-start)

;;; Shortcuts
(global-set-key "\C-x\C-g" 'goto-line)
(global-set-key [f1] 'compile)
(global-set-key [f2] 'next-error)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My functions

; Regexing for def
(setq exp "def ")
(defun python-functions ()
  (interactive)
  (goto-char 1)
  (occur exp))
(global-set-key "\M-+" 'python-functions)

; Insert (a proper) tab's worth of spaces
;(defun add-four-spaces ()
;; Use C-> and C-< when you finally work out how to get this to work
  

;; Get the current computer name
(defun insert-system-name()
(interactive)
"Get current system's name"
(insert (format "%s" system-name))
)

;; Hopefully get the OS type
(defun insert-system-type()
(interactive)
"Get current system type"
(insert (format "%s" system-type))
)

;; Insert the path of the currently selected buffer
(defun insert-buffer-file-name()
(interactive)
"Get current system type"
(insert (format "%s" (or (buffer-file-name) default-directory)))
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My Aliases
(defalias 'eb 'eval-buffer)
(defalias 'er 'eval-region)
(defalias 'rb 'revert-buffer)
(defalias 'rep 'replace-string) 
(defalias 'qrep 'query-replace) 
(defalias 'erep 'replace-regexp)
(defalias 'qerep 'query-replace-regexp)
(defalias 'ff 'find-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C programming
; From:
; http://truongtx.me/2013/03/10/emacs-setting-up-perfect-environment-for-cc-programming/
(require 'autopair)
(autopair-global-mode 1)
(setq autopair-autowrap t)

;; Cuda

(autoload 'cuda-mode "~/.emacs.d/cuda-mode.el" "Cuda-mode" t)
(add-to-list 'auto-mode-alist '("\\.cu$" . cuda-mode))
(add-to-list 'auto-mode-alist '("\\.cuh$" . cuda-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eshell
;; Use bash type completions
(setq eshell-cmpl-cycle-completions nil)

;; scroll to the bottom
(setq eshell-scroll-to-bottom-on-output t)
(setq eshell-scroll-show-maximum-output t)
;; This line doesn't work
;(add-to-list ‘eshell-output-filter-functions ‘eshell-postoutput-scroll-to-bottom)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BREAKABLES

;;;;;;;;;;;;;;;;
;; Fixing PATH and PYTHONPATH ISSUES
;; See here
;; https://github.com/purcell/exec-path-from-shell
;; http://stackoverflow.com/questions/6411121/how-to-make-emacs-to-use-my-bashrc-file
;; Works in ubuntu 13.04
(setq to-install
      '(exec-path-from-shell)) 
; Comment out for offline 
(mapc 'install-if-needed to-install)
(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "PYTHONPATH")
(exec-path-from-shell-copy-env "PATH")
(exec-path-from-shell-copy-env "LD_LIBRARY_PATH")


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

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(max-lisp-eval-depth 1000)
;;  '(max-specpdl-size 10000))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
