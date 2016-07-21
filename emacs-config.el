
(setq user-full-name "Ganesh N. Sivalingam"
      user-mail-address "g.n.sivalingam@gmail.com")

(setq custom-file (expand-file-name "customize-output.el" user-emacs-directory))
(load custom-file)

(when (string-equal system-type "darwin")
(defun open-dir-in-finder ()
    "Open a new Finder window to the path of the current buffer"
    (interactive)
    (start-process "mai-open-dir-process" nil "open" "."))
  (bind-key "C-c o f" 'open-dir-in-finder)

  (defun open-dir-in-iterm ()
    "Open the current directory of the buffer in iTerm."
    (interactive)
    (let* ((iterm-app-path "/Applications/iTerm.app")
           (iterm-brew-path "/opt/homebrew-cask/Caskroom/iterm2/1.0.0/iTerm.app")
           (iterm-path (if (file-directory-p iterm-app-path)
                           iterm-app-path
                         iterm-brew-path)))
      (start-process "mai-open-dir-process" nil "open" "-a" iterm-path ".")))
  (bind-key "C-c o t" 'open-dir-in-iterm))

;; Requisites: Emacs >= 24
(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
; (add-to-list 'package-archives
;            '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/"))

(package-refresh-contents)
(defun install-if-needed (package)
  (unless (package-installed-p package)
    (package-install package)))

;; make more packages available with the package installer
(setq to-install
      '(python-mode cl-lib yasnippet jedi auto-complete autopair find-file-in-repository exec-path-from-shell magit auctex htmlize org emacs-eclim gtags smartscan nxml git-commit))

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
;;                           "~/yasnippets")))

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

(require 'org)
(require 'org-install)
(require 'ox-html)
(require 'ob-tangle)
(require 'ox-latex)

(setq org-latex-packages-alist 
      (quote (("" "color" t) ("" "minted" t) ("" "parskip" t))))

(add-hook 'org-mode-hook 'turn-on-visual-line-mode)
(setq org-startup-indented nil)

; (automatically starts files ending in .org in org mode)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

; display images inline
(if (display-graphic-p)
    (org-display-inline-images t))


(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

; log time of completion for tasks
(setq org-log-done t)

;; Stop org-mode asking for confirmation when executing python code block
(defun my-org-confirm-babel-evaluate (lang body)
  (not (string= lang "python"))) ; don't ask for python
  ;; (not (string= lang "C")))  

(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (js . t)
   (C . t)
   (org . t)
   (java . t)
   (R . t)
   (sql . t)
   (sh . t)))

(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))

(setq org-export-latex-listings 'minted)
;; TODO (add-to-list 'org-export-latex-packages-alist '("" "minted"))

; syntax colouring for html at least (and in the buffer)
(setq org-src-fontify-natively t)

; Don't execute all the code blocks when exporting the document
(setq org-export-babel-evaluate nil)

; Put table captions below the table rather than above
(setq org-export-latex-table-caption-above nil)

; See part three of this:
; http://orgmode.org/worg/org-tutorials/org-latex-export.html
(add-to-list 'org-latex-classes
             '("myarticle"
"\\documentclass[a4paper,11pt]{article}
\\usepackage{lmodern}
\\usepackage[margin=3cm]{geometry}
\\usepackage{setspace}
\\usepackage{graphicx}
\\onehalfspacing
\\usepackage{etoolbox}
\\AtBeginEnvironment{minted}{\\singlespacing \\fontsize{11}{11}\\selectfont}
\\usepackage[hidelinks]{hyperref}
\\bibliographystyle{unsrt}
"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             '("acs"
"\\documentclass[journal=ancham,manuscript=article,layout=twocolumn]{achemso}
\\usepackage{lmodern}
\\usepackage{setspace}
\\usepackage{graphicx}
"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             '("corrections"
"
%\\documentclass[12pt]{article}
\\documentclass[14pt]{extarticle}
\\usepackage[top=3cm, bottom=3cm, left=3cm, right=3cm]{geometry}
% changes vertical space between paragraphs
\\usepackage{parskip}
\\setlength{\\parskip}{10pt}

% Get rid of red boxes around links
\\usepackage{hyperref}
\\hypersetup{
    colorlinks,%
    citecolor=black,%
    filecolor=black,%
    linkcolor=black,%
    urlcolor=black
}
"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; ================================================================
;; Actual thesis format
(add-to-list 'org-latex-classes
             '("thesis"
"\\documentclass[a4paper, twoside]{book}
\\usepackage[fontsize=13pt]{scrextend}
\\usepackage{lmodern}
\\usepackage[lmargin=4cm,rmargin=2cm,tmargin=3cm,bmargin=3.2cm]{geometry}
\\usepackage{graphicx}

\\usepackage{setspace}
\\onehalfspacing

%\\DeclareMathSizes{13}{13}{12}{8}
\\usepackage{xcolor}

% ================
% Header and Footer

% see this for more on fancyhdr (pg 6-7)
% http://texdoc.net/texmf-dist/doc/latex/fancyhdr/fancyhdr.pdf

% fonts for header and footer (pg 14)
\\newcommand{\\hdrFtrFont}{\\fontfamily{cmr}\\fontsize{14}{14}\\selectfont}

\\usepackage{fancyhdr}
\\pagestyle{fancy}
\\renewcommand{\\headrulewidth}{0.4pt}
\\fancyhf{}
\\fancyhead[RO]{\\hdrFtrFont \\nouppercase{\\rightmark}}
\\fancyhead[LE]{\\hdrFtrFont \\nouppercase{\\leftmark}}
\\fancyfoot[LE,RO]{\\hdrFtrFont \\thepage}

% changes vertical space between paragraphs
%\\setlength{\\parskip}{10pt} 

% footnotes - use symbols instead of numbers
\\renewcommand*{\\thefootnote}{\\fnsymbol{footnote}}

% overcoming org mode fail where it wouldn't let me put in a web link properly
\\newcommand{\\clemmerdb}{http://www.indiana.edu/$\\sim$clemmer/Research/Cross\\%20Section\\%20Database/cs\\_database.php}
\\newcommand{\\bushdb}{http://depts.washington.edu/bushlab/ccsdatabase/}
\\newcommand{\\mobcal}{http://www.indiana.edu/$\\sim$nano/software.html}
\\newcommand{\\impact}{http://impact.chem.ox.ac.uk/}
\\newcommand{\\pdblink}{http://www.rcsb.org/pdb/}
\\newcommand{\\maldiFigureLink}{http://www.chm.bris.ac.uk/ms/maldi-ionisation.xhtml}
\\newcommand{\\sigmasoftware}{http://bowers.chem.ucsb.edu/theory\\_analysis/cross-sections/sigma.shtml}
\\newcommand{\\thalassinoslab}{http://www.homepages.ucl.ac.uk/$\\sim$ucbtkth/resources.html}
%\\newcommand{\\}{}
%\\newcommand{\\}{}

% ================================================================
% bibliography
% make bibliography a numbered section in the contents
% \\usepackage[nottoc,notlot,notlof]{tocbibind} % turned it into a chapter, so no good
% change name of bibliography sections to references
\\renewcommand{\\bibname}{References}

% ================
% bibtex per chapter bibliography 
% http://tex.stackexchange.com/questions/87414/per-chapter-bibliographies-in-biblatex

\\usepackage[citestyle=numeric-comp,bibstyle=authoryear,sorting=none,maxbibnames=99,backend=bibtex,refsection=chapter,doi=false,isbn=false,url=false,firstinits=true]{biblatex}
\\AtEveryBibitem{\\clearfield{month}}
\\AtEveryBibitem{\\clearfield{day}}
\\AtEveryBibitem{\\clearfield{series}}
\\AtEveryBibitem{\\clearlist{language}}
\\renewbibmacro{in:}{}
\\renewcommand*{\\mkbibnamefirst}[1]{{\\let~\\,#1}}
\\setlength\\bibitemsep{2\\itemsep}

\\DeclareFieldFormat{bibentrysetcount}{\\mkbibparens{\\mknumalph{#1}}}
\\DeclareFieldFormat{labelnumberwidth}{\\mkbibbrackets{#1}}

\\defbibenvironment{bibliography}
  {\\list
     {\\printtext[labelnumberwidth]{%
    \\printfield{prefixnumber}%
    \\printfield{labelnumber}}}
     {\\setlength{\\labelwidth}{\\labelnumberwidth}%
      \\setlength{\\leftmargin}{\\labelwidth}%
      \\setlength{\\labelsep}{\\biblabelsep}%
      \\addtolength{\\leftmargin}{\\labelsep}%
      \\setlength{\\itemsep}{\\bibitemsep}%
      \\setlength{\\parsep}{\\bibparsep}}%
      \\renewcommand*{\\makelabel}[1]{\\hss##1}}
  {\\endlist}
  {\\item}

\\DeclareNameAlias{sortname}{last-first}

%\\addbibresource{introduction.bib}
%\\addbibresource{1408_a1at.bib}
%\\addbibresource{1306_amphi.bib}
\\addbibresource{1407_challenger.bib}
\\addbibresource{bib-thesis.bib}

% original
%\\bibliographystyle{unsrt} 
%\\usepackage[superscript,biblabel]{cite}

% ================================================================

\\usepackage[font=singlespacing,font=footnotesize,width=.75\\textwidth]{caption}
\\usepackage{etoolbox}
\\AtBeginEnvironment{minted}{\\singlespacing \\fontsize{8}{8}\\selectfont}
\\usepackage[hidelinks]{hyperref}
\\usepackage{cancel}
"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")))

;; ================ 
;; RefTex
;; Configure RefTeX for use with org-mode. At the end of your
;; org-mode file you need to insert your style and bib file:
;; \bibliographystyle{plain}
;; \bibliography{ProbePosition}
;; See http://www.mfasold.net/blog/2009/02/using-emacs-org-mode-to-draft-papers/
(defun org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name)
       (file-exists-p (buffer-file-name))
       (reftex-parse-all))
  (define-key org-mode-map (kbd "<f8>") 'reftex-citation))

(add-hook 'org-mode-hook 'org-mode-reftex-setup)

;; compiling pdfs
;; normal version
;; (setq org-latex-to-pdf-process '("pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
;;                               "bibtex $(basename %b)"
;;                               "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
;;                               "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"))

;; thesis version
;; pdflatex myfile.tex
;; bibtex myfile1-blx.aux  ;; you need one of these for each chapter
;; bibtex myfile2-blx.aux
;; bibtex myfile.aux
;; pdflatex myfile.tex
;; pdflatex myfile.tex
(setq org-latex-to-pdf-process '("pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
                                 "bibtex $(basename %b1-blx.aux)" ;; you need one of these for each chapter/bibliography
                                 "bibtex $(basename %b2-blx.aux)"
                                 "bibtex $(basename %b3-blx.aux)"
                                 "bibtex $(basename %b4-blx.aux)"
                                 "bibtex $(basename %b5-blx.aux)"
                                 "bibtex $(basename %b6-blx.aux)"
                                 "bibtex $(basename %b.aux)"
                                 "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
                                 "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"))

; Spell checking
(setq ispell-program-name "aspell"
  ispell-extra-args '("--sug-mode=ultra"))

;;;;;;;;;;;;;;;;
;; ;; automatically add spell check to org-mode files. Turned it off cos it was annoying
;; ;; just switch it on when necessary M-x flyspell-mode
;; (add-hook 'org-mode-hook
;;   (lambda()
;;     (flyspell-mode 1)))

(setq flyspell-issue-message-flag nil)

;; Key bindings

(setq x-super-keysym 'alt)

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

;; ; disable return and backspace
;; (global-set-key (kbd "<return>") (lambda () ))
;; (global-set-key (kbd "<backspace>") (lambda () ))

; search and replace
;; (global-set-key "\C-\M-s" 'search-forward-regexp)

(global-set-key [f1] 'compile)
(global-set-key [f2] 'next-error)
; eshell!!!
(global-set-key [f3] 'eshell)
; Standard terminal
(global-set-key [f4] 'ansi-term)

; Refresh buffer
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive) (revert-buffer t t))
(global-set-key [f5] 'revert-buffer-no-confirm)

(global-set-key [f6] 'athena)

; Macro quick button
(global-set-key [f7] 'kmacro-end-and-call-macro)

;; [f8] is used by reftex (org mode)

; On Mac use cmd key as meta (as in M-x)
(when (string= system-type "darwin")
  (setq x-super-keysym 'meta))


; org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
; TODO make these part of a hook, they only do stuff when in org-mode
(global-set-key "\M-M" 'org-insert-todo-heading)
(global-set-key "\M-p" 'org-up-element)
(global-set-key "\M-n" 'org-forward-element)



;;; Mac Os X stuff
;; mac cmd key as Meta (also fixes ubuntu to make meta key alt apparently)
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)
; separate tutorial [[http://mcclanahoochie.com/blog/2011/08/remapping-macbook-pro-keys-for-emacs/][separate tutorial]] 

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
(defalias 'esearch 'isearch-forward-regexp)
(defalias 'ff 'find-file)

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
  (insert (format "%s" system-name)))


;; Hopefully get the OS type
(defun insert-system-type()
  (interactive)
  "Get current system type"
  (insert (format "%s" system-type)))


;; Insert the path of the currently selected buffer
(defun insert-buffer-file-name()
  (interactive)
  "Get current system type"
  (insert (format "%s" (or (buffer-file-name) default-directory))))


;; Word count (only works on the entire buffer)
(defun wc () 
  (interactive) 
  (shell-command (concat "wc " buffer-file-name)))
;; (global-set-key "\C-cw" 'wc)


;; Doesn't currently work...
; The suggestion came from:
; http://emacs.1067599.n5.nabble.com/tramp-does-not-see-directory-changes-td242710.html
; It worked the first time I tried the solution manually, but no longer does..
; Pretty sure the function is doing what I want it to though
(defun ido-tramp-refresh-file-list()
  (interactive)
  "Update ido completion cache when using tramp-mode."
  (dired ".")
  (revert-buffer)
  (kill-this-buffer))


(defun athena()
  (interactive)
  "Ssh into athena server with ansi-term as 'athena' buffer. If buffer already exists, switch to it."
  (if (get-buffer "athena")
      (switch-to-buffer "athena")
      (progn
          (ansi-term "/bin/bash")
          (rename-buffer "athena")
          (term-send-invisible "ssh -XY athena"))))

; linux
;; (when (string= window-system "x")
;;   (invert-face 'default))
; mac version
;; (when (string= window-system "ns")
;;   (invert-face 'default))

(set-frame-font "Ubuntu Mono-11" nil t)

; Bev Macbook Pro
;(if (equal system-name "lettie")
;    (set-frame-font "Ubuntu Mono-11" nil t))

;; Ubuntu Mate Desktop
(if (equal system-name "anake")
    (set-frame-font "Ubuntu Mono-11" nil t))

;; run the aliases etc from .bashrc for M-x compile and shell-command
(setq shell-file-name "bash")
(setq shell-command-switch "-ic")

;; Run locate from within Emacs !!!!
(use-package locate)


;; Trying to make eshell work as expected
;; Use bash type completions
(setq eshell-cmpl-cycle-completions nil)
;; Scroll to the bottom
(setq eshell-scroll-to-bottom-on-output t)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; These functions are useful. Activate them.
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Put backup files in a central place (stops <fn>~ files in current dir)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; Stop audible bell, have flashing mode line instead
(setq visible-bell nil)
(setq ring-bell-function (lambda ()
   (invert-face 'mode-line)
   (run-with-timer 0.1 nil 'invert-face 'mode-line)))
(tool-bar-mode -1)
(menu-bar-mode -1)
(transient-mark-mode 1)

;; delete the region when typing, just like as we expect nowadays.
(delete-selection-mode t)

;; Always show matching parenthesis
(show-paren-mode t)

;; Put the column number next to the line number in the mode line
(column-number-mode t)

;; Turn off the blinking cursor
(blink-cursor-mode -1)

;; enable word wrap on all buffers
(global-visual-line-mode)
(diminish 'visual-line-mode)

; had to move this to init.el because it is overwritten
(set-cursor-color "Royal Blue")

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

(ido-mode t)
(setq ido-everywhere 1)
(setq ido-enable-flex-matching t)

; not sure what this does, if problems, turn off
(setq ido-create-new-buffer 'always)

;;;;;;;;;;;;;;;;
;; Fixing PATH and PYTHONPATH ISSUES
;; See here
;; https://github.com/purcell/exec-path-from-shell
;; http://stackoverflow.com/questions/6411121/how-to-make-emacs-to-use-my-bashrc-file
;; Works in ubuntu 13.04 (update: also works on 12.04, 13.10 and 14.04)
(setq to-install
      '(exec-path-from-shell)) 

(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "PYTHONPATH")
(exec-path-from-shell-copy-env "PATH")
(exec-path-from-shell-copy-env "LD_LIBRARY_PATH")

;; Setup
; sudo apt-get install pylint python-virtualenv
; sudo pip install jedi
; sudo pip install epc
; sudo pip install virtualenv

;; There is stuff needed from the packages-gs.el file

;; ;; Python mode settings
(require 'python)

(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))

(setq py-electric-colon-active t)

;(require 'epc)
(require 'jedi)

;; Have to use system-type as for some reason it still uses the ubuntu name in mac on the mba
(if (equal system-type "darwin")
    (setq jedi:server-command
          '("/Library/Frameworks/Python.framework/Versions/Current/bin/python" "/Users/ganesh/.emacs.d/elpa/jedi-20130714.1415/jediepcserver.py")))


(add-hook 'python-hook
          (lambda ()
            (jedi:setup)
            (jedi:ac-setup)
            (local-set-key (kbd "M-?") 'jedi:show-doc)
            (local-set-key (kbd "A-SPC") 'jedi:complete)
            (local-set-key (kbd "M-.") 'jedi:goto-definition)
            (local-set-key (kbd "M-,") 'jedi:goto-definition-pop-marker)
            (local-set-key (kbd "M-/") 'jedi:get-in-function-call)))


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

(add-hook 'python-hook 'flymake-activate)
(add-hook 'python-hook 'auto-complete-mode)
(add-hook 'python-hook 'autopair-mode)
(add-hook 'python-hook 'yas-minor-mode)


;; ================================================================
;; All the stuff you commented out to switch to python.el from python-mode.el
;; the reason was that htmlize.el didn't work with python-mode.el

;; TODO - uncomment this once you get python.el working properly

;; (require 'python-mode)

;; (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))

;; (add-hook 'python-mode-hook
;;        (lambda ()
;;          (jedi:setup)
;;          (jedi:ac-setup)
;;             (local-set-key (kbd "M-?") 'jedi:show-doc)
;;             (local-set-key (kbd "A-SPC") 'jedi:complete)
;;             (local-set-key (kbd "M-.") 'jedi:goto-definition)
;;             (local-set-key (kbd "M-,") 'jedi:goto-definition-pop-marker)
;;             (local-set-key (kbd "M-/") 'jedi:get-in-function-call)))


;; ;; I couldn't work out how to hack this to work with python.el (not that I really tried)
;; (add-to-list 'flymake-allowed-file-name-masks
;;              '("\\.py\\'" flymake-python-init))

;; (add-hook 'python-mode-hook 'flymake-activate)
;; (add-hook 'python-mode-hook 'auto-complete-mode)
;; (add-hook 'python-mode-hook 'autopair-mode)
;; (add-hook 'python-mode-hook 'yas-minor-mode)

;; -*- emacs-lisp -*-
(defun tkj-insert-serial-version-uuid()
  (interactive)
  (insert "private static final long serialVersionUID = 1L;"))

(defun tkj-eclim-maven-run-quick-package()
  (interactive)
  (eclim-maven-run "-o -q -DskipTests package"))

(defun my-c-mode-hook ()
  (auto-fill-mode)
  (gtags-mode)
  (flyspell-prog-mode)
  (flymake-mode)
  (subword-mode)
  (smartscan-mode)

  (define-key c-mode-base-map "\C-\M-j" 'tkj-insert-serial-version-uuid)
  (define-key c-mode-base-map "\C-m" 'c-context-line-break)
  (define-key c-mode-base-map (kbd "<f2>") 'eclim-problems)

  ;; Setting up a number of Java related shortcuts to mimic IDEA.
  (define-key c-mode-base-map "\C-\M-g" 'eclim-java-find-declaration)
  (define-key c-mode-base-map "\C-\M-o" 'eclim-java-import-organize)
  (define-key c-mode-base-map "\C-q" 'eclim-java-show-documentation-for-current-element)
  (define-key c-mode-base-map "\M-i" 'eclim-java-implement) ;; IDEA is C-i
  (define-key c-mode-base-map (kbd "<M-RET>") 'eclim-problems-correct)
  (define-key c-mode-base-map (kbd "<M-f7>") 'eclim-java-find-references)
  (define-key c-mode-base-map (kbd "<S-f6>") 'eclim-java-refactor-rename-symbol-at-point)
  (define-key c-mode-base-map (kbd "<S-f7>") 'gtags-find-tag-from-here)
  (define-key c-mode-base-map (kbd "<C-f9>") 'tkj-eclim-maven-run-quick-package)

  ;; Fix indentation for anonymous classes
  (c-set-offset 'substatement-open 0)
  (if (assoc 'inexpr-class c-offsets-alist)
      (c-set-offset 'inexpr-class 0))

  ;; Indent arguments on the next line as indented body.
  (c-set-offset 'arglist-intro '+))
(add-hook 'c-mode-common-hook 'my-c-mode-hook)

(defun tkj-default-code-style-hook()
  (setq c-basic-offset 2
        c-label-offset 0
        indent-tabs-mode nil
        compile-command "cd ~/src/drifting/jms && mvn -q -o -DskipTests package"
        require-final-newline nil))

(add-hook 'c-mode-hook 'tkj-default-code-style-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flymake settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'flymake)
(setq flymake-log-level -1) ;; 3 is debug

;; On the fly checkstyle & pmd checking
(defun my-flymake-init ()
  (list "my-java-flymake-checks"
        (list (flymake-init-create-temp-buffer-copy
               'flymake-create-temp-with-folder-structure))))

(add-to-list 'flymake-allowed-file-name-masks
             '("\\.java$" my-flymake-init flymake-simple-cleanup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface to eclipse via eclim
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'eclim)
(global-eclim-mode)

;; Variables
(setq eclim-auto-save t
;;      eclim-executable "/opt/eclipse/eclim"
;;      eclimd-executable "/opt/eclipse/eclimd"
      eclimd-wait-for-process nil
      eclimd-default-workspace "~/workspace"
      eclim-use-yasnippet nil
      help-at-pt-display-when-idle t
      help-at-pt-timer-delay 0.1
      )

;; Call the help framework with the settings above & activate
;; eclim-mode
(help-at-pt-set-timer)

;; Hook eclim up with auto complete mode
(require 'auto-complete-config)
(ac-config-default)
(require 'ac-emacs-eclim-source)
(ac-emacs-eclim-config)

(find-file "~/.emacs.d/init.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (wombat)))
 '(org-agenda-files (quote ("~/repos/org-agenda/pp.org"
                            "~/repos/org-agenda/life.org")))
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
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(put 'dired-find-alternate-file 'disabled nil)
