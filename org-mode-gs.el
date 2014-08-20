(require 'org)
(require 'org-install)
(require 'org-html)

;; ================================================================
;; Running code inside org mode

(require 'ob-tangle)

;; Programming languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (js . t)
   (C . t)
   (org . t)
   (java . t)
   (sh . y)))

;; Stop org-mode asking for confirmation when executing python code block
(defun my-org-confirm-babel-evaluate (lang body)
  (not (string= lang "python"))) ; don't ask for python
  ;; (not (string= lang "C")))  

(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)


;; ================================================================
;; Org export
; You might need to install pygments in os x for this to work (easy_install has it)
; pip has it too

;; LaTeX
(require 'org-latex)
(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))

(setq org-export-latex-listings 'minted)
(add-to-list 'org-export-latex-packages-alist '("" "minted"))
; syntax colouring for html at least (and in the buffer)
(setq org-src-fontify-natively t)

; Don't execute all the code blocks when exporting the document
(setq org-export-babel-evaluate nil)

; Put table captions below the table rather than above
(setq org-export-latex-table-caption-above nil)


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
\\usepackage{etoolbox}
\\AtBeginEnvironment{minted}{\\singlespacing \\fontsize{8}{8}\\selectfont}
\\usepackage[hidelinks]{hyperref}
\\bibliographystyle{unsrt}
"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
 
(add-to-list 'org-export-latex-classes
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


(add-to-list 'org-export-latex-classes
             '("test"
"\\documentclass[a4paper,20pt, twoside]{article}
\\usepackage{lmodern}
\\usepackage[lmargin=4cm,rmargin=2cm,tmargin=2cm,bmargin=2cm]{geometry}
\\usepackage{setspace}
\\usepackage{graphicx}
\\onehalfspacing
\\usepackage[font=singlespacing,font=footnotesize,width=.75\\textwidth]{caption}
\\usepackage{etoolbox}
\\AtBeginEnvironment{minted}{\\singlespacing \\fontsize{8}{8}\\selectfont}
\\usepackage[hidelinks]{hyperref}
\\bibliographystyle{unsrt}
\\usepackage[superscript,biblabel]{cite}
\\usepackage{cancel}
"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


(require 'org)
  (require 'org-latex)
  ;; (require 'ox-latex)
  (setq org-latex-packages-alist 
        (quote (("" "color" t) ("" "minted" t) ("" "parskip" t))))

(setq org-latex-to-pdf-process '("pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
				 "bibtex $(basename %b)"
				 "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
				 "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"))

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
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)
(setq org-startup-indented nil)

;; ================================================================
;; General stuff
; (automatically starts files ending in .org in org mode)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

; display images inline
(if (equal (window-system) 'x)
    (org-display-inline-images t))


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
