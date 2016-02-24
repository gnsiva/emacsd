(require 'org)
(require 'org-install)
(require 'ox-html)

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
   (R . t)
   (sql . t)
   (sh . t)))

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
(require 'ox-latex)
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
(add-to-list 'org-export-latex-classes
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
"\\documentclass[a4paper, twoside]{article}
\\usepackage[fontsize=13pt]{scrextend}
\\usepackage{lmodern}
\\usepackage[lmargin=4cm,rmargin=2cm,tmargin=2.5cm,bmargin=3.2cm]{geometry}
\\usepackage{graphicx}

\\usepackage{setspace}
\\onehalfspacing
%\\doublespacing

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

;; ================================================================
;; Actual thesis format
(add-to-list 'org-export-latex-classes
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


(add-to-list 'org-export-latex-classes
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


(require 'org)
  ;; (require 'ox-latex)
  (setq org-latex-packages-alist 
        (quote (("" "color" t) ("" "minted" t) ("" "parskip" t))))

;; normal version
;; (setq org-latex-to-pdf-process '("pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
;; 				 "bibtex $(basename %b)"
;; 				 "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
;; 				 "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"))

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
