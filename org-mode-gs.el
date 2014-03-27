(require 'org)
(require 'org-install)
(require 'org-html)

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
\\onehalfspacing
"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
 

; You might need to install pygments in os x for this to work (easy_install has it)
; pip has it too
(setq org-export-latex-listings 'minted)
(add-to-list 'org-export-latex-packages-alist '("" "minted"))


;; Programming languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (js . t)))
   ;; (python . t)
   ;; (C . t)
   ;; (js . t)))

; (automatically starts files ending in .org in org mode)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

; syntax colouring for html at least (and in the buffer)
(setq org-src-fontify-natively t)

; display images inline
(org-display-inline-images t)


; Spell checking
(setq ispell-program-name "aspell"
  ispell-extra-args '("--sug-mode=ultra"))

(add-hook 'org-mode-hook
  (lambda()
    (flyspell-mode 1)))

(setq flyspell-issue-message-flag nil)
