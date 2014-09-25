
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
;; 	  (lambda ()
;; 	    (jedi:setup)
;; 	    (jedi:ac-setup)
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


