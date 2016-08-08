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

