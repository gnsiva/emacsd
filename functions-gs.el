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
