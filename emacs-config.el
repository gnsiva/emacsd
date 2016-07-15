
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
