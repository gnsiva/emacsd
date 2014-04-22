;;;;;;;;;;;;;;;;
;; Fixing PATH and PYTHONPATH ISSUES
;; See here
;; https://github.com/purcell/exec-path-from-shell
;; http://stackoverflow.com/questions/6411121/how-to-make-emacs-to-use-my-bashrc-file
;; Works in ubuntu 13.04
(setq to-install
      '(exec-path-from-shell)) 

(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "PYTHONPATH")
(exec-path-from-shell-copy-env "PATH")
(exec-path-from-shell-copy-env "LD_LIBRARY_PATH")
;; (when (string= system-name "anake")
;;   (exec-path-from-shell-copy-env "LD_LIBRARY_PATH"))
