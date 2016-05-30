;;; package --- Main init file
;;; Commentary:
;;; This is my init file

;;; Code:

(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))


(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

(load-user-file "packages.el")
(load-user-file "emacs.el")
(load-user-file "functions.el")
(load-user-file "python.el")
(load-user-file "go.el")
(load-user-file "web.el")
(load-user-file "shell.el")
(load-user-file "keys.el")
(load-user-file "org.el")

(provide 'init)
;;; init.el ends here
