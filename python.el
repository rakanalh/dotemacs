;;; package --- python configs
;;; Commentary:
;;; Contains my python configs

;;; Code:

(use-package elpy
  :init
  (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
  :config
  (elpy-enable)
  ;; (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
  (setq elpy-rpc-backend "jedi"
        ;;flycheck-python-flake8-executable "/usr/local/bin/flake8"
	ac-modes (delq 'python-mode ac-modes))

  :bind (:map elpy-mode-map
              ("<M-left>" . nil)
              ("<M-right>" . nil)
              ("<M-S-left>" . elpy-nav-indent-shift-left)
              ("<M-S-right>" . elpy-nav-indent-shift-right)
              ("M-," . pop-tag-mark)
              ("C-c C-s" . nil)))

(use-package pip-requirements
  :config
  (add-hook 'pip-requirements-mode-hook #'pip-requirements-auto-complete-setup))

(use-package py-autopep8)

(use-package pyenv-mode
  :init
  (add-to-list 'exec-path "~/.pyenv/shims")
  (setenv "WORKON_HOME" "~/.pyenv/versions/")
  :config
  (pyenv-mode)
  :bind
  ("C-x p" . pyenv-activate-current-project))

(defun pyenv-init()
  (setq global-pyenv (replace-regexp-in-string "\n" "" (shell-command-to-string "pyenv global")))
  (message (concat "Setting pyenv version to " global-pyenv))
  (pyenv-mode-set global-pyenv)
  (defvar pyenv-current-version nil global-pyenv))

(defun pyenv-activate-current-project ()
  "Automatically activates pyenv version if .python-version file exists."
  (interactive)
  (f-traverse-upwards
   (lambda (path)
     (message path)
     (let ((pyenv-version-path (f-expand ".python-version" path)))
       (if (f-exists? pyenv-version-path)
          (progn
            (setq pyenv-current-version (s-trim (f-read-text pyenv-version-path 'utf-8)))
            (pyenv-mode-set pyenv-current-version)
            (pyvenv-workon pyenv-current-version)
            (message (concat "Setting virtualenv to " pyenv-current-version))))))))

(add-hook 'after-init-hook 'pyenv-init)
(add-hook 'find-file-hook 'pyenv-activate-current-project)
(add-hook 'projectile-switch-project-hook 'pyenv-activate-current-project)
(add-hook 'projectile-find-file-hook 'pyenv-activate-current-project)
(add-hook 'projectile-find-dir-hook 'pyenv-activate-current-project)

;;; python.el ends here
