;;; package --- python configs
;;; Commentary:
;;; Contains my python configs

;;; Code:

(require 'elpy)
(require 'py-autopep8)
(require 'pyenv-mode)

(add-to-list 'exec-path "~/.pyenv/shims")
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))

(setenv "WORKON_HOME" "~/.pyenv/versions/")

(elpy-enable)
(pyenv-mode)

(setq elpy-rpc-backend "jedi")

;;(setq flycheck-python-flake8-executable "/usr/local/bin/flake8")
(setq ac-modes (delq 'python-mode ac-modes))

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
;; (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
(add-hook 'pip-requirements-mode-hook #'pip-requirements-auto-complete-setup)

;(defadvice auto-complete-mode (around disable-auto-complete-for-python)
;  (unless (eq major-mode 'python-mode) ad-do-it))
;(ad-activate 'auto-complete-mode)

;;; python.el ends here
