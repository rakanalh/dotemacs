;;; package --- python configs
;;; Commentary:
;;; Contains my python configs

;;; Code:

(require 'elpy)
(require 'py-autopep8)
(require 'pyenv-mode)

(elpy-enable)
(pyenv-mode)

(add-to-list 'exec-path "~/.pyenv/shims")
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))

(setq elpy-rpc-backend "jedi")

;;(setq flycheck-python-flake8-executable "/usr/local/bin/flake8")
(setq ac-modes (delq 'python-mode ac-modes))

(defun pyenv-init()
  (setq global-pyenv (replace-regexp-in-string "\n" "" (shell-command-to-string "pyenv global")))
  (message (concat "Setting pyenv version to " global-pyenv))
  (pyenv-mode-set global-pyenv)
  (defvar pyenv-current-version nil global-pyenv))

(defun pyenv-hook ()
"Automatically activates pyenv version if .python-version file exists."
(f-traverse-upwards
 (lambda (path)
    (message path)
    (let ((pyenv-version-path (f-expand ".python-version" path)))
      (if (f-exists? pyenv-version-path)
          (progn
            (message (concat "Found .python-version in path " pyenv-version-path))
            (setq pyenv-current-version (s-trim (f-read-text pyenv-version-path 'utf-8)))
            (pyenv-mode-set pyenv-current-version)
            (setenv "WORKON_HOME" (concat "~/.pyenv/versions/" pyenv-current-version "/envs"))
            (message (concat "Setting virtualenv path to ~/.pyenv/versions/" pyenv-current-version "/envs"))))))))

(add-hook 'after-init-hook 'pyenv-init)
(add-hook 'find-file-hook 'pyenv-hook)
(add-hook 'projectile-switch-project-hook 'pyenv-hook)
(add-hook 'projectile-find-file-hook 'pyenv-hook)
(add-hook 'projectile-find-dir-hook 'pyenv-hook)
;; (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
(add-hook 'pip-requirements-mode-hook #'pip-requirements-auto-complete-setup)

;(defadvice auto-complete-mode (around disable-auto-complete-for-python)
;  (unless (eq major-mode 'python-mode) ad-do-it))
;(ad-activate 'auto-complete-mode)

;;; python.el ends here
