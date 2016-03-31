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

(defun ssbb-pyenv-hook ()
"Automatically activates pyenv version if .python-version file exists."
(f-traverse-upwards
 (lambda (path)
    (let ((pyenv-version-path (f-expand ".python-version" path)))
      (if (f-exists? pyenv-version-path)
          (progn
           (message (concat "Found .python-version in path " pyenv-version-path))
           (defvar pyenv-current-version (s-trim (f-read-text pyenv-version-path 'utf-8)))
           (pyenv-mode-set pyenv-current-version)
           (setenv "WORKON_HOME" (concat "~/.pyenv/versions/" pyenv-current-version "/envs"))
           (message (concat "Setting virtualenv path to ~/.pyenv/versions/" pyenv-current-version "/envs"))))))))

(add-hook 'find-file-hook 'ssbb-pyenv-hook)
;; (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
(add-hook 'pip-requirements-mode-hook #'pip-requirements-auto-complete-setup)

;(defadvice auto-complete-mode (around disable-auto-complete-for-python)
;  (unless (eq major-mode 'python-mode) ad-do-it))
;(ad-activate 'auto-complete-mode)

;;; python.el ends here
