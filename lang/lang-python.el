;;; package --- python configs
;;; Commentary:
;;; Contains my python configs

;;; Code:

(require 'f)

(defvar pyenv-current-version nil nil)

(defvar pyenv-current-version nil nil)

(use-package python
  :mode ("\\.py" . python-mode)
  :config
  (setq python-indent-offset 4)
  (setq python-mode-map (make-sparse-keymap))
  :hook
  (python-mode . anaconda-mode)
  (python-mode . anaconda-eldoc-mode))

(use-package anaconda-mode
  :config
  (add-to-list 'company-backends 'company-yasnippet)
  :bind (:map anaconda-mode-map
              ("M-[" . python-nav-backward-block)
              ("M-]" . python-nav-forward-block)
              ("M-J" . indent-rigidly-left-to-tab-stop)
              ("M-L" . indent-rigidly-right-to-tab-stop)
              ("M-'" . anaconda-mode-find-references)
              ("M-." . anaconda-mode-find-definitions)
              ("M-," . pop-tag-mark)
              ("DEL" . python-dedent-or-remove)))

(use-package company-anaconda
  :config
  (eval-after-load "company"
    '(add-to-list 'company-backends 'company-anaconda)))


(use-package pip-requirements
  :config
  (add-hook 'pip-requirements-mode-hook #'pip-requirements-auto-complete-setup))

(use-package py-autopep8)

(use-package pyenv-mode
  :init
  (add-to-list 'exec-path "~/.pyenv/shims")
  (setenv "WORKON_HOME" "~/.pyenv/versions/")
  :bind (:map pyenv-mode-map
              ("C-c C-s" . nil)
              ("C-c C-u" . nil)
              ("C-c m v s" . pyenv-mode-set)
              ("C-c m v u" . pyenv-mode-unset)
              ("C-c m v e" . pyenv-activate-current-project))
  :config
  (pyenv-mode))

(use-package py-isort
  :config
  (defun python-mode-before-save-hook ()
    (when (eq major-mode 'python-mode) (eq major-mode 'c-mode)
      (py-isort-before-save)))

  (add-hook 'before-save-hook 'python-mode-before-save-hook))


(defun pyenv-init()
  "Initialize pyenv's current version to the global one."
  (let ((global-pyenv (replace-regexp-in-string "\n" "" (shell-command-to-string "pyenv global"))))
    (message (concat "Setting pyenv version to " global-pyenv))
    (pyenv-mode-set global-pyenv)
    (setq pyenv-current-version global-pyenv)))

(defun pyenv-activate-current-project ()
  "Automatically activates pyenv version if .python-version file exists."
  (interactive)
  (let ((python-version-directory (locate-dominating-file (buffer-file-name) ".python-version")))
    (if python-version-directory
        (let* ((pyenv-version-path (f-expand ".python-version" python-version-directory))
               (pyenv-current-version (s-trim (f-read-text pyenv-version-path 'utf-8))))
          (pyvenv-workon pyenv-current-version)
          (pyenv-mode-set pyenv-current-version)
          (setq python-shell-interpreter (expand-file-name (concat "~/.pyenv/versions/" pyenv-current-version "/bin/python")))
          (setq importmagic-python-interpreter python-shell-interpreter)
          (setq doom-modeline-env-version pyenv-current-version)
          (message (concat "Setting virtualenv to " pyenv-current-version))))))

(add-hook 'after-init-hook 'pyenv-init)
(add-hook 'projectile-after-switch-project-hook 'pyenv-activate-current-project)

(defun jsonify-python-output ()
  "Convert the output of a logged/printed dict into a pretty JSON format."
  (interactive)
  (let* ((min (if (region-active-p) (region-beginning) (point-min)))
        (max (if (region-active-p) (region-end) (point-max)))
        (max-altered max))
    (goto-char min)
    (while (re-search-forward "Decimal(\"\\\([0-9.]+\\\)\")" max t)
      (replace-match "\\1")
      (setq max-altered (- max-altered 11)))

    (replace-in-buffer "'" "\"" min max)
    (replace-in-buffer "None" "null" min max)
    (replace-in-buffer "True" "true" min max)
    (replace-in-buffer "False" "false" min max)

    (json-pretty-print min max-altered)))

(defun replace-in-buffer (search replace start end)
  "Replace all occurances of a SEARCH with REPLACE in buffer from START to END."
  (goto-char start)
  (while (search-forward search end t)
    (replace-match replace t)))

(defun python-dedent-or-remove ()
  "Delete region if selected, otherwise, dedent."
  (interactive)
  (if (region-active-p)
      (delete-region (region-beginning) (region-end))
    (call-interactively #'python-indent-dedent-line-backspace 0)))

(provide 'lang-python)
;;; python.el ends here
