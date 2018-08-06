;; Favor local eslint over global, if available
(defun javascript-init-flycheck-eslint ()
  (when (derived-mode-p 'js2-mode)
    (when-let ((eslint (expand-file-name "node_modules/eslint/bin/eslint.js"
					 (core-project-root)))
	       (exists-p (file-exists-p eslint))
	       (executable-p (file-executable-p eslint)))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(defun javascript-init-flycheck ()
  (setq-default flycheck-disabled-checkers '(javascript-jshint))
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (add-hook 'flycheck-mode-hook #'javascript-init-flycheck-eslint))

(use-package emmet-mode
  :init
  (add-hook 'js2-mode-hook #'emmet-mode)
  :config
  (setq emmet-move-cursor-between-quotes t)
  (setq emmet-expand-jsx-className? t))

(use-package js2-mode
  :after flycheck-mode
  :mode
  ("\\.js$" . js2-mode)
  ("\\.jsx$" . js2-jsx-mode)
  ("\\.json$" . js2-jsx-mode)
  :config
  (setq js-indent-level 2
	js2-basic-offset 2
	js2-bounce-indent-p t)
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  (add-hook 'js2-mode-hook #'company-mode)
  (define-key js-mode-map (kbd "M-.") nil)
  (javascript-init-flycheck))

(use-package xref-js2
 :init
 (add-hook 'js2-mode-hook (lambda ()
			     (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
 :config
 (define-key js2-mode-map (kbd "M-.") nil))

(use-package tern
  :init (add-hook 'js2-mode-hook #'tern-mode)
  :config
  (advice-add #'tern-project-dir :override #'core*project-root))

(use-package company-tern
  :config
  (defun activate-tern-hook ()
    "Hook for `js-mode'."
    (set (make-local-variable 'company-backends)
	 '((company-tern company-files))))
  (add-hook 'js2-mode-hook 'activate-tern-hook))

(use-package web-beautify)

(provide 'core-javascript)
