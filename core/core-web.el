(use-package web-mode
  :mode
  ("\\.html$" . web-mode)
  ("\\.phtml$" . web-mode)
  ("\\.mustache$" . web-mode)
  ("\\.djhtml$" . web-mode)
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
	web-mode-enable-current-element-highlight t)
  (add-hook 'web-mode-hook  #'js2-minor-mode))

(use-package company-web
  :config
  (add-hook 'web-mode-hook (lambda ()
			     (set (make-local-variable 'company-backends) '(company-web-html))
			     (company-mode t)))
  (defun my-web-mode-hook ()
    "Hook for `web-mode'."
    (set (make-local-variable 'company-backends)
         '(company-tern company-web-html company-yasnippet company-files)))

  (add-hook 'web-mode-hook 'my-web-mode-hook))

(provide 'core-web)
