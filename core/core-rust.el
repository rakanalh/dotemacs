(use-package rust-mode)

(use-package flycheck-rust
  :hook
  (flycheck-mode . flycheck-rust-setup))

(use-package racer
  :after rust-mode
  :hook
  (rust-mode . racer-mode)
  (racer-mode .eldoc-mode)
  (racer-mode . company-mode)
  :custom
  (company-tooltip-align-annotations t))

(use-package company-racer
  :after racer-mode
  :config
  (add-to-list 'company-backends 'company-racer))

(provide 'core-rust)
