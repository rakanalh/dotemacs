(use-package rust-mode)

(use-package flycheck-rust
  :hook
  (flycheck-mode-hook . flycheck-rust-setup))

(use-package racer
  :after rust-mode
  :hook
  (rust-mode-hook . racer-mode)
  (racer-mode-hook .eldoc-mode)
  (racer-mode-hook . company-mode)
  :custom
  (company-tooltip-align-annotations t))

(use-package company-racer
  :after racer-mode
  :config
  (add-to-list 'company-backends 'company-racer))

(provide 'core-rust)
