(use-package rust-mode
  :bind (:map rust-mode-map
              ("C-c C-f" . nil)
              ("C-c C-d" . nil)
              ("C-c , f" . rust-format-buffer)
              ("C-c , d" . rist-dbg-wrap-or-unwrap)))

(use-package cargo
  :bind (:map cargo-minor-mode-map
  ("C-c C-c" . nil)

  ("C-c m e" . 'cargo-process-bench)
  ("C-c m b" . 'cargo-process-build)
  ("C-c m l" . 'cargo-process-clean)
  ("C-c m d" . 'cargo-process-doc)
  ("C-c m v" . 'cargo-process-doc-open)
  ("C-c m n" . 'cargo-process-new)
  ("C-c m i" . 'cargo-process-init)
  ("C-c m r" . 'cargo-process-run)
  ("C-c m x" . 'cargo-process-run-example)
  ("C-c m s" . 'cargo-process-search)
  ("C-c m t" . 'cargo-process-test)
  ("C-c m u" . 'cargo-process-update)
  ("C-c m c" . 'cargo-process-repeat)
  ("C-c m f" . 'cargo-process-current-test)
  ("C-c m o" . 'cargo-process-current-file-tests)
  ("C-c m m" . 'cargo-process-fmt)
  ("C-c m k" . 'cargo-process-check)
  ("C-c m a" . 'cargo-process-add)
  ("C-c m C-S-o" . 'cargo-process-outdated)
  ("C-c m C-S-k" . 'cargo-process-clippy)
  ("C-c m C-S-d" . 'cargo-process-rm)
  ("C-c m C-S-u" . 'cargo-process-upgrade)
  ("C-c m C-S-a" . 'cargo-process-audit))
  :hook
  (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :custom
  (flycheck-rust-check-tests nil)
  :hook
  (flycheck-mode . flycheck-rust-setup))

(use-package racer
  :after rust-mode
  :hook
  (rust-mode . racer-mode)
  (racer-mode . eldoc-mode)
  (racer-mode . company-mode)
  :custom
  (racer-cmd (expand-file-name "~/.cargo/bin/racer"))
  (company-tooltip-align-annotations t))

(use-package company-racer
  :custom
  (company-racer-executable (expand-file-name "~/.cargo/bin/racer"))
  :config
  (add-to-list 'company-backends 'company-racer))

;; (use-package rustic
;;   :mode ("\\.rs" . rustic-mode)
;;   :after lsp-mode
;;   :bind (:map rustic-mode-map
;; 	      ("M-." . xref-find-definitions)
;; 	      ("M-," . pop-tag-mark))
;;   :custom
;;   (rustic-format-display-method 'ignore)
;;   (rustic-format-trigger 'on-save))

(provide 'lang-rust)
