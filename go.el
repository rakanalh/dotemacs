(use-package go-mode
  :ensure t
  :config
  ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  (add-hook 'go-mode-hook 'setup-go-mode-compile)
  (add-hook 'go-mode-hook #'smartparens-mode))

(use-package go-autocomplete
  :ensure t
  :bind (:map go-mode-map
  ; Godef jump key binding
  ("M-." . godef-jump)))

(use-package go-eldoc
  :ensure t)

(use-package smartparens
  :ensure t)

;; Add GOPATH to shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-initialize))

(defun setup-go-mode-compile ()
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet")))

(provide 'go)
