;; REQUIREMENTS:
;; go get -u golang.org/x/tools/cmd/...
;; go get -u github.com/rogpeppe/godef
;; go get -u github.com/nsf/gocode
;; go get -u github.com/kisielk/errcheck

(use-package go-guru)
(use-package go-mode
  :config
  ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  (add-hook 'go-mode-hook 'company-mode)
  ;; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'setup-go-mode-compile)
  (add-hook 'go-mode-hook #'smartparens-mode)
  (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)
  (add-hook 'go-mode-hook (lambda ()
			     (setq tab-width 4)
			     (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
  			     (local-set-key (kbd "C-c C-g") 'go-goto-imports)
			     (set (make-local-variable 'company-backends) '(company-go))
			     (company-mode))))

(use-package go-errcheck)

(use-package go-add-tags)

(use-package company-go
  :after go-mode
  :bind (:map go-mode-map
  ; Godef jump key binding
  ("M-." . godef-jump)))

(use-package flymake-go)

(use-package go-eldoc
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(defun setup-go-mode-compile ()
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet")))

(provide 'core-go)
