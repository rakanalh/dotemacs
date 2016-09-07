(use-package company
  :ensure t
  :init
  (require 'auto-complete-config)
  :config
  (ac-config-default)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package expand-region
  :ensure t)

(use-package flycheck
  :ensure t
  :config
  (setq flycheck-indication-mode 'right-fringe
      ;; Removed checks on idle/change for snappiness
      flycheck-check-syntax-automatically '(save mode-enabled)
      flycheck-highlighting-mode 'symbols
      flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc make)
      ;; `flycheck-pos-tip'
      flycheck-pos-tip-timeout 10
      flycheck-display-errors-delay 0.5)
  (when (eq window-system 'mac)
    (require 'flycheck-pos-tip)
    (flycheck-pos-tip-mode +1))
  ;; Enable flycheck
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package helm
  :ensure t
  :init
  (require 'helm-config)
  :config
  (setq helm-split-window-in-side-p t
        helm-split-window-default-side 'below)
  (helm-mode 1))

(use-package helm-ag
  :ensure t)

(use-package hlinum
  :ensure t
  :config
  (hlinum-activate))

(use-package hl-line
  :init (add-hook! (prog-mode markdown-mode) 'hl-line-mode)
  :config
  ;; Doesn't seem to play nice in emacs 25+
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil)

  (defvar-local current-hl-line-mode nil)
  (defun hl-line-on ()  (if current-hl-line-mode (hl-line-mode +1)))
  (defun hl-line-off () (if current-hl-line-mode (hl-line-mode -1)))
  (add-hook! hl-line-mode (if current-hl-line-mode (setq current-hl-line-mode t)))
  (global-hl-line-mode))

(use-package linum-mode
  :ensure t
  :config
  (setq linum-format " %3d ")
  (global-linum-mode nil))

(use-package neotree
  :ensure t
  :config
  (setq neo-theme 'arrow
        neotree-smart-optn t
        neo-window-fixed-size nil)
  (advice-add 'neo-buffer--insert-fold-symbol :override 'neo-insert-fold-symbol)
  (advice-add 'neo-buffer--insert-root-entry :filter-args 'neo-insert-root-entry)
  ;; Disable linum for neotree
  (add-hook 'neo-after-create-hook 'my/neotree-hook))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode))

(use-package recentf
  :ensure t
  :config
  (recentf-mode 1))

(use-package spacemacs-startup
  :config
  (spacemacs/setup-startup-hook))

(use-package syntax-subword
  :ensure t
  :config
  (syntax-subword-mode))

(use-package undotree
  :ensure t
  :config
  (global-undo-tree-mode 1))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package diminish-minor-mode
  :ensure t
  :config
  (diminish-minor-mode 'abbrev 'abbrev-mode)
  (diminish-minor-mode 'company 'company-mode)
  (diminish-minor-mode 'elpy 'elpy-mode)
  (diminish-minor-mode 'eldoc 'eldoc-mode)
  (diminish-minor-mode 'flycheck 'flycheck-mode)
  (diminish-minor-mode 'flyspell 'flyspell-mode)
  (diminish-minor-mode 'projectile 'projectile-mode)
  (diminish-minor-mode 'undo-tree 'undo-tree-mode)
  (diminish-minor-mode 'yasnippet 'yas-minor-mode)
  (diminish-minor-mode 'helm 'helm-mode)
  (diminish-minor-mode 'Auto-Complete 'auto-complete-mode)
  (diminish-minor-mode 'magit 'auto-revert-mode)
  (diminish-minor-mode 'Git-Gutter 'git-gutter-mode)
  (diminish-minor-mode 'Which-Key 'which-key-mode)

  (diminish-major-mode 'emacs-lisp-mode-hook "el")
  (diminish-major-mode 'python-mode-hook "Py"))
