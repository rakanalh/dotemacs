(use-package ace-jump-mode
  :bind
  ("C-c SPC" . ace-jump-mode))

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package dockerfile-mode)

(use-package exec-path-from-shell)

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package flycheck
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

(use-package git-gutter)

(use-package helm
  :init
  (require 'helm-config)
  :config
  (setq helm-split-window-in-side-p t
        helm-split-window-default-side 'below
	helm-idle-delay 0.0
	helm-input-idle-delay 0.01
	helm-quick-update t
	helm-ff-skip-boring-files t
	helm-boring-file-regexp-list '("\\.DS_Store" "\\.git" "__pycache__" "\\undo-tree-history"))
  (helm-mode 1)
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x v" . helm-projectile)
         ("C-x c o" . helm-occur)
         ("C-x c p" . helm-projectile-ag)
         ("C-x c k" . helm-show-kill-ring)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)))

(use-package helm-ag)

(use-package helm-git-grep)

(use-package helm-swoop
  :bind
  ("C-x c s" . helm-swoop))

(use-package hlinum
  :config
  (hlinum-activate))

(use-package hl-line
  :config
  ;; Doesn't seem to play nice in emacs 25+
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil)

  (defvar-local current-hl-line-mode nil)
  (defun hl-line-on ()  (if current-hl-line-mode (hl-line-mode +1)))
  (defun hl-line-off () (if current-hl-line-mode (hl-line-mode -1)))
  ;;(add-hook hl-line-mode (lambda () (if current-hl-line-mode (setq current-hl-line-mode t))))
  (global-hl-line-mode))

(use-package linum
  :config
  (setq linum-format " %3d ")
  (global-linum-mode nil))

(use-package magit
  :bind
  ;; Magic
  ("C-x g s" . magit-status)
  ("C-x g c" . magit-commit)
  ("C-x g p" . magit-push)
  ("C-x g u" . magit-pull))

(use-package magit-popup)

(use-package markdown-mode)

(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this))

(use-package neotree
  :config
  (setq neo-theme 'arrow
        neotree-smart-optn t
        neo-window-fixed-size nil)
  (advice-add 'neo-buffer--insert-fold-symbol :override 'neo-insert-fold-symbol)
  (advice-add 'neo-buffer--insert-root-entry :filter-args 'neo-insert-root-entry)
  ;; Disable linum for neotree
  (add-hook 'neo-after-create-hook 'disable-neotree-hook)
  :bind
  ("C-x C-t" . neotree-toggle))

(use-package org
  :config
  (setq org-directory "~/DropBox/org-mode"
        org-agenda-files (list "~/DropBox/org-mode/ideas.org")
        org-default-notes-file (concat org-directory "/todo.org"))
  (org-babel-do-load-languages
   'org-babel-load-languages '((python . t)))
  :bind
  ("\C-cl" . org-store-link)
  ("\C-ca" . org-agenda))

(use-package org-projectile
  :config
  (org-projectile:per-repo)
  (setq org-projectile:per-repo-filename "todo.org"
	org-agenda-files (append org-agenda-files (org-projectile:todo-files)))
  :bind
  ("C-c c" . org-projectile:capture-for-current-project)
  ("C-c n p" . org-projectile:project-todo-completing-read))

(use-package org-bullets
  :config
  (setq org-hide-leading-stars t)
  (add-hook 'org-mode-hook
            (lambda ()
              (org-bullets-mode t))))

(use-package page-break-lines)

(use-package projectile
  :config
  (setq projectile-known-projects-file
        (expand-file-name "projectile-bookmarks.eld" temp-dir))
  (projectile-global-mode))

(use-package recentf
  :config
  (setq recentf-save-file (recentf-expand-file-name "~/.emacs.d/private/cache/recentf"))
  (recentf-mode 1))

(use-package smartparens)

(use-package spacemacs-startup
  :ensure nil
  :config
  (spacemacs/setup-startup-hook))

(use-package syntax-subword
  :config
  (syntax-subword-mode)
  :bind
  ("<M-left>" . syntax-subword-backward)
  ("<M-right>" . syntax-subword-forward))

(use-package undo-tree
  :config
  (global-undo-tree-mode 1))

(use-package which-key
  :config
  (which-key-mode))

(use-package windmove
  :bind
  ("C-x <up>" . windmove-up)
  ("C-x <down>" . windmove-down)
  ("C-x <left>" . windmove-left)
  ("C-x <right>" . windmove-right))

(use-package wgrep)

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package diminish
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
  (diminish-minor-mode 'magit 'auto-revert-mode)
  (diminish-minor-mode 'Git-Gutter 'git-gutter-mode)
  (diminish-minor-mode 'Which-Key 'which-key-mode)

  (diminish-major-mode 'emacs-lisp-mode-hook "el")
  (diminish-major-mode 'python-mode-hook "Py"))
