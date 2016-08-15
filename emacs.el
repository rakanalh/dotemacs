(require 'auto-complete-config)
(require 'expand-region)
(require 'helm)
(require 'helm-config)
(require 'neotree)
(require 'recentf)
(require 'spaceline-config)
(require 'which-key)

(if window-system
      (custom-set-variables
       ;; custom-set-variables was added by Custom.
       ;; If you edit it by hand, you could mess it up, so be careful.
       ;; Your init file should contain only one such instance.
       ;; If there is more than one, they won't work right.
       '(custom-enabled-themes (quote (spacemacs-dark)))
       '(custom-safe-themes
         (quote
          ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476")))))

(custom-set-variables
 '(menu-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(widget-button ((t (:foreground "gray90" :underline nil :weight bold)))))

(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)

;; Disable toolbar & menubar
(menu-bar-mode -1)
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))


;; Enable modes
(projectile-global-mode)
(global-linum-mode nil)
(recentf-mode 1)
(spaceline-spacemacs-theme)
(show-paren-mode 1)
(which-key-mode)
(ac-config-default)
(global-git-gutter-mode t)
(yas-global-mode 1)
(helm-mode 1)
(global-undo-tree-mode 1)
(desktop-save-mode 0)
(git-gutter:linum-setup)

;; Env vars
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin/:$GOPATH"))
(setenv "SHELL" "/bin/zsh")

;; Variables
(setq neotree-smart-optn t)
(setq neo-theme 'arrow)
(setq helm-split-window-in-side-p t)
(setq helm-split-window-default-side 'below)
(setq exec-path (append exec-path '("/usr/local/bin/")))
(setq backup-inhibited t)
(setq auto-save-default nil)
(setq inhibit-startup-message t)
(setq make-backup-files nil)
(setq-default indent-tabs-mode nil)

(setq x-select-enable-clipboard t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t
      require-final-newline t
      visible-bell t
      load-prefer-newer t
      backup-directory-alist `(("." . ,(concat user-emacs-directory
					       "backups"))))

;; Custom line number stuff
(setq linum-format 'dynamic)
(setq-default left-fringe-width  12)
(setq-default right-fringe-width  0)
(set-face-attribute 'fringe nil)

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
(diminish-major-mode 'python-mode-hook "Py")


;; Delete trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Flycheck enable
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'after-init-hook 'global-company-mode)

;; Disable linum for neotree
(add-hook 'neo-after-create-hook 'my/neotree-hook)

(require 'spacemacs-startup)
(spacemacs/setup-startup-hook)
