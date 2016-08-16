(require 'auto-complete-config)
(require 'expand-region)
(require 'helm)
(require 'helm-config)
(require 'hlinum)
(require 'neotree)
(require 'powerline)
(require 'recentf)
(require 'which-key)

(if window-system
      (custom-set-variables
       ;; custom-set-variables was added by Custom.
       ;; If you edit it by hand, you could mess it up, so be careful.
       ;; Your init file should contain only one such instance.
       ;; If there is more than one, they won't work right.
       '(custom-enabled-themes (quote (doom-one)))
       '(custom-safe-themes
         (quote
          ("45d403c0a4c170d7aba1c133fe3f9b816f2936ae84e65217637b3e1c7179ee07" "bd8a462608ca326957e1d9ba3bd165aed80959f6a584b9c5bd8c63e9ec42ed2e" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476")))))

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
(global-hl-line-mode)
(recentf-mode 1)
(show-paren-mode 1)
(which-key-mode)
(ac-config-default)
;(global-git-gutter-mode t)
(yas-global-mode 1)
(helm-mode 1)
(global-undo-tree-mode 1)
(desktop-save-mode 0)
;(git-gutter:linum-setup)
(hlinum-activate)

;; Env vars
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin/:$GOPATH"))
(setenv "SHELL" "/bin/zsh")

;; Variables
(setq fringes-outside-margins t)
(setq cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
(setq neotree-smart-optn t)
(setq neo-theme 'arrow)
(setq helm-split-window-in-side-p t)
(setq helm-split-window-default-side 'below)
(setq exec-path (append exec-path '("/usr/local/bin/")))
(setq backup-inhibited t)
(setq auto-save-default nil)
(setq inhibit-startup-message t)
(setq make-backup-files nil)
(setq warning-minimum-level :emergency)
(setq-default indent-tabs-mode nil)
(setq neo-window-fixed-size nil)
(setq neo-theme 'arrow)
(setq flycheck-indication-mode 'right-fringe
      ;; Removed checks on idle/change for snappiness
      flycheck-check-syntax-automatically '(save mode-enabled)
      flycheck-highlighting-mode 'symbols
      flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc make)
      ;; `flycheck-pos-tip'
      flycheck-pos-tip-timeout 10
      flycheck-display-errors-delay 0.5)


(define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
  [0 0 0 0 0 4 12 28 60 124 252 124 60 28 12 4 0 0 0 0])

(when (eq window-system 'mac)
  (require 'flycheck-pos-tip)
  (flycheck-pos-tip-mode +1))

(setq x-select-enable-clipboard t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t
      require-final-newline t
      visible-bell t
      load-prefer-newer t
      backup-directory-alist `(("." . ,(concat user-emacs-directory
					       "backups"))))

;; Custom line number stuff
(setq linum-format "%3d ")
(set-face-attribute 'fringe nil)
(set-face-foreground 'linum-highlight-face "#00B3EF")
(set-face-background 'linum-highlight-face "#1f252b")

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

(advice-add 'neo-buffer--insert-fold-symbol :override 'neo-insert-fold-symbol)
(advice-add 'neo-buffer--insert-root-entry :filter-args 'neo-insert-root-entry)

(require 'spacemacs-startup)
(spacemacs/setup-startup-hook)
