(require 'auto-complete-config)
(require 'expand-region)
(require 'helm)
(require 'helm-config)
(require 'hlinum)
(require 'neotree)
(require 'recentf)
(require 'spaceline-config)
(require 'which-key)
(require 'doom)

(if window-system
      (custom-set-variables
       ;; custom-set-variables was added by Custom.
       ;; If you edit it by hand, you could mess it up, so be careful.
       ;; Your init file should contain only one such instance.
       ;; If there is more than one, they won't work right.
       '(custom-enabled-themes (quote (doom-one)))
       '(custom-safe-themes
         (quote
          ("bd8a462608ca326957e1d9ba3bd165aed80959f6a584b9c5bd8c63e9ec42ed2e" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476")))))

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
(spaceline-emacs-theme)
(show-paren-mode 1)
(which-key-mode)
(ac-config-default)
;(global-git-gutter-mode t)
(yas-global-mode 1)
(helm-mode 1)
(global-undo-tree-mode 1)
(desktop-save-mode 0)
(git-gutter:linum-setup)
(hlinum-activate)

;; Env vars
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin/:$GOPATH"))
(setenv "SHELL" "/bin/zsh")

;; Variables
(setq fringes-outside-margins t)
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
(setq-default right-fringe-width  12)
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

(use-package hl-line
  :init (add-hook! (prog-mode markdown-mode) 'hl-line-mode)
  :config
  ;; Doesn't seem to play nice in emacs 25+
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil)

  (defvar-local doom--hl-line-mode nil)
  (defun doom|hl-line-on ()  (if doom--hl-line-mode (hl-line-mode +1)))
  (defun doom|hl-line-off () (if doom--hl-line-mode (hl-line-mode -1)))
  (add-hook! hl-line-mode (if hl-line-mode (setq doom--hl-line-mode t)))
  ;; Disable line highlight in visual mode
  (add-hook 'evil-visual-state-entry-hook 'doom|hl-line-off)
  (add-hook 'evil-visual-state-exit-hook  'doom|hl-line-on))

(use-package visual-fill-column :defer t
  :config
  (setq-default visual-fill-column-center-text nil
                visual-fill-column-width fill-column
                split-window-preferred-function 'visual-line-mode-split-window-sensibly))

(require 'spacemacs-startup)
(spacemacs/setup-startup-hook)
