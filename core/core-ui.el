;; `window-divider-mode' gives us finer control over the border between windows.
;; The native border "consumes" a pixel of the fringe on righter-most splits (in
;; Yamamoto's emacs-mac at least), window-divider does not. You can also control
;; vertical borders between windows (introduced in Emacs 25.1+)
(when (boundp 'window-divider-mode)
  (setq window-divider-default-places t
        window-divider-default-bottom-width 1
        window-divider-default-right-width 1)
  (window-divider-mode +1))

(use-package all-the-icons)

(use-package doom-themes
  :config
  (load-theme 'doom-tomorrow-night t)

  ;(require 'doom-neotree)
  (doom-themes-neotree-config)  ; all-the-icons fonts must be installed!
  (doom-themes-org-config))

(use-package doom-modeline
      :ensure t
      :defer t
      :hook (after-init . doom-modeline-init)
      :custom
      (doom-modeline-env-command "pyenv local")
      :config
      (remove-hook 'focus-in-hook #'doom-modeline-update-env)
      (remove-hook 'find-file-hook #'doom-modeline-update-env))

(use-package solaire-mode
  :config
  ;; brighten buffers (that represent real files)
  (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
  ;; To enable solaire-mode unconditionally for certain modes:
  (add-hook 'ediff-prepare-buffer-hook #'solaire-mode)

  ;; ...if you use auto-revert-mode:
  (add-hook 'after-revert-hook #'turn-on-solaire-mode)

  ;; highlight the minibuffer when it is activated:
  (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)

  ;; if the bright and dark background colors are the wrong way around, use this
  ;; to switch the backgrounds of the `default` and `solaire-default-face` faces.
  ;; This should be used *after* you load the active theme!
  ;;
  ;; NOTE: This is necessary for themes in the doom-themes package!
  (solaire-mode-swap-bg))

(use-package git-gutter-fringe)

(use-package git-gutter
  :config
  (require 'git-gutter-fringe)
  (global-git-gutter-mode +1)
  ;; places the git gutter outside the margins.
    (setq-default fringes-outside-margins t)
    ;; thin fringe bitmaps
    (fringe-helper-define 'git-gutter-fr:added '(center repeated)
      "XXX.....")
    (fringe-helper-define 'git-gutter-fr:modified '(center repeated)
      "XXX.....")
    (fringe-helper-define 'git-gutter-fr:deleted 'bottom
      "X......."
      "XX......"
      "XXX....."
      "XXXX....")

  (add-hook 'focus-in-hook 'git-gutter:update-all-windows))

(use-package powerline)

;; Custom line number stuff
(set-face-attribute 'fringe nil)

(setq org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t)

(when (display-graphic-p)
  ;; standardize fringe width
  (push (cons 'left-fringe  '4) default-frame-alist)
  (push (cons 'right-fringe '4) default-frame-alist)
  ;; no fringe in the minibuffer
  (set-window-fringes (minibuffer-window) 0 0 nil))

;; because git-gutter is in the left fringe
(setq flycheck-indication-mode 'right-fringe)
;; A non-descript, left-pointing arrow
(fringe-helper-define 'flycheck-fringe-bitmap-double-arrow 'center
  "...X...."
  "..XX...."
  ".XXX...."
  "XXXX...."
  ".XXX...."
  "..XX...."
  "...X....")

(provide 'core-ui)
