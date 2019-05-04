(eval-after-load 'grep
  '(progn
    (add-to-list 'grep-find-ignored-directories "local")
    (add-to-list 'grep-find-ignored-directories "build")
    (add-to-list 'grep-find-ignored-directories "media")))
(add-hook 'grep-mode-hook (lambda() (toggle-truncate-lines 1)))

(use-package ag)

(use-package auto-highlight-symbol
  :config
  (global-auto-highlight-symbol-mode t)
  (setq ahs-case-fold-search nil))

(use-package anzu
  :config
  (global-anzu-mode +1)
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp))

(use-package avy
  :bind
  ("C-c SPC" . avy-goto-char))

(use-package buffer-move)

(use-package company
  :bind
  ("M-TAB" . company-complete)
  ("M-;" . company-yasnippet)
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  :config
  (define-key company-active-map (kbd "C-n") (lambda () (interactive) (company-complete-common-or-cycle 1)))
  (define-key company-active-map (kbd "C-p") (lambda () (interactive) (company-complete-common-or-cycle -1)))
  :hook
  (after-init . global-company-mode))

(use-package company-statistics
  :config
  (company-statistics-mode))

(use-package counsel
  :custom
  (counsel-find-file-ignore-regexp ".*\.egg-info\\|__pycache__\\|.cache")
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-m" . counsel-M-x)
  ("C-x m" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-x c k" . counsel-yank-pop)
  ("M-;" . counsel-imenu))

(use-package counsel-projectile
  :bind
  ("C-x v" . counsel-projectile)
  ("C-x c p" . counsel-projectile-rg)
  :config
  (counsel-projectile-mode))

(use-package diffview
  :config
  ;; scroll-all-mode doesn't work with mouse.
  ;; WORKAROUND: https://www.emacswiki.org/emacs/ScrollAllMode
  (defun mwheel-scroll-all-function-all (func &optional arg)
    (if (and scroll-all-mode arg)
        (save-selected-window
          (walk-windows
           (lambda (win)
             (select-window win)
             (condition-case nil
                 (funcall func arg)
               (error nil)))))
      (funcall func arg)))

  (defun mwheel-scroll-all-scroll-up-all (&optional arg)
    (mwheel-scroll-all-function-all 'scroll-up arg))

  (defun mwheel-scroll-all-scroll-down-all (&optional arg)
    (mwheel-scroll-all-function-all 'scroll-down arg))

  (setq mwheel-scroll-up-function 'mwheel-scroll-all-scroll-up-all)
  (setq mwheel-scroll-down-function 'mwheel-scroll-all-scroll-down-all)
  ;; Activate scoll-all-mode when in diffview mode
  (add-hook 'diffview-mode (lambda() (scroll-all-mode)))
  )

(use-package dired-single)

(use-package dired-subtree
  :config
  (define-key dired-mode-map "i" 'dired-subtree-insert)
  (define-key dired-mode-map ";" 'dired-subtree-remove))

(use-package dockerfile-mode)

(use-package dumb-jump
  :config
  (dumb-jump-mode))

(use-package ediff
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-diff-options "-w")
  :config
  (setq-default ediff-highlight-all-diffs 'nil))

(use-package elfeed
  :bind
  ("C-x w" . elfeed)
  :custom
  (elfeed-feeds
   '("http://nullprogram.com/feed/"
     "http://planet.emacsen.org/atom.xml"
     "https://hnrss.org/frontpage"))
  :config
  ;; Entries older than 2 weeks are marked as read
  (add-hook 'elfeed-new-entry-hook #'(elfeed-make-tagger :before "2 weeks ago"
                                               :remove 'unread)))


(use-package exec-path-from-shell
  :config
  ;; Add GOPATH to shell
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-copy-env "PYTHONPATH")
  (exec-path-from-shell-copy-env "TERM")
  (exec-path-from-shell-initialize))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package f)

(use-package flycheck
  :custom
  (flycheck-indication-mode 'right-fringe)
  ;; Removed checks on idle/change for snappiness
  (flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-highlighting-mode 'symbols)
  (flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc make))
  ;; `flycheck-pos-tip'
  (flycheck-pos-tip-timeout 10)
  (flycheck-display-errors-delay 0.5)
  (flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  :config
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.23)))
  (when (eq window-system 'mac)
    (require 'flycheck-pos-tip)
    (flycheck-pos-tip-mode +1))
  (global-flycheck-mode 1))

(use-package flycheck-pos-tip
  :config
  (flycheck-pos-tip-mode))

(use-package forge)

(use-package git-gutter)

(use-package git-link
  :config
  (setq git-link-open-in-browser t
        git-link-use-commit t))

;; (use-package hl-line
;;   :config
;;   ;; Doesn't seem to play nice in emacs 25+
;;   (setq hl-line-sticky-flag nil
;;         global-hl-line-sticky-flag nil)

;;   (defvar-local current-hl-line-mode nil)
;;   (defun hl-line-on ()  (if current-hl-line-mode (hl-line-mode +1)))
;;   (defun hl-line-off () (if current-hl-line-mode (hl-line-mode -1)))
;;   ;;(add-hook hl-line-mode (lambda () (if current-hl-line-mode (setq current-hl-line-mode t))))
;;   (global-hl-line-mode))

(use-package hungry-delete
  :config
  (global-hungry-delete-mode))

(use-package imenu-list
  :custom
  (imenu-list-focus-after-activation t)
  (imenu-list-size 0.2)
  (imenu-list-auto-resize nil)
  :bind
  ("C-c m l" . imenu-list-minor-mode))

(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package kill-or-bury-alive
  :bind
  ("C-x k" . kill-or-bury-alive)
  ("C-c C-k" . kill-buffer))

(use-package magit
  :custom
  (magit-completing-read-function 'ivy-completing-read)
  :config
  (global-magit-file-mode -1)
  (setq magit-prefer-remote-upstream "origin")
  :bind
  ;; Magic
  ("C-x g s" . magit-status)
  ("C-x g x" . magit-checkout)
  ("C-x g c" . magit-commit)
  ("C-x g p" . magit-push)
  ("C-x g u" . magit-pull)
  ("C-x g e" . magit-ediff-resolve)
  ("C-x g r" . magit-rebase-interactive)
  ("C-x g b" . magit-blame))

(use-package magit-popup)

(use-package markdown-mode)

(use-package multiple-cursors
  :custom
  (mc/list-file (concat temp-dir "/.mc-lists.el"))
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C->" . mc/mark-all-like-this)
  ("C-c ;" . mc/skip-to-next-like-this))

(use-package page-break-lines)

(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default))

(use-package projectile
  :custom
  (projectile-enable-caching t)
  (projectile-cache-file (expand-file-name "projectile.cache" temp-dir))
  (projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" temp-dir))
  (projectile-completion-system 'ivy)
  (projectile-indexing-method 'native)
  :config
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories "data/postgres")
  (add-to-list 'projectile-globally-ignored-directories "data/solr")
  (add-to-list 'projectile-globally-ignored-directories "data/mysql")
  (projectile-mode)
  :bind
  ("C-x c a" . projectile-ripgrep)
  ("C-c p k" . projectile-kill-buffers))

(use-package dashboard
  :load-path "~/.emacs.d/vendor/emacs-dashboard"
  :custom
  (dashboard-items '((agenda . 10)
                     (recents  . 10)
                     (projects . 15)
                     (bookmarks . 15)
                     (registers . 10)))
  :config
  (dashboard-setup-startup-hook))

(if (memq window-system '(mac ns))
    (use-package dash-at-point
      :bind
      ("C-c d" . dash-at-point)
      ("C-c e" . dash-at-point-with-docset))
  (use-package zeal-at-point
    :bind
    ("C-c d" . zeal-at-point)))

(use-package recentf
  :config
  (setq recentf-exclude '("/elpa/" ;; ignore all files in elpa directory
                          ".*?autoloads.el$"
                          "/tmp/" ;; ignore temporary files
                          "*/.elfeed/index"
                          "company-statistics-cache.el"
                          ".gitignore"
                          "*/Documents/org-mode"
                          )
        recentf-save-file (recentf-expand-file-name "~/.emacs.d/private/cache/recentf"))
  (recentf-mode 1))

(use-package resize-window
  :bind
  ("C-x /" . resize-window))

(use-package restclient
  :custom
  (restclient-log-request t)
  (restclient-same-buffer-response t))

(use-package rotate
  :bind
  ("C-c C-r w" . rotate-window)
  ("C-c C-r l" . rotate-layout))

(use-package smartparens
  :config
  (require 'smartparens-config))

(use-package smex
  :custom
  (smex-save-file (expand-file-name "smex-items" temp-dir)))

(use-package syntax-subword
  :config
  (syntax-subword-mode)
  :bind
  ("M-j" . syntax-subword-backward)
  ("M-l" . syntax-subword-forward))

(use-package undo-tree
  :config
  (global-undo-tree-mode 1)
  :custom
  ;; Remember undo history
  (undo-tree-auto-save-history nil)
  (undo-tree-history-directory-alist `(("." . ,(concat temp-dir "/undo/")))))

(use-package which-key
  :config
  (which-key-mode))

(use-package windmove
  :bind
  ("C-c i" . windmove-up)
  ("C-c k" . windmove-down)
  ("C-c j" . windmove-left)
  ("C-c l" . windmove-right))

(use-package wgrep)

(use-package yasnippet
  :bind
  ("C-c y s" . yas-insert-snippet)
  ("C-c y v" . yas-visit-snippet-file)
  :config
  (setq yas-snippet-dirs (append yas-snippet-dirs '("~/.emacs.d/snippets")))
  (yas-global-mode 1))

(use-package zoom-window
  :bind
  ("C-x C-z" . zoom-window-zoom)
  :custom
  (zoom-window-mode-line-color "#22252c"))

(use-package diminish
  :config
  (defmacro diminish-minor-mode (filename mode &optional abbrev)
    `(eval-after-load (symbol-name ,filename)
       '(diminish ,mode ,abbrev)))

  (defmacro diminish-major-mode (mode-hook abbrev)
    `(add-hook ,mode-hook
               (lambda () (setq mode-name ,abbrev))))

  (diminish-minor-mode 'abbrev 'abbrev-mode)
  (diminish-minor-mode 'company 'company-mode)
  (diminish-minor-mode 'elpy 'elpy-mode)
  (diminish-minor-mode 'eldoc 'eldoc-mode)
  (diminish-minor-mode 'flycheck 'flycheck-mode)
  (diminish-minor-mode 'flyspell 'flyspell-mode)
  (diminish-minor-mode 'projectile 'projectile-mode)
  (diminish-minor-mode 'undo-tree 'undo-tree-mode)
  (diminish-minor-mode 'yasnippet 'yas-minor-mode)
  (diminish-minor-mode 'magit 'auto-revert-mode)
  (diminish-minor-mode 'Git-Gutter 'git-gutter-mode)
  (diminish-minor-mode 'Which-Key 'which-key-mode)
  (diminish-minor-mode 'ivy 'ivy-mode)
  (diminish-major-mode 'emacs-lisp-mode-hook "el")
  (diminish-major-mode 'python-mode-hook "Py"))

(provide 'core-extensions)
