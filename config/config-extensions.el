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
  ("C-c j j" . avy-goto-char-timer)
  ("C-c j w" . avy-goto-word-1)
  ("C-c j l" . avy-goto-line))

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
  (add-to-list 'company-backends 'company-yasnippet)
  (add-to-list 'company-backends 'company-dabbrev-code)
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
  ("C-c j i" . counsel-imenu))

(use-package counsel-projectile
  :bind
  ("C-c p f" . counsel-projectile)
  ("C-c s p" . counsel-projectile-rg)
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
  (setq dumb-jump-force-searcher 'rg)
  (dumb-jump-mode))

(use-package ediff
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-diff-options "-w")
  :config
  (setq-default ediff-highlight-all-diffs 'nil))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package erc
  :custom
  (erc-autojoin-channels-alist '(("freenode.net" "#qutebrowser" "#emacs")))
  (erc-autojoin-timing 'ident)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 22)
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-threshold-time 43200)
  (erc-prompt-for-nickserv-password nil)
  (erc-server-reconnect-attempts 5)
  (erc-server-reconnect-timeout 3)
  (erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                             "324" "329" "332" "333" "353" "477"))
  :config
  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'spelling)
  (erc-services-mode 1)
  (erc-update-modules))

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
  :init
  (setq flycheck-keymap-prefix (kbd "C-c e"))
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

(use-package iedit
  :bind
  ("C-;" . iedit-mode))

(use-package imenu-list
  :custom
  (imenu-list-focus-after-activation t)
  (imenu-list-size 0.2)
  (imenu-list-auto-resize nil))

(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package kill-or-bury-alive
  :bind
  ("C-c b x" . kill-or-bury-alive)
  ("C-c b k" . kill-buffer))

(use-package ledger-mode)

(use-package magit
  :custom
  (magit-completing-read-function 'ivy-completing-read)
  :config
  (global-magit-file-mode -1)
  (setq magit-prefer-remote-upstream "origin")
  :bind
  ;; Magic
  ("C-c g s" . magit-status)
  ("C-c g x" . magit-checkout)
  ("C-c g c" . magit-commit)
  ("C-c g p" . magit-push)
  ("C-c g u" . magit-pull)
  ("C-c g e" . magit-ediff-resolve)
  ("C-c g r" . magit-rebase-interactive)
  ("C-c g b" . magit-blame))

(use-package magit-todos)

(use-package magit-popup)

(use-package markdown-mode)

(use-package multiple-cursors
  :bind
  ("C-c s ." . mc/mark-next-like-this)
  ("C-c s ," . mc/mark-previous-like-this)
  ("C-c s >" . mc/mark-all-like-this)
  ("C-c s ;" . mc/skip-to-next-like-this)
  :custom
  (mc/list-file (concat temp-dir "/.mc-lists.el")))

(use-package page-break-lines)

(use-package perspective
  :config
  (persp-mode)
  (setf (cdr persp-mode-map) nil))

(use-package persp-projectile
  :bind
  ("C-c p l" . projectile-persp-switch-project))

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
  (projectile-sort-order 'recently-active)
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
  (dashboard-items '((recents  . 10)
                     (projects . 15)
                     (bookmarks . 15)
                     (registers . 10)))
  :config
  (setq dashboard-set-init-info t)
  (setq dashboard-set-navigator t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-center-content t)
  (setq dashboard-footer "Enjoy!")
  (setq dashboard-footer-icon
        (all-the-icons-octicon "dashboard"
                               :height 1.1
                               :v-adjust -0.05
                               :face 'font-lock-keyword-face))
  (dashboard-setup-startup-hook))

(if (memq window-system '(mac ns))
    (use-package dash-at-point
      :bind
      ("C-c j x" . dash-at-point))
  (use-package zeal-at-point
    :bind
    ("C-c j x" . zeal-at-point)))

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
  ("C-w /" . resize-window))

(use-package restclient
  :custom
  (restclient-log-request t)
  (restclient-same-buffer-response t))

(use-package rotate
  :bind
  ("C-w r w" . rotate-window)
  ("C-w r l" . rotate-layout))

(use-package smartparens
  :config
  (require 'smartparens-config))

(use-package smex
  :custom
  (smex-save-file (expand-file-name "smex-items" temp-dir)))

(use-package sql
  :config
  (add-to-list 'auto-mode-alist '("\\.psql$" . sql-mode)))

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

(use-package window-purpose
  :config
  (purpose-x-magit-single-on))

(use-package which-key
  :config
  (which-key-add-key-based-replacements
    "C-c b" "Buffers")
  (which-key-add-key-based-replacements
    "C-c e" "Errors")
  (which-key-add-key-based-replacements
    "C-c g" "Magit")
  (which-key-add-key-based-replacements
    "C-c h" "Hydras")
  (which-key-add-key-based-replacements
    "C-c j" "Jump")
  (which-key-add-key-based-replacements
    "C-c C-m" "Menu")
  (which-key-add-key-based-replacements
    "C-c m" "Major mode keys")
  (which-key-add-key-based-replacements
    "C-c o" "Org")
  (which-key-add-key-based-replacements
    "C-c p" "Projectile")
  (which-key-add-key-based-replacements
    "C-c r" "Resume")
  (which-key-add-key-based-replacements
    "C-c s" "Search")
  (which-key-add-key-based-replacements
    "C-c t" "Text")
  (which-key-add-key-based-replacements
    "C-c y" "Snippets")
  (which-key-add-key-based-replacements
    "C-w 5" "Frames")
  (which-key-mode))

(use-package winner
  :bind
  ("C-w u" . winner-undo)
  ("C-w r" . winner-redo)
  :config
  (winner-mode 1))

(use-package windmove
  :bind
  ("C-w C-w" . other-window)
  ("C-w w" . other-window)
  ("C-w i" . windmove-up)
  ("C-w k" . windmove-down)
  ("C-w j" . windmove-left)
  ("C-w l" . windmove-right))

(use-package wgrep)

(use-package yaml-mode)

(use-package yasnippet
  :bind (:map yas-minor-mode-map
         ("C-c &" . nil)
         ("C-c y i" . yas-insert-snippet)
         ("C-c y n" . yas-new-snippet)
         ("C-c y v" . yas-visit-snippet-file)
         ("C-c y s" . yas-insert-snippet)
         ("C-c y v" . yas-visit-snippet-file))
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(use-package zoom-window
  :bind
  ("C-w z" . zoom-window-zoom)
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

(provide 'config-extensions)
