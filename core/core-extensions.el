
(eval-after-load 'grep
  '(progn
    (add-to-list 'grep-find-ignored-directories "local")
    (add-to-list 'grep-find-ignored-directories "build")
    (add-to-list 'grep-find-ignored-directories "media")))
(add-hook 'grep-mode-hook (lambda() (toggle-truncate-lines 1)))

(use-package ag)

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
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-statistics
  :config
  (company-statistics-mode))

(use-package counsel
  :config
  (setq counsel-find-file-ignore-regexp ".*\.egg-info\\|__pycache__\\|.cache")
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
  ("C-x c p" . counsel-projectile-ag)
  :config
  (counsel-projectile-mode))

(use-package dired-subtree
  :config
  (define-key dired-mode-map "i" 'dired-subtree-insert)
  (define-key dired-mode-map ";" 'dired-subtree-remove))

(use-package dockerfile-mode)

(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq-default ediff-highlight-all-diffs 'nil)
  (setq ediff-diff-options "-w"))

(use-package elfeed
  :bind
  ("C-x w" . elfeed)
  :config
  (setq elfeed-feeds
      '("http://nullprogram.com/feed/"
        "http://planet.emacsen.org/atom.xml"
        "https://hnrss.org/frontpage"))
  ;; Entries older than 2 weeks are marked as read
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :before "2 weeks ago"
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

  (defun me/flycheck ()
    "Configurate flycheck."
    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*Flycheck errors*" eos)
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (side            . bottom)
                   (reusable-frames . visible)
                   (window-height   . 0.23)))
    (setq flycheck-display-errors-function
          #'flycheck-display-error-messages-unless-error-list))
  ;; Enable flycheck
  (add-hook 'prog-mode-hook 'me/flycheck)
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package git-gutter)

(use-package hideshow
  :bind (("C-\\" . hs-toggle-hiding)
         ("M-+" . hs-show-all))
  :init (add-hook #'prog-mode-hook #'hs-minor-mode)
  :diminish hs-minor-mode
  :config
  (setq hs-special-modes-alist
        (mapcar 'purecopy
                '((c-mode "{" "}" "/[*/]" nil nil)
                  (c++-mode "{" "}" "/[*/]" nil nil)
                  (java-mode "{" "}" "/[*/]" nil nil)
                  (js-mode "{" "}" "/[*/]" nil)
                  (json-mode "{" "}" "/[*/]" nil)
                  (javascript-mode  "{" "}" "/[*/]" nil)))))

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
  :config
  (setq imenu-list-focus-after-activation t
        imenu-list-size 0.2
        imenu-list-auto-resize nil)
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

;; (use-package hlinum
;;   :config
;;   (hlinum-activate))

;; (use-package linum
;;   :config
;;   (setq linum-format " %3d ")
;;   (global-linum-mode nil))

(use-package magit
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  :bind
  ;; Magic
  ("C-x g s" . magit-status)
  ("C-x g x" . magit-checkout)
  ("C-x g c" . magit-commit)
  ("C-x g p" . magit-push)
  ("C-x g u" . magit-pull)
  ("C-x g e" . magit-ediff-resolve)
  ("C-x g r" . magit-rebase-interactive))

(use-package magit-popup)

(use-package markdown-mode)

(use-package multiple-cursors
  :config
  (setq mc/list-file (concat temp-dir "/.mc-lists.el"))
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C->" . mc/mark-all-like-this)
  ("C-c ;" . mc/skip-to-next-like-this))

(use-package neotree
  :config
  (setq neo-theme 'arrow
        neotree-smart-optn t
        neo-window-fixed-size nil)
  (neotree-projectile-action)
  ;; Disable linum for neotree
  (add-hook 'neo-after-create-hook 'disable-neotree-hook)
  :bind
  ("C-x C-t" . neotree-toggle))

(use-package org
  :config
  (setq org-directory "~/DropBox/org-mode"
        org-agenda-files (list "~/Google Drive/org-mode/ideas.org"
                               "~/Google Drive/org-mode/calendar.org"
                               "~/Google Drive/org-mode/learning.org")
        org-default-notes-file (concat org-directory "/todo.org")
        org-confirm-babel-evaluate nil
        org-src-fontify-natively t)


  (setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/mygtd.org" "Tasks")
         "* TODO %?\nAdded: %U\n" :prepend t :kill-buffer t)
        ("w" "Web" entry (file+headline "~/www/org/index.org" "Tasks")
         "* TODO %?\nAdded: %U\n" :prepend t :kill-buffer t)
        ("r" "Prog. R" entry (file+headline "~/www/org/teaching/introR.org" "Tasks")
         "* TODO %?\nAdded: %U\n" :prepend t :kill-buffer t)
        ("i" "Idea" entry (file+headline "~/org/mygtd.org" "Someday/Maybe")
         "* IDEA %?\nAdded: %U\n" :prepend t :kill-buffer t)
        ("h" "Home" entry (file+headline "~/org/mygtd.org" "Home")
         "* TODO %?\nAdded: %U\n" :prepend t :kill-buffer t)
        )
      )

  (org-babel-do-load-languages
   'org-babel-load-languages '((python . t)
                               (shell . t)
                               (emacs-lisp . t)
                               (lisp . t)))
  (add-hook 'org-finalize-agenda-hook (lambda ()
                                        (setq org-agenda-tags-column (- 6 (window-width)))
                                        (org-agenda-align-tags)))
  :bind
  ("\C-cl" . org-store-link)
  ("\C-ca" . org-agenda))


(use-package org-alert
  :config
  (if (memq window-system '(mac ns))
      (setq alert-default-style 'osx-notifier)
    (setq alert-default-style 'libnotify)))

(use-package org-projectile
  :after org-mode
  :config
  (org-projectile-per-project)
  (setq org-projectile-per-project-filepath "notes.org"
        org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
  :bind
  ("C-c c" . org-projectile-capture-for-current-project))

(use-package org-bullets
  :config
  (setq org-hide-leading-stars t)
  (add-hook 'org-mode-hook
            (lambda ()
              (org-bullets-mode t))))

(use-package page-break-lines)

(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default))

(use-package projectile
  :config
  (setq projectile-enable-caching t
        projectile-cache-file (expand-file-name "projectile.cache" temp-dir)
        projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" temp-dir))
  (setq projectile-completion-system 'ivy)
  (projectile-global-mode)
  :bind
  ("C-x c a" . projectile-ag))

(use-package dashboard
  :config
  (setq dashboard-items '((agenda . 10)
                          (recents  . 5)
                          (projects . 5)
                          (bookmarks . 15)
                          (registers . 10)))
  (dashboard-setup-startup-hook))
  ;(setq dashboard-banner-logo-title (format "Loaded in %.02fs" loading-time)))

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
                          )
        recentf-save-file (recentf-expand-file-name "~/.emacs.d/private/cache/recentf"))
  (recentf-mode 1))

(use-package resize-window
  :bind
  ("C-x /" . resize-window))

(use-package restclient
  :init
  (setq restclient-log-request t
        restclient-same-buffer-response t))

(use-package rotate
  :bind
  ("C-c C-r w" . rotate-window)
  ("C-c C-r l" . rotate-layout))

(use-package smartparens)

(use-package smex
  :config
  (setq smex-save-file (expand-file-name "smex-items" temp-dir)))

(use-package syntax-subword
  :config
  (syntax-subword-mode)
  :bind
  ("M-j" . syntax-subword-backward)
  ("M-l" . syntax-subword-forward))

(use-package undo-tree
  :config
  (global-undo-tree-mode 1)
  ;; Remember undo history
  (setq undo-tree-auto-save-history        nil
        undo-tree-history-directory-alist `(("." . ,(concat temp-dir "/undo/")))))

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
  (yas-global-mode 1)
  (setq yas-snippet-dirs (append yas-snippet-dirs
                                 '("~/.emacs.d/snippets"))))

(use-package zoom-window
  :bind
  ("C-x C-z" . zoom-window-zoom)
  :config
  (setq zoom-window-mode-line-color "#22252c"))

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
