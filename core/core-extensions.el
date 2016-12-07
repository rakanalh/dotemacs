(eval-after-load 'grep
  '(progn
    (add-to-list 'grep-find-ignored-directories "local")
    (add-to-list 'grep-find-ignored-directories "build")
    (add-to-list 'grep-find-ignored-directories "media")))
(add-hook 'grep-mode-hook (lambda() (toggle-truncate-lines 1)))

(use-package ace-jump-mode
  :bind
  ("C-c SPC" . ace-jump-mode))


(use-package anzu
  :config
  (global-anzu-mode +1)
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp))

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package counsel
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-m" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-x c k" . counsel-yank-pop))

(use-package counsel-projectile
  :bind
  ("C-x v" . counsel-projectile)
  ("C-x c p" . counsel-projectile-ag)
  :config
  (counsel-projectile-on))

(use-package dockerfile-mode)

(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq-default ediff-highlight-all-diffs 'nil)
  (setq ediff-diff-options "-w"))

(use-package exec-path-from-shell
  :config
  ;; Add GOPATH to shell
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-copy-env "GOPATH")
    (exec-path-from-shell-copy-env "PYTHONPATH")
    (exec-path-from-shell-initialize)))

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

(use-package hungry-delete
  :config
  (global-hungry-delete-mode))

(use-package ivy
  :bind
  ("C-x s" . swiper)
  ("C-x C-r" . ivy-resume)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers nil)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))


(use-package hlinum
  :config
  (hlinum-activate))

(use-package linum
  :config
  (setq linum-format " %3d ")
  (global-linum-mode nil))

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

(use-package persp-mode
  :init
  (persp-mode)
  (setq persp-save-dir (concat private-dir "/persp-confs/")
	persp-auto-save-opt 0)
  :config
  (add-hook 'kill-emacs-hook 'persp/close-perspective)
  :bind
  ("C-x p p" . persp/switch-to-current-branch-persp))

(use-package projectile
  :config
  (setq projectile-known-projects-file
        (expand-file-name "projectile-bookmarks.eld" temp-dir))
  (setq projectile-completion-system 'ivy)
  (projectile-global-mode))

(use-package dashboard
  :config
  (defun dashboard-insert-reddit-list (reddit-list list)
  "Render REDDIT-LIST title and items of LIST."
  (when (car list)
    (insert reddit-list)
    (mapc (lambda (el)
	    (setq url (nth 1 (split-string el "__")) )
	    (setq title (nth 0 (split-string el "__")) )
            (insert "\n    ")
            (widget-create 'push-button
                           :action `(lambda (&rest ignore)
				      (browse-url url))
                           :mouse-face 'highlight
                           :follow-link "\C-m"
                           :button-prefix ""
                           :button-suffix ""
                           :format "%[%t%]"
			   title
			   ))
          list)))


  (defun dashboard-insert-reddits (list-size)
    "Add the list of LIST-SIZE items from recently edited files."
    (if (> list-size 0 )
	(progn
	  (setq file-path "/tmp/dashboard_reddits.json")
	  (condition-case nil
	      (delete-file file-path)
	    (error nil))

	  (require 'json)
	  (url-copy-file "https://www.reddit.com/r/emacs/.json"  file-path)
	  (setq reddit-list (mapcar (lambda (entry)
				      (format "%s__%s " (let-alist entry .data.title ) (let-alist entry .data.url )))
				    (let-alist (json-read-file  file-path) .data.children)))
	  (when (dashboard-insert-reddit-list
		 "Recent Posts to /r/emacs:"
		 (dashboard-subseq reddit-list 0 list-size)))
	  (dashboard-insert--shortcut "p" "Recent Posts:"))
      ))

  (defun dashboard-insert-sx-list (title list)
    "Render SX-LIST title and items of LIST."
    (when (car list)
      (insert title )
      (mapc (lambda (el)
	      (setq link (nth 1 (split-string el "__")) )
	      (setq link-title (nth 0 (split-string el "__")) )
	      (insert "\n    ")
	      (widget-create 'push-button
			     :action `(lambda (&rest ignore)
					(browse-url , link))
			     :mouse-face 'highlight
			     :follow-link "\C-m"
			     :button-prefix ""
			     :button-suffix ""
			     :format "%[%t%]"
			     link-title
			     ))
	    list)))
  (defun dashboard-insert-sx (list-size)
    "Add the list of LIST-SIZE items from recently edited files."
    (if (> list-size 0 )
	(progn

	  (setq stackxc-file-path "/tmp/dashboard_stackxc.json")
	  (condition-case nil
	      (delete-file stackxc-file-path)
	    (error nil))

	  (url-copy-file "https://api.stackexchange.com/2.2/questions?order=desc&sort=activity&site=emacs"  stackxc-file-path)
	  (setq sx-list (mapcar (lambda (entry)
				  (format "views(%s)\t  %s__%s " (let-alist entry .view_count ) (let-alist entry .title ) (let-alist entry .link )))
					;(concat (let-alist entry .data.title ) (concat " - " (let-alist entry .data.url ))))
				(let-alist (json-read-file  stackxc-file-path) .items )))

	  (when (dashboard-insert-sx-list
		 "Recent Items on emacs.stackexchange.com :"
		 (dashboard-subseq sx-list 0 list-size)))
	  (dashboard-insert--shortcut "p" "Recent Stackexchange Emacs items:")
	  )
      ))


  (add-to-list 'dashboard-item-generators  '(stackxc . dashboard-insert-sx))
  (add-to-list 'dashboard-item-generators  '(reddit . dashboard-insert-reddits))
  (setq dashboard-items '((recents  . 5)
			  (bookmarks . 5)
			  (projects . 5)
			  (reddit . 5)
			  (stackxc . 5)))
  (dashboard-setup-startup-hook))

(use-package recentf
  :config
  (setq recentf-exclude '("/elpa/" ;; ignore all files in elpa directory
                          ".*?autoloads.el$"
                          "/tmp/" ;; ignore temporary files
			  )
	recentf-save-file (recentf-expand-file-name "~/.emacs.d/private/cache/recentf"))
  (recentf-mode 1))

(use-package restclient
  :init
  (setq restclient-log-request t
	restclient-same-buffer-response t))

(use-package rotate
  :bind
  ("C-c C-r w" . rotate-window)
  ("C-c C-r l" . rotate-layout))

(use-package smartparens)

(use-package smex)

(use-package syntax-subword
  :config
  (syntax-subword-mode)
  :bind
  ("<M-left>" . syntax-subword-backward)
  ("<M-right>" . syntax-subword-forward))

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
  ("C-x <up>" . windmove-up)
  ("C-x <down>" . windmove-down)
  ("C-x <left>" . windmove-left)
  ("C-x <right>" . windmove-right))

(use-package wgrep)

(use-package yasnippet
  :config
  (yas-global-mode 1))

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
