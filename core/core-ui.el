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
  (load-theme 'doom-one t)
  (let ((c '((class color) (min-colors 89)))
      (black          "#181e26")
      (white          "#DFDFDF")
      (blue           "#51afef"))

    (custom-theme-set-faces
     'doom-one
     ;; Doom faces
     `(show-paren-match          ((,c (:foreground ,black :background ,white))))
     ;; Ivy
     `(ivy-current-match         ((,c (:background ,blue))))
     ;; org-mode
     `(org-level-2               ((,c (:foreground ,blue))))
     `(org-level-3               ((,c (:foreground ,white))))
     `(org-level-4               ((,c (:foreground ,white))))
     `(org-level-5               ((,c (:foreground ,white))))
     `(org-level-6               ((,c (:foreground ,white))))))
  ;(require 'doom-neotree)
  (doom-themes-neotree-config)  ; all-the-icons fonts must be installed!
  (add-hook 'find-file-hook 'doom-buffer-mode))

(use-package solaire-mode
  :config
  ;; brighten buffers (that represent real files)
  (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)

  ;; You can do similar with the minibuffer when it is activated:
  (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)

  ;; To enable solaire-mode unconditionally for certain modes:
  (add-hook 'ediff-prepare-buffer-hook #'solaire-mode))

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
(set-face-foreground 'linum-highlight-face "#00B3EF")
(set-face-background 'linum-highlight-face "#1f252b")

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
