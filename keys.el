(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
;; (define-key [remap backward-delete-char-untabify] 'backward-delete-char global-map)

(global-set-key (kbd "<M-left>") 'backward-word)
(global-set-key (kbd "<M-right>") 'forward-word)

(global-set-key (kbd "C-f") (lambda () (interactive) (forward-word)))
(global-set-key (kbd "C-b") (lambda () (interactive) (backward-word)))
(global-set-key (read-kbd-macro "<M-DEL>") 'backward-delete-word)
(global-set-key (kbd "C-d") 'delete-word)

(global-set-key [home] 'smart-beginning-of-line)
(global-set-key "\C-a" 'smart-beginning-of-line)

(global-set-key (kbd "C-x 2") 'split-window-below-and-switch)
(global-set-key (kbd "C-x 3") 'split-window-right-and-switch)

;; Extensions

;; Helm
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x v") 'helm-projectile)
(global-set-key (kbd "M-s") 'helm-occur)
(global-set-key (kbd "C-s") 'helm-occur)
(global-set-key (kbd "C-S-s") 'helm-projectile-ag)
(global-set-key (kbd "C-S-v") 'helm-show-kill-ring)

;; Magic
(global-set-key (kbd "C-x g s") 'magit-status)

;; Elpy
(define-key elpy-mode-map (kbd "<M-left>") nil)
(define-key elpy-mode-map (kbd "<M-right>") nil)
(define-key elpy-mode-map (kbd "<M-S-left>") 'elpy-nav-indent-shift-left)
(define-key elpy-mode-map (kbd "<M-S-right>") 'elpy-nav-indent-shift-right)
(define-key elpy-mode-map (kbd "M-,") 'pop-tag-mark)
(define-key pyenv-mode-map (kbd "C-c C-s") nil)

;; Other Extensions
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)

;; Custom Emacs Shortcuts
(global-set-key (kbd "C-}") (lambda () (interactive) (forward-line 5)))
(global-set-key (kbd "C-{") (lambda () (interactive) (forward-line -5)))
