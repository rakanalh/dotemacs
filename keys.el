(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x v") 'helm-projectile)
(global-set-key (kbd "M-s") 'helm-occur)
(global-set-key (kbd "C-s") 'helm-occur)
(global-set-key (kbd "C-x g s") 'magit-status)
(global-set-key (kbd "C-=") 'er/expand-region)

(define-key elpy-mode-map (kbd "<M-left>") nil)
(define-key elpy-mode-map (kbd "<M-right>") nil)
(define-key elpy-mode-map (kbd "<M-S-left>") 'elpy-nav-indent-shift-left)
(define-key elpy-mode-map (kbd "<M-S-right>") 'elpy-nav-indent-shift-right)
(define-key elpy-mode-map (kbd "M-,") 'pop-tag-mark)
(define-key pyenv-mode-map (kbd "C-c C-s") nil)
(global-set-key (kbd "<M-left>") 'backward-word)
(global-set-key (kbd "<M-right>") 'forward-word)

;; Custom Shortcuts
(global-set-key (kbd "C-f") (lambda () (interactive) (forward-word)))
(global-set-key (kbd "C-b") (lambda () (interactive) (backward-word)))
(global-set-key (read-kbd-macro "<M-DEL>") 'backward-delete-word)
(global-set-key (kbd "C-d") 'delete-word)

(global-set-key [home] 'smart-beginning-of-line)
(global-set-key "\C-a" 'smart-beginning-of-line)

(global-set-key (kbd "C-}")
                (lambda () (interactive) (forward-line 5)))
(global-set-key (kbd "C-{")
                (lambda () (interactive) (forward-line -5)))

;; make ctrl-z undo
(global-set-key (kbd "C-z") 'undo)
;; make ctrl-Z redo
(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "C-S-z") 'redo)
