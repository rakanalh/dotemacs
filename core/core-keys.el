(setq ns-function-modifier 'control)
;; Set global keys not specific to a certain package
(global-set-key (kbd "C-f") (lambda () (interactive) (forward-word)))
(global-set-key (kbd "C-b") (lambda () (interactive) (backward-word)))
(global-set-key (read-kbd-macro "<M-DEL>") 'backward-delete-word)
(global-set-key (kbd "C-d") 'delete-word)

(global-set-key [home] 'smart-beginning-of-line)
(global-set-key "\C-a" 'smart-beginning-of-line)

(global-set-key (kbd "C-x 2") 'split-window-below-and-switch)
(global-set-key (kbd "C-x 3") 'split-window-right-and-switch)

(global-set-key (kbd "C-x C-b") 'switch-to-buffer)

;; Custom Emacs Shortcuts
(global-set-key (kbd "M-<") (lambda () (interactive) (beginning-of-buffer-record)))
(global-set-key (kbd "M->") (lambda () (interactive) (end-of-buffer-record)))
(global-set-key (kbd "M-/") (lambda () (interactive) (go-back-to-point)))
(global-set-key (kbd "C-}") (lambda () (interactive) (forward-line 5)))
(global-set-key (kbd "C-{") (lambda () (interactive) (forward-line -5)))

;; iTerm2
(global-set-key (kbd "C-'") 'iterm-focus)

;; rgrep
(global-set-key (kbd "C-x c g") (lambda () (interactive) (call-interactively 'rgrep)))
(define-key ctl-x-r-map "b" 'bookmark-jump-or-find-file)

(provide 'core-keys)
