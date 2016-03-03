(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x v") 'helm-projectile)
(global-set-key (kbd "C-x g s") 'magit-status)

(global-set-key map (kbd "<M-left>") 'backward-word)
(global-set-key map (kbd "<M-right>") 'forward-word)

(global-set-key [home] 'smart-beginning-of-line)
(global-set-key "\C-a" 'smart-beginning-of-line)
