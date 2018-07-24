(use-package ivy
  :bind
  ("C-x s" . swiper)
  ("C-x C-r" . ivy-resume)
  ("C-x b" . ivy-switch-buffer)
  ("C-x B" . ivy-switch-buffer-other-window)
  :custom
  (ivy-display-style 'fancy)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers nil
        ivy-count-format "%d/%d ")
  (define-key read-expression-map (kbd "C-r")
  'counsel-expression-history))

(use-package ivy-rich
  :after ivy
  :custom
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer))

(use-package ivy-hydra)

(provide 'core-ivy)
