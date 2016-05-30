(setq multi-term-program "/bin/bash")
(setq term-buffer-maximum-size 10000)
(setq show-trailing-whitespace nil)
(setq comint-prompt-read-only t)

(setq explicit-shell-file-name "/bin/bash")

(add-hook 'term-mode-hook (lambda()
                            (setq yas-dont-activate t)))
