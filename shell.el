(require 'shell-switcher)

(setq explicit-shell-file-name "/bin/bash")
(setq shell-switcher-mode t)
(setq multi-term-program "/bin/bash")
(setq system-uses-terminfo nil)

(defun eshell-clear-buffer ()
  "Clear terminal"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))
(add-hook 'eshell-mode-hook
      '(lambda()
          (local-set-key (kbd "C-l") 'eshell-clear-buffer)))

(defun shell-clear()
  "Clear a shell buffer."
  (interactive)
  (when (equal mode-name "Shell")
    (delete-region (point-min) (point-max))
    (call-interactively 'comint-send-input)))
