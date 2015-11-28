(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (spacemacs-dark)))
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476")))
 '(menu-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'powerline)
(require 'helm)
(require 'helm-config)
(require 'jedi)
(require 'sr-speedbar)

(global-linum-mode nil)
(setq sr-speedbar-right-side nil)
(powerline-default-theme)
(projectile-global-mode)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(global-set-key (kbd "M-x") 'helm-M-x)
(helm-mode 1)

(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-to-list 'ac-sources 'ac-source-jedi-direct)

(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'pip-requirements-mode-hook #'pip-requirements-auto-complete-setup)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Custom line number stuff
(setq linum-format "%d  ")
(setq-default left-fringe-width  12)
(setq-default right-fringe-width  0)
(set-face-attribute 'fringe nil)


;; COPY / PASTE on OSX
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

