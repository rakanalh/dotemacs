
;;; package --- Main init file
;;; Commentary:
;;; This is my init file

;;; Code:
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(add-to-list 'load-path "~/.emacs.d/vendor/")

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar packages
  '(auto-complete
    dockerfile-mode
    elpy
    epc
    epl
    exec-path-from-shell
    flycheck
    flymake-go
    git-gutter+
    go-autocomplete
    go-eldoc
    go-mode
    helm
    helm-projectile
    magit
    magit-popup
    neotree
    page-break-lines
    pip-requirements
    projectile
    py-autopep8
    pyenv-mode
    shell-switcher
    spaceline
    spacemacs-theme
    sr-speedbar
    which-key
    yaml-mode
    yasnippet))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      packages)

(if window-system
      (custom-set-variables
       ;; custom-set-variables was added by Custom.
       ;; If you edit it by hand, you could mess it up, so be careful.
       ;; Your init file should contain only one such instance.
       ;; If there is more than one, they won't work right.
       '(custom-enabled-themes (quote (spacemacs-dark)))
       '(custom-safe-themes
         (quote
          ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476")))
       '(menu-bar-mode nil)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(widget-button ((t (:foreground "gray90" :underline nil :weight bold)))))

(require 'elpy)
(require 'helm)
(require 'helm-config)
(require 'go-autocomplete)
(require 'go-eldoc)
(require 'auto-complete-config)
(require 'recentf)
(require 'sr-speedbar)
(require 'spaceline-config)
(require 'which-key)
(require 'py-autopep8)
(require 'pyenv-mode)
(require 'shell-switcher)

;; Disable toolbar & menubar
(menu-bar-mode -1)
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-initialize))

(elpy-enable)
(pyenv-mode)
(add-to-list 'exec-path "~/.pyenv/shims")
(setq elpy-rpc-backend "jedi")

(ac-config-default)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(global-git-gutter+-mode)
(global-linum-mode nil)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x v") 'helm-projectile)
(global-set-key (kbd "C-x g s") 'magit-status)
(helm-mode 1)
(projectile-global-mode)
(recentf-mode 1)
(spaceline-emacs-theme)
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin/"))
(setq exec-path (append exec-path '("/usr/local/bin/")))
(setq backup-inhibited t)
(setq auto-save-default nil)
(setq inhibit-startup-message t)
(setq sr-speedbar-right-side nil)
(setq speedbar-smart-directory-expand-flag t)
(setq make-backup-files nil)
;(setq flycheck-python-flake8-executable "/usr/local/bin/flake8")
(setq explicit-shell-file-name "/bin/bash")
(setq shell-switcher-mode t)
(setq-default indent-tabs-mode nil)
(setq x-select-enable-clipboard t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t
      require-final-newline t
      visible-bell t
      load-prefer-newer t
      backup-directory-alist `(("." . ,(concat user-emacs-directory
					       "backups"))))
(show-paren-mode 1)
(which-key-mode)

(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))

(add-hook 'go-mode-hook 'go-eldoc-setup)
;; (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
(add-hook 'pip-requirements-mode-hook #'pip-requirements-auto-complete-setup)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Custom line number stuff
(setq linum-format 'dynamic)
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

;; Custom Shortcuts
(global-set-key (kbd "C-}")
                (lambda () (interactive) (forward-line 5)))
(global-set-key (kbd "C-{")
                (lambda () (interactive) (forward-line -5)))

;; Go Stuff
(defun my-go-mode-hook ()
  ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'my-go-mode-hook)

(require 'spacemacs-startup)
(spacemacs/setup-startup-hook)

(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))
(global-set-key [home] 'smart-beginning-of-line)
(global-set-key "\C-a" 'smart-beginning-of-line)

;; Delete trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(yas-global-mode 1)

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

(defun ssbb-pyenv-hook ()
"Automatically activates pyenv version if .python-version file exists."
(f-traverse-upwards
 (lambda (path)
    (let ((pyenv-version-path (f-expand ".python-version" path)))
     (if (f-exists? pyenv-version-path)
         (progn
           (setq python-current-version (s-trim (f-read-text pyenv-version-path 'utf-8)))
           (pyenv-mode-set python-current-version)
           (setenv "WORKON_HOME" (concat "~/.pyenv/versions/" python-current-version "/envs"))
           (message (concat "Setting virtualenv path to ~/.pyenv/versions/" python-current-version "/envs"))))))))

(add-hook 'find-file-hook 'ssbb-pyenv-hook)

(provide 'init)
;;; init.el ends here
