(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

(defconst private-dir  (expand-file-name "private" user-emacs-directory))
(defconst temp-dir
  (format "%s/cache" private-dir)
  "Hostname-based elisp temp directories")

;; Core settings
;; UTF-8 please
(set-charset-priority 'unicode)
(setq locale-coding-system   'utf-8)   ; pretty
(set-terminal-coding-system  'utf-8)   ; pretty
(set-keyboard-coding-system  'utf-8)   ; pretty
(set-selection-coding-system 'utf-8)   ; please
(prefer-coding-system        'utf-8)   ; with sugar on top
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)

(setq-default indent-tabs-mode nil
              truncate-lines t)

(setq
 confirm-kill-emacs 'y-or-n-p
 confirm-nonexistent-file-or-buffer  t
 save-interprogram-paste-before-kill t
 mouse-yank-at-point                 t
 require-final-newline              nil
 visible-bell                       nil
 ring-bell-function                 'ignore
 custom-file                        "~/.emacs.d/.custom.el"
 ;; http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
 minibuffer-prompt-properties
 '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)
 ;; persistent bookmarks
 bookmark-save-flag                 t
 bookmark-default-file              (concat temp-dir "/bookmarks")
 ;; Disable backups (that's what git/dropbox are for)
 history-length                     1000
 auto-save-default                  nil
 auto-save-list-file-name           (concat temp-dir "/autosave")
 make-backup-files                  nil
 create-lockfiles                   nil
 backup-directory-alist            `((".*" . ,(concat temp-dir "/backup/")))
 auto-save-file-name-transforms    `((".*" ,(concat temp-dir "/auto-save-list/") t))
 ;; Disable non selected window highlight
 cursor-in-non-selected-windows     nil
 highlight-nonselected-windows      nil
 ;; PATH
 exec-path                          (append exec-path '("/usr/local/bin/"))
 ;; Backups disabled
 backup-inhibited                   t
 make-backup-files                  nil
 indent-tabs-mode                   nil
 inhibit-startup-message            t
 fringes-outside-margins            t
 x-select-enable-clipboard          t
 use-package-always-ensure          t
 vc-follow-symlinks                 t
 auto-revert-check-vc-info          nil
 frame-resize-pixelwise             t)

;; Disable toolbar & menubar
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(setq-default display-line-numbers-current-absolute t
              display-line-numbers-width 1
              display-line-numbers-widen t)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(put 'narrow-to-region 'disabled nil)

;; Env vars
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin/"))
(setenv "SHELL" "/bin/zsh")

(show-paren-mode 1)
(desktop-save-mode 0)

(if (eq system-type 'darwin)
    (set-default-font "-*-Hack-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1" t t)
  ;(set-default-font "Hack 13" t t)
  (set-frame-font "Hack 12" t t))

;; Delete trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package emacs
  :init
  (global-unset-key (kbd "C-w"))
  ;; (global-unset-key (kbd "C-x +"))
  ;; (global-unset-key (kbd "C-x -"))
  ;; (global-unset-key (kbd "C-x 0"))
  ;; (global-unset-key (kbd "C-x 1"))
  ;; (global-unset-key (kbd "C-x 2"))
  ;; (global-unset-key (kbd "C-x 3"))
  ;; (global-unset-key (kbd "C-x 4"))
  ;; (global-unset-key (kbd "C-x 5")
  :bind
  ("C-x C-x" . kill-region)
  ("C-c t +" . text-scale-increase)
  ("C-c t -" . text-scale-decrease)
  ("C-c t =" . text-scale-adjust)
  ("C-c g g" . goto-line)
  ("C-w =" . balance-windows)
  ("C-w -" . shrink-window-if-larger-than-buffer)
  ("C-w 0" . delete-window)
  ("C-w 1" . delete-other-windows)
  ("C-w 2" . delete-window-below-and-switch)
  ("C-w 3" . delete-window-right-and-switch)
  ("C-w 5 0" . delete-frame)
  ("C-w 5 1" . delete-other-frames)
  ("C-w 5 2" . make-frame)
  ("C-w 5 o" . other-frame)
  ("C-w }" . shrink-window-horizontally)
  ("C-w {" . enlarge-window-horizontally))



(provide 'config-base)
;;; core ends here
