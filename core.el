(defconst private-dir  (eval-when-compile (expand-file-name "private" user-emacs-directory)))
(defconst temp-dir
  (eval-when-compile (format "%s/cache/%s" private-dir (system-name)))
  "Hostname-based elisp temp directories")

;; UTF-8 please
(set-charset-priority 'unicode)
(setq locale-coding-system   'utf-8)   ; pretty
(set-terminal-coding-system  'utf-8)   ; pretty
(set-keyboard-coding-system  'utf-8)   ; pretty
(set-selection-coding-system 'utf-8)   ; please
(prefer-coding-system        'utf-8)   ; with sugar on top
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; Core settings
(setq byte-compile-warnings              nil
      ad-redefinition-action            'accept      ; silence the advised function warnings
      apropos-do-all                     t
      compilation-always-kill            t           ; kill compl. process before spawning another
      compilation-ask-about-save         nil         ; save all buffers before compiling
      compilation-scroll-output          t           ; scroll with output while compiling
      confirm-nonexistent-file-or-buffer t
      delete-by-moving-to-trash          t
      echo-keystrokes                    0.02        ; show me what I type
      ediff-diff-options                 "-w"
      ediff-split-window-function       'split-window-horizontally  ; side-by-side diffs
      ediff-window-setup-function       'ediff-setup-windows-plain  ; no extra frames
      enable-recursive-minibuffers       nil         ; no minibufferception
      idle-update-delay                  5           ; update a little less often
      major-mode                        'text-mode
      ring-bell-function                'ignore      ; silence of the bells!
      save-interprogram-paste-before-kill t
      mouse-yank-at-point                 t
      sentence-end-double-space          nil
      require-final-newline              nil
      visible-bell                       nil
      load-prefer-newer                  t
      custom-file                        "~/.emacs.d/.custom.el"
      ;; http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
      minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)
      ;; persistent bookmarks
      bookmark-save-flag                 t
      bookmark-default-file              (concat temp-dir "/bookmarks")
      ;; Disable backups (that's what git/dropbox are for)
      history-length                     1000
      vc-make-backup-files               nil
      auto-save-default                  nil
      auto-save-list-file-name           (concat temp-dir "/autosave")
      make-backup-files                  nil
      create-lockfiles                   nil
      backup-directory-alist            `((".*" . ,(concat temp-dir "/backup/")))
      ;; Remember undo history
      undo-tree-auto-save-history        nil
      undo-tree-history-directory-alist `(("." . ,(concat temp-dir "/undo/")))
      ;; Disable non selected window highlight
      cursor-in-non-selected-windows     nil
      highlight-nonselected-windows      nil
      ;; PATH
      exec-path                          (append exec-path '("/usr/local/bin/"))
      ;; Backups disabled
      backup-inhibited                   t
      make-backup-files                  nil
      warning-minimum-level              :emergency
      indent-tabs-mode                   nil
      inhibit-startup-message            t
      fringes-outside-margins            t
      x-select-enable-clipboard          t)

(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)

;; Disable toolbar & menubar
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (  fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Env vars
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin/:$GOPATH"))
(setenv "SHELL" "/bin/bash")

(show-paren-mode 1)
(desktop-save-mode 0)

;; Delete trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)


(provide 'core)
