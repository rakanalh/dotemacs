(use-package dired :ensure nil
  :bind (:map dired-mode-map
              ("/" . dired-narrow-regexp))
  :config
  (setq delete-by-moving-to-trash t)
  ;; mark symlinks
  (setq dired-ls-F-marks-symlinks t)
  ;; fix `ls' for macOS.
  (when (memq window-system '(mac ns))
    (setq insert-directory-program "gls" dired-use-ls-dired t))
  ;; Never prompt for recursive copies of a directory
  (setq dired-recursive-copies 'always)
  ;; Never prompt for recursive deletes of a directory
  (setq dired-recursive-deletes 'always)

  ;; makes dired guess the target directory
  (setq dired-dwim-target t)

  ;; Dired listing switches
  ;;  -a : Do not ignore entries starting with .
  ;;  -l : Use long listing format.
  ;;  -G : Do not print group names like 'users'
  ;;  -h : Human-readable sizes like 1K, 234M, ..
  ;;  -v : Do natural sort .. so the file names starting with . will show up first.
  ;;  -F : Classify filenames by appending '*' to executables,
  ;;       '/' to directories, etc.
  ;; default value for dired: "-al"
  (setq dired-listing-switches "-alGhvF --group-directories-first")

  ;; auto-revert dired
  (setq dired-auto-revert-buffer t)

  (defun rag/dired-rename-buffer-name ()
    "Rename the dired buffer name to distinguish it from file buffers.
It added extra strings at the front and back of the default dired buffer name."
    (let ((name (buffer-name)))
      (if (not (string-match "/$" name))
	  (rename-buffer (concat "*Dired* " name "/") t))))

  (add-hook 'dired-mode-hook #'rag/dired-rename-buffer-name)

  ;; filter dired lists by regexp, fuzzy matching or string
  ;; https://github.com/Fuco1/dired-hacks#dired-filter
  (use-package dired-narrow)

  ;; a hydra to sort files in dired easily
  ;; Press `S' to invoke dired-quick-sort hydra
  ;; https://gitlab.com/xuhdev/dired-quick-sort
  (use-package dired-quick-sort
    :config (dired-quick-sort-setup))

  ;; dired-ranger: copy paste like in GUI applications
  ;; https://github.com/Fuco1/dired-hacks#dired-ranger
  ;; (use-package dired-ranger
  ;;   :ensure t
  ;;   :bind (:map dired-mode-map
  ;;               ("C" . dired-ranger-copy)
  ;;               ("R" . dired-ranger-move)
  ;;               ("Y" . dired-ranger-paste)))

  ;; dired-x - to hide uninteresting files in dired
  (use-package dired-x :ensure nil
    :config
    (progn
      (setq dired-omit-verbose nil)
      ;; hide backup, autosave, *.*~ files
      ;; omit mode can be toggled using `C-x M-o' in dired buffer.
      (add-hook 'dired-mode-hook #'dired-omit-mode)
      (setq dired-omit-files
            (concat dired-omit-files "\\|^.DS_STORE$\\|^.projectile$\\|^.git$")))))

(provide 'config-dired)
