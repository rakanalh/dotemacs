(require 'projectile)
(projectile-mode +1)
;; Show more information in ivy-switch-buffer; and only display
;; workgroup-relevant buffers.
(defface ivy-buffer-modified
  '((t (:inherit error :background nil :bold t)))
  "Face used for the 'unsaved' symbol in the mode-line.")

(defun get-root ()
  (let ((current-project (projectile-project-name)))
    (if (not current-project)
	(return "")
      current-project)))

(defun +ivy--get-buffers (&optional buffer-list)
  (let ((min-name 5)
        (min-mode 5)
        (proot (get-root)))
    (mapcar
     (lambda (b) (format (format "%%-%ds %%-%ds %%s" min-name min-mode)
                    (nth 0 b)
                    (nth 1 b)
                    (or (nth 2 b) "")))
     (mapcar (lambda (b)
               (with-current-buffer b
                 (let ((buffer-name (buffer-name b))
                       (mode-name (symbol-name major-mode)))
                   (when (> (length buffer-name) min-name)
                     (setq min-name (+ (length buffer-name) 15)))
                   (when (> (length mode-name) min-mode)
                     (setq min-mode (+ (length mode-name) 3)))
                   (list (concat
                          (propertize buffer-name
                                      'face (cond ((string-match-p "^ ?\\*" buffer-name)
                                                   'font-lock-comment-face)
                                                  ((not (string= proot (get-root)))
                                                   'font-lock-keyword-face)
                                                  (buffer-read-only
                                                   'error)))
                          (when (and buffer-file-name (buffer-modified-p))
                            (propertize "[+]" 'face 'ivy-buffer-modified)))
                         (propertize mode-name 'face 'font-lock-constant-face)
                         (when buffer-file-name
                           (abbreviate-file-name (file-name-directory buffer-file-name)))))))
             buffer-list))))

(defun +ivy--select-buffer-action (buffer)
  (ivy--switch-buffer-action
   (s-chop-suffix
    "[+]"
    (substring buffer 0 (string-match-p (regexp-quote "   ") buffer)))))

(defun +ivy--select-buffer-other-window-action (buffer)
  (ivy--switch-buffer-other-window-action
   (s-chop-suffix
    "[+]"
    (substring buffer 0 (string-match-p (regexp-quote "   ") buffer)))))

;;;###autoload
(defun +ivy/switch-workspace-buffer (&optional other-window-p)
  "Switch to an open buffer in the current workspace."
  (interactive "P")
  (+ivy/switch-buffer other-window-p t))

;;;###autoload
(defun +ivy/switch-buffer (&optional other-window-p workspace-only-p)
  "Switch to an open buffer in the global buffer list. If WORKSPACE-ONLY-P,
limit to buffers in the current workspace."
  (interactive "P")
  (ivy-read "Open buffers: "
            (+ivy--get-buffers (unless workspace-only-p (buffer-list)))
            :action (if other-window-p
                        '+ivy--select-buffer-other-window-action
                      '+ivy--select-buffer-action)
            :matcher 'ivy--switch-buffer-matcher
            :keymap ivy-switch-buffer-map
            :caller '+ivy/switch-workspace-buffer
	    :preselect 1))

(use-package ivy
  :bind
  ("C-x s" . swiper)
  ("C-x C-r" . ivy-resume)
  ("C-x b" . +ivy/switch-buffer)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers nil)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))

(provide 'core-ivy)
