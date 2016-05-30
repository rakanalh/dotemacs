;;; Package --- Summary
;;; Commentary:
;; Spacemacs startup screen

;;; Code:
;; Custom splash screen
(defvar spacemacs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab] 'widget-forward)
    (define-key map (kbd "C-i") 'widget-forward)
    (define-key map [backtab] 'widget-backward)
    (define-key map (kbd "RET") 'widget-button-press)
    (define-key map [down-mouse-1] 'widget-button-click)
    map)
  "Keymap for spacemacs mode.")

(define-derived-mode spacemacs-mode special-mode "Spacemacs"
  "Spacemacs major mode for startup screen.
\\<spacemacs-mode-map>
"
  :group 'spacemacs
  :syntax-table nil
  :abbrev-table nil
  (setq truncate-lines t))

(defun spacemacs/insert-startupify-lists ()
  (interactive)
  (with-current-buffer (get-buffer-create "*spacemacs*")
    (let ((buffer-read-only nil)
          (list-separator "\n\n"))
      (goto-char (point-max))
      (page-break-lines-mode)
      (spacemacs/insert-page-break)

      (recentf-mode)
      (when (spacemacs//insert-file-list "Recent Files:" (recentf-elements 5))
	(spacemacs//insert--shortcut "r" "Recent Files:")
	(insert list-separator))

      (helm-mode)
      (when (spacemacs//insert-bookmark-list "Bookmarks:" (bookmark-all-names))
	(spacemacs//insert--shortcut "m" "Bookmarks:")
	(insert list-separator))

      (projectile-mode)
      (when (spacemacs//insert-project-list "Projects:" (projectile-relevant-known-projects))
	(spacemacs//insert--shortcut "p" "Projects:")
	(insert list-separator)))
    (spacemacs-mode)))

(defun spacemacs//insert-file-list (list-display-name list)
  (when (car list)
    (insert list-display-name)
    (mapc (lambda (el)
            (insert "\n    ")
            (widget-create 'push-button
                           :action `(lambda (&rest ignore) (find-file-existing ,el))
                           :mouse-face 'highlight
                           :follow-link "\C-m"
                           :button-prefix ""
                           :button-suffix ""
                           :format "%[%t%]"
                           (abbreviate-file-name el)))
          list)))

(defun spacemacs//insert-project-list (list-display-name list)
  (when (car list)
    (insert list-display-name)
    (mapc (lambda (el)
            (insert "\n    ")
            (widget-create 'push-button
                           :action `(lambda (&rest ignore) (projectile-switch-project-by-name ,el))
                           :mouse-face 'highlight
                           :follow-link "\C-m"
                           :button-prefix ""
                           :button-suffix ""
                           :format "%[%t%]"
                           (abbreviate-file-name el)))
          list)))

(defun spacemacs//insert-bookmark-list (list-display-name list)
  (when (car list)
    (insert list-display-name)
    (mapc (lambda (el)
            (insert "\n    ")
            (widget-create 'push-button
                           :action `(lambda (&rest ignore) (bookmark-jump ,el))
                           :mouse-face 'highlight
                           :follow-link "\C-m"
                           :button-prefix ""
                           :button-suffix ""
                           :format "%[%t%]"
                           (format "%s - %s" el (abbreviate-file-name
                                                 (bookmark-get-filename el)))))
          list)))

(defun spacemacs/insert-page-break ()
  "Insert a page break line in spacemacs buffer."
  (spacemacs/append "\n\n\n"))

(defun spacemacs/append (msg &optional messagebuf)
  "Append MSG to spacemacs buffer. If MESSAGEBUF is not nil then MSG is also written in message buffer."
  (with-current-buffer (get-buffer-create "*spacemacs*")
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (insert msg)
      (if messagebuf (message "(My Emacs) %s" msg)))))

(defmacro spacemacs//insert--shortcut (shortcut-char search-label &optional no-next-line)
  `(define-key spacemacs-mode-map ,shortcut-char (lambda ()
			       (interactive)
			       (unless (search-forward ,search-label (point-max) t)
				 (search-backward ,search-label (point-min) t))
			       ,@(unless no-next-line
				   '((forward-line 1)))
			       (back-to-indentation))))


(defun spacemacs/goto-link-line ()
  "Move the point to the beginning of the link line."
  (interactive)
  (with-current-buffer "*spacemacs*"
    (goto-char (point-min))
    (re-search-forward "Homepage")
    (beginning-of-line)
    (widget-forward 1)))

(defun spacemacs/setup-startup-hook ()
  "Add post init processing."
  (add-hook
   'emacs-startup-hook
   (lambda ()
     ;; Display useful lists of items
     (spacemacs/insert-startupify-lists))
   (redisplay))
  (add-hook 'after-init-hook '(lambda () (switch-to-buffer "*spacemacs*"))))

(provide 'spacemacs-startup)
;;; spacemacs-startup ends here
