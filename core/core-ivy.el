(use-package ivy
  :bind
  ("C-s" . swiper)
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

(use-package ivy-pass
  :after ivy
  :commands ivy-pass)

(use-package ivy-rich
  :after ivy
  :config
  (defun ivy-rich-switch-buffer-icon (candidate)
    (with-current-buffer
   	(get-buffer candidate)
      (let ((icon (all-the-icons-icon-for-mode major-mode)))
	(if (symbolp icon)
	    (all-the-icons-icon-for-mode 'fundamental-mode)
	  icon))))
  (setq ivy-rich--display-transformers-list
      '(ivy-switch-buffer
        (:columns
         ((ivy-rich-switch-buffer-icon :width 2)
          (ivy-rich-candidate (:width 30))
          (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
          (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
          (ivy-rich-switch-buffer-project (:width 15 :face success))
          (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
         :predicate
         (lambda (cand) (get-buffer cand)))))
  (ivy-rich-mode 1))

(use-package ivy-hydra)

(use-package ivy-xref
  :ensure t
  :init (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(provide 'core-ivy)
