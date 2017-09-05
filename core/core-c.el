;; C-IDE based on https://github.com/tuhdo/emacs-c-ide-demo
(use-package cc-mode
  :config
  ;; Available C style:
  ;; "gnu": The default style for GNU projects
  ;; "k&r": What Kernighan and Ritchie, the authors of C used in their book
  ;; "bsd": What BSD developers use, aka "Allman style" after Eric Allman.
  ;; "whitesmith": Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
  ;; "stroustrup": What Stroustrup, the author of C++ used in his book
  ;; "ellemtel": Popular C++ coding standards as defined by "Programming in C++, Rules and Recommendations," Erik Nyquist and Mats Henricson, Ellemtel
  ;; "linux": What the Linux developers use for kernel development
  ;; "python": What Python developers use for extension modules
  ;; "java": The default style for java-mode (see below)
  ;; "user": When you want to define your own style
  (setq c-default-style "linux") ;; set style to "linux"
  (setq gdb-many-windows t ;; use gdb-many-windows by default
	gdb-show-main t
	c-basic-offset 4
	tab-width 4
	indent-tabs-mode t))

(use-package semantic
  :config
  (setq semanticdb-default-save-directory (expand-file-name "semanticdb/" temp-dir))
  (add-hook 'c-mode-common-hook (lambda ()
	    (semantic-mode 1)
	    ;(global-semanticdb-minor-mode 1)
	    (global-semantic-idle-scheduler-mode 1)
	    (global-semantic-stickyfunc-mode 1))))

(use-package ggtags
  :config
  (add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1)))))

(use-package irony
  :config
  ;; replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's function
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package company-irony
  :after company-mode
  :config
  (add-to-list 'company-backends 'company-irony))

(use-package company-irony-c-headers
  :after company-mode
  :config
  (add-to-list
    'company-backends '(company-irony-c-headers company-irony)))


(use-package flycheck-irony
  :after flycheck-mode
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; company-c-headers
(use-package company-c-headers
  :config
  (add-to-list 'company-backends 'company-c-headers))

(defun enable-semantic-shortcuts ()
  (local-set-key (kbd "C-c C-j") 'semantic-ia-fast-jump)
  (local-set-key (kbd "C-c C-s") 'semantic-ia-show-summary))

;; hs-minor-mode for folding source code
(add-hook 'c-mode-common-hook 'hs-minor-mode)
(add-hook 'c-mode-common-hook 'enable-semantic-shortcuts)
(add-hook 'c-mode-hook 'enable-semantic-shortcuts)
(add-hook 'c++-mode-hook 'alexott/cedet-hook)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)

(provide 'core-c)
