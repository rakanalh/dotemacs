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
	gdb-show-main t))

(use-package semantic
  :config
  (add-hook 'c-mode-common-hook (lambda ()
	    (semantic-mode 1)
	    ;(global-semanticdb-minor-mode 1)
	    (global-semantic-idle-scheduler-mode 1)
	    (global-semantic-stickyfunc-mode 1))))

(use-package ede
  :config
  ;; Enable EDE only in C/C++
  (add-hook 'c-mode-common-hook (lambda ()
				  (global-ede-mode))))

(use-package ggtags
  :config
  ;; Please note `file-truename' must be used!
  (setenv "GTAGSLIBPATH" (concat "/usr/include"
                               ":"
                               "/usr/local/include"))
  (setenv "MAKEOBJDIRPREFIX" (file-truename "~/.obj/"))
  (add-hook 'c-mode-common-hook
	    (lambda ()
	      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
		(ggtags-mode 1))))

  (dolist (map (list ggtags-mode-map))
    (define-key map (kbd "C-c g s") 'ggtags-find-other-symbol)
    (define-key map (kbd "C-c g h") 'ggtags-view-tag-history)
    (define-key map (kbd "C-c g r") 'ggtags-find-reference)
    (define-key map (kbd "C-c g f") 'ggtags-find-file)
    (define-key map (kbd "C-c g c") 'ggtags-create-tags)
    (define-key map (kbd "C-c g u") 'ggtags-update-tags)
    (define-key map (kbd "M-.")     'ggtags-find-tag-dwim)
    (define-key map (kbd "M-,")     'pop-tag-mark)
    (define-key map (kbd "C-c <")   'ggtags-prev-mark)
    (define-key map (kbd "C-c >")   'ggtags-next-mark)))

;; company-c-headers
(use-package company-c-headers
  :init
  (add-to-list 'company-backends 'company-c-headers))

(defun enable-semantic-shortcuts ()
  (local-set-key (kbd "C-c C-j") 'semantic-ia-fast-jump)
  (local-set-key (kbd "C-c C-s") 'semantic-ia-show-summary))

;; hs-minor-mode for folding source code
(add-hook 'c-mode-common-hook 'hs-minor-mode)
(add-hook 'c-mode-common-hook 'enable-semantic-shortcuts)
(add-hook 'c-mode-hook 'enable-semantic-shortcuts)
(add-hook 'c++-mode-hook 'alexott/cedet-hook)

(provide 'core-c)
