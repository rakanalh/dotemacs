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

(use-package cmake-ide
  :config
  (add-hook 'c++-mode-hook '(lambda()
                              (cmake-ide-setup))))

;; (use-package semantic
;;   :config
;;   (semanticdb-enable-gnu-global-databases 'c-mode t)
;;   (semanticdb-enable-gnu-global-databases 'c++-mode t)

;;   (defun my-inhibit-semantic-p ()
;;     (not (equal major-mode 'python-mode)))

;;   (add-to-list 'semantic-inhibit-functions #'my-inhibit-semantic-p)

;;   (setq semanticdb-default-save-directory (expand-file-name "semanticdb/" temp-dir))

;;   (let ((semantic-submodes '(global-semantic-decoration-mode
;; 			     global-semantic-idle-local-symbol-highlight-mode
;; 			     global-semantic-highlight-func-mode
;; 			     global-semanticdb-minor-mode
;; 			     global-semantic-mru-bookmark-mode
;; 			     global-semantic-idle-summary-mode
;; 			     global-semantic-stickyfunc-mode
;; 			     )))
;;     (setq semantic-default-submodes (append semantic-default-submodes semantic-submodes)
;; 	  semantic-idle-scheduler-idle-time 1))

;;   (add-hook 'c-mode-common-hook (lambda () (semantic-mode 1))))

(use-package irony
  :config
  ;; replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's function
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package irony-eldoc
  :config
  (add-hook 'irony-mode-hook 'irony-eldoc))

(use-package company-irony
  :config
  (add-to-list 'company-backends 'company-keywords)
  (add-to-list 'company-backends 'company-irony))

(use-package company-irony-c-headers
  :config
  (add-to-list 'company-backends 'company-irony-c-headers))


(use-package flycheck-irony
  :after flycheck-mode
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; company-c-headers
(use-package company-c-headers
  :config
  (add-to-list 'company-backends 'company-c-headers))

(defun me/rtags ()
  "Rtags configuration.
Used only for nevigation."
  (interactive)
  (rtags-start-process-unless-running)
  (setq rtags-display-result-backend 'ivy)
  (add-hook 'kill-emacs-hook 'rtags-quit-rdm))

(use-package rtags
  :commands rtags-start-process-unless-running
  :bind (("M-."     .  rtags-find-symbol-at-point)
         ("M-?"     .  rtags-find-references-at-point)
         ("M-,"     .  rtags-location-stack-back)
         ("C-,"   .    rtags-location-stack-forward)
         ("C-c r r" .  rtags-rename-symbolrtags-next-match))
  :config
  (setq rtags-completions-enabled t)
  (add-hook 'c++-mode 'me/rtags)
  (add-hook 'c-mode 'me/rtags)
  (message "Rtags loaded")
  (use-package company-rtags))

(use-package clang-format
  :config
  (defun c-mode-before-save-hook ()
    (when (or (eq major-mode 'c++-mode) (eq major-mode 'c-mode))
      (call-interactively 'clang-format)))

  (add-hook 'before-save-hook #'c-mode-before-save-hook))




(provide 'core-c)
