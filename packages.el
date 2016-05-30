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
    desktop+
    easy-kill
    elpy
    epc
    epl
    exec-path-from-shell
    expand-region
    flycheck
    flymake-go
    git-gutter+
    go-autocomplete
    go-eldoc
    go-mode
    helm
    helm-projectile
    js2-refactor
    magit
    magit-popup
    neotree
    page-break-lines
    pip-requirements
    projectile
    py-autopep8
    pyenv-mode
    shell-switcher
    smartparens
    spaceline
    spacemacs-theme
    sr-speedbar
    tern
    undo-tree
    which-key
    web-mode
    yaml-mode
    yasnippet))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      packages)
