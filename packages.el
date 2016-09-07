

(package-initialize)



(defvar packages
  '(auto-complete
    ace-jump-mode
    dockerfile-mode
    diminish
    exec-path-from-shell
    expand-region
    flycheck
    flymake-go
    git-gutter
    helm
    helm-projectile
    helm-ag
    helm-git-grep
    hl-line
    hlinum
    magit
    magit-popup
    multiple-cursors
    neotree
    page-break-lines
    powerline
    projectile
    py-autopep8
    shell-switcher
    syntax-subword
    smartparens
    undo-tree
    wgrep
    which-key
    web-mode
    yaml-mode
    yasnippet))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      packages)
