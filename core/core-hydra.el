(use-package hydra
  :defer 0.5
  :bind (
         ("C-c h p" . hydra-projectile/body)
         ("C-c h b" . hydra-buffer/body)
         ("C-c h f" . hydra-flycheck/body)
         ("C-c h m" . hydra-magit/body)
         ("C-c h o" . hydra-org/body)
         ("C-c h y" . hydra-yasnippet/body)
         ("C-c h w" . hydra-windows/body)))


(defhydra hydra-buffer (:color blue)
  "
  ^
  ^Buffer^             ^Do^
  ^──────^─────────────^──^──────────
  _q_ quit             _k_ kill
  ^^                   _l_ list
  ^^                   _n_ next
  ^^                   _p_ previous
  ^^                   ^^
  "
  ("q" nil)
  ("k" kill-buffer)
  ("l" ibuffer)
  ("n" next-buffer)
  ("p" previous-buffer))


(defhydra hydra-magit (:color blue)
  "
  ^
  ^Magit^             ^Do^
  ^─────^─────────────^──^────────
  _q_ quit            _b_ blame
  ^^                  _c_ clone
  ^^                  _i_ init
  ^^                  _s_ status
  ^^                  ^^
  "
  ("q" nil)
  ("b" magit-blame)
  ("c" magit-clone)
  ("i" magit-init)
  ("s" magit-status))


(defhydra hydra-flycheck (:color blue)
  "
  ^
  ^Flycheck^          ^Errors^            ^Checker^
  ^────────^──────────^──────^────────────^───────^─────
  _q_ quit            _<_ previous        _?_ describe
  _M_ manual          _>_ next            _d_ disable
  _v_ verify setup    _f_ check           _m_ mode
  ^^                  _l_ list            _s_ select
  ^^                  ^^                  ^^
  "
  ("q" nil)
  ("<" flycheck-previous-error :color pink)
  (">" flycheck-next-error :color pink)
  ("?" flycheck-describe-checker)
  ("M" flycheck-manual)
  ("d" flycheck-disable-checker)
  ("f" flycheck-buffer)
  ("l" flycheck-list-errors)
  ("m" flycheck-mode)
  ("s" flycheck-select-checker)
  ("v" flycheck-verify-setup))


(defhydra hydra-org (:color blue)
  "
  ^
  ^Org^             ^Do^
  ^───^─────────────^──^─────────────
  _q_ quit          _a_ agenda
  ^^                _c_ capture
  ^^                _d_ decrypt
  ^^                _i_ insert-link
  ^^                _k_ cut-subtree
  ^^                _o_ open-link
  ^^                _r_ refile
  ^^                _s_ store-link
  ^^                _t_ todo-tree
  ^^                ^^
  "
  ("q" nil)
  ("a" org-agenda)
  ("c" org-capture)
  ("d" org-decrypt-entry)
  ("k" org-cut-subtree)
  ("i" org-insert-link-global)
  ("o" org-open-at-point-global)
  ("r" org-refile)
  ("s" org-store-link)
  ("t" org-show-todo-tree))

(defhydra hydra-projectile (:color blue)
  "
  ^
  ^Projectile^        ^Buffers^           ^Find^              ^Search^
  ^──────────^────────^───────^───────────^────^──────────────^──────^────────────
  _q_ quit            _b_ list            _d_ directory       _r_ replace
  _i_ reset cache     _K_ kill all        _D_ root            _R_ regexp replace
  ^^                  _S_ save all        _f_ file            _s_ ag
  ^^                  ^^                  _p_ project         ^^
  ^^                  ^^                  ^^                  ^^
  "
  ("q" nil)
  ("b" counsel-projectile-switch-to-buffer)
  ("d" counsel-projectile-find-dir)
  ("D" projectile-dired)
  ("f" counsel-projectile-find-file)
  ("i" projectile-invalidate-cache :color red)
  ("K" projectile-kill-buffers)
  ("p" counsel-projectile-switch-project)
  ("r" projectile-replace)
  ("R" projectile-replace-regexp)
  ("s" counsel-projectile-ag)
  ("S" projectile-save-project-buffers))

(defhydra hydra-yasnippet (:color blue)
  "
  ^
  ^YASnippet^          ^Do^
  ^─────────^──────────^──^────────
  _q_ quit             _i_ insert
  ^^                   _m_ mode
  ^^                   _n_ new
  ^^                   ^^
  "
  ("q" nil)
  ("i" ivy-yasnippet)
  ("m" yas-minor-mode)
  ("n" yas-new-snippet))


(defhydra hydra-windows (:color pink)
  "
  ^
  ^Windows^           ^Window^            ^Zoom^
  ^───────^───────────^──────^────────────^────^──────
  _q_ quit            _b_ balance         _-_ out
  ^^                  _i_ heighten        _+_ in
  ^^                  _j_ narrow          _=_ reset
  ^^                  _k_ lower           ^^
  ^^                  _l_ widen           ^^
  ^^                  _s_ swap            ^^
  ^^                  ^^                  ^^
  "
  ("q" nil)
  ("b" balance-windows)
  ("i" enlarge-window)
  ("j" shrink-window-horizontally)
  ("k" shrink-window)
  ("l" enlarge-window-horizontally)
  ("s" switch-window-then-swap-buffer :color blue)
  ("-" text-scale-decrease)
  ("+" text-scale-increase)
  ("=" (text-scale-increase 0))
  ("J" buf-move-left)
  ("K" buf-move-down)
  ("I" buf-move-up)
  ("L" buf-move-right))



(provide 'core-hydra)
