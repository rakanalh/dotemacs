(use-package hydra
  :defer 2
  :bind ("C-c c" . hydra-clock/body)
  :bind ("C-c y" . hydra-yasnippet/body))

(defhydra hydra-clock (:color blue)
    "
    ^
    ^Clock^             ^Do^
    ^─────^─────────────^──^─────────
    _q_ quit            _c_ cancel
    ^^                  _d_ display
    ^^                  _e_ effort
    ^^                  _i_ in
    ^^                  _j_ jump
    ^^                  _o_ out
    ^^                  _r_ report
    ^^                  ^^
    "
    ("q" nil)
    ("c" org-clock-cancel :color pink)
    ("d" org-clock-display)
    ("e" org-clock-modify-effort-estimate)
    ("i" org-clock-in)
    ("j" org-clock-goto)
    ("o" org-clock-out)
    ("r" org-clock-report))


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
  ("i" yas-insert-snippet)
  ("m" yas-minor-mode)
  ("n" yas-new-snippet))

(provide 'core-hydra)
