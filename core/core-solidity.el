(use-package solidity-mode
  :mode ("\\.sol" . solidity-mode)
  :config
  (push 'solidity-mode irony-supported-major-modes))

(provide 'core-solidity)
