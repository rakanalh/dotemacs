;;; package --- Main init file
;;; Commentary:
;;; This is my init file

;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;;(package-initialize)

(add-to-list 'load-path (concat user-emacs-directory "config"))
(add-to-list 'load-path (concat user-emacs-directory "lang"))
(add-to-list 'load-path "~/.emacs.d/vendor/")
(add-to-list 'load-path "~/.emacs.d/vendor/greview")

(require 'config-base)
(require 'config-org)
(require 'config-extensions)
(require 'config-ivy)
(require 'config-hydra)
(require 'config-functions)
(require 'config-dired)
;;(require 'config-shell)
(require 'config-aliases)
(require 'config-keys)
(require 'config-ui)

(require 'lang-c)
(require 'lang-go)
(require 'lang-javascript)
(require 'lang-python)
(require 'lang-solidity)
(require 'lang-web)
(require 'greview)
