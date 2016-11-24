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
(package-initialize)

(add-to-list 'load-path (concat user-emacs-directory "core"))
(add-to-list 'load-path "~/.emacs.d/vendor/")

(require 'core)
(require 'core-functions)
(require 'core-extensions)
(require 'core-go)
(require 'core-haskell)
(require 'core-python)
(require 'core-web)
(require 'core-shell)
(require 'core-aliases)
(require 'core-keys)
(require 'core-ui)
