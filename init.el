;;; init.el --- better rocks Initialization File
;; Author  : unprogramable
;;

;;; Code:

;; uncomment this for debugging purposes
;; (setq debug-on-error t)
;;
;; without this line emacs25..26 adds (package-initialize) here
;;(package-initialize)


;; user settings
(setq theme 'gruvbox-dark-hard)

;; set and load paths
(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))

;; load all required paths
(require 'core-load-paths)

;; appearance settings
(require 'core-ui)

;; initialize
(require 'core-init)

;; setup-package and config extensions
(require 'setup-package)

;; load user settings and functions
(mapc 'load-path-recursive
      `(
        ,core-defun-directory
        ,user-settings-directory
        ))

;; keyboard bindings
(require 'core-keybind)
(put 'erase-buffer 'disabled nil)
