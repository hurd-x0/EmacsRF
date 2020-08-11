;;; setup-python.el 

;; Enable elpy
(require 'elpy)
(elpy-enable)

;; Use IPython for REPL
(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt"
      python-shell-prompt-detect-failure-warning nil)

(add-to-list 'python-shell-completion-native-disabled-interpreters
             "jupyter")

;; Enable autopep8
(require 'py-autopep8)

(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)


;; Enable Flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(provide 'setup-python)

;;; setup-python.el ends here

