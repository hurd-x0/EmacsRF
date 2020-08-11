;; setup-package.el

;; Package configs
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("melpa"        . "https://melpa.org/packages/")
;;                       ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("org"   . "http://orgmode.org/elpa/")
                         ))
(package-initialize)
;; Install extensions if they're missing
;; Require Packages

(defvar my-packages

  '(;; -------------- ;; -----------------------------------------------------------
    auto-complete     ;; auto Completion for GNU Emacs
    diff-hl           ;; highlight uncommitted changes using Version Control
    neotree           ;; a tree plugin like NerdTree for Vim ( required mode-icons )
    mode-icons        ;; Show icons for mode
    powerline         ;; rewrite of Powerline
    ;; ---  MISC  --- ;; -----------------------------------------------------------
    magit             ;; Git integration
    ;; --- PYTHON --- ;; -----------------------------------------------------------
    ; elpy            ;; Emacs Lisp Python Environment
    ; flycheck        ;; On the fly syntax checking
    ; py-autopep8     ;; Run autopep8 on save
    ; blacken         ;; Black formatting on save
    ; ein             ;; Emacs IPython Notebook
    ;; ---        --- ;; -----------------------------------------------------------
    ;; -------------- ;; -----------------------------------------------------------
    ))

;; if list packges is not loaded fetch the list of packages available
(when (not package-archive-contents)
  (package-refresh-contents))

;; install extensions if they're missing or not installed
(dolist (package-name my-packages)
  (when (not (package-installed-p package-name))
    (package-install package-name)))

;; Setup extensions

;; Auto Completion for GNU Emacs
(require 'auto-complete)
(global-auto-complete-mode t)

;; Highlight uncommitted changes using VC
(require 'diff-hl)
(global-diff-hl-mode)

;; a tree plugin like NerdTree for Vim
(require 'neotree)
(setq neo-theme 'fileicons)
(global-set-key [f5] 'neotree-toggle)

;; icons for emacs
(require 'mode-icons)
(mode-icons-mode)

;; Rewrite of Powerline. better power line
(require 'powerline)
(powerline-default-theme)


(provide 'setup-package)

;;; setup-package.el ends here
