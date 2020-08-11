;; web-mode
;; wiki: http://web-mode.org/
;;==============================
(require 'web-mode)

;; Automatically load web-mode when opening web-related files
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))

;; Never forget that the matching is done on the path and not just on the filename.
;; If your templates are stored in a subdirectory called views, html or templates 
(add-to-list 'auto-mode-alist '("/\\(views\\|html\\|templates\\)/.*\\.php\\'" . web-mode))

;; Enable / disable features
;; Auto-pairing
(setq web-mode-enable-auto-pairing t)

;; CSS colorization
(setq web-mode-enable-css-colorization t)

;; Block face: can be used to set blocks background and
;; default foreground (see web-mode-block-face)
;;(setq web-mode-enable-block-face t)

;; can be used to set parts background and default foreground
;; (see web-mode-script-face and
;;      web-mode-style-face which inheritate
;; from web-mode-part-face)
;;(setq web-mode-enable-part-face t)

;;Comment keywords (see web-mode-comment-keyword-face)
(setq web-mode-enable-comment-interpolation t)

;; Set indentations
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2))

(add-hook 'web-mode-hook  'my-web-mode-hook)
(setq tab-width 2)

;; disable other indent
(add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))


;; padding
(setq web-mode-style-padding 1)
(setq web-mode-script-padding 1)
(setq web-mode-block-padding 0)
(setq web-mode-comment-style 2)

;; auto indentation
(local-set-key (kbd "RET") 'newline-and-indent)

;; Highlight of columns
(setq web-mode-enable-current-column-highlight t)
;(setq web-mode-enable-current-element-highlight t)

;; Company settings
;; Set the company completion vocabulary to css and html when in web-mode
(defun my-web-mode-hook ()
  (set (make-local-variable 'company-backends)
      '(company-css company-web-html company-yasnippet company-files))
)

;; Emmet settings
(add-hook 'web-mode-hook  'emmet-mode)

;; Web-mode is able to switch modes into css or js in an html file.
;; switch between language in the same document
(add-hook 'web-mode-before-auto-complete-hooks
    '(lambda ()
     (let ((web-mode-cur-language
  	    (web-mode-language-at-pos)))
               (if (string= web-mode-cur-language "php")
    	   (yas-activate-extra-mode 'php-mode)
      	 (yas-deactivate-extra-mode 'php-mode))
               (if (string= web-mode-cur-language "css")
    	   (setq emmet-use-css-transform t)
      	 (setq emmet-use-css-transform nil)))))

;; Autoremove final white spaces on save
(add-hook 'local-write-file-hooks
          (lambda ()
            (delete-trailing-whitespace)
            nil))

;; provide setup-web-mode
(provide 'setup-web-mode)

;;; setup-web-mode.el ends here


