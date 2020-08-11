;; core-ui.el

;; something about faster startup
(modify-frame-parameters nil '((wait-for-wm . nil)))

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'tool-bar-mode  ) (tool-bar-mode   0))
(if (fboundp 'menu-bar-mode  ) (menu-bar-mode   0))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; scratch message
(setq-default initial-scratch-message
              (concat ";; " user-login-name "\n\n\n\n\n"))

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; global Highlight current line
(global-hl-line-mode t)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode t)
(setq-default show-paren-delay 1)

;; gui running
(when window-system
  ;; Emacs Title
  (setq frame-title-format '(buffer-file-name "( %b )"))
  ;; disable tooltip
  (tooltip-mode 0)
  ;; disable blink
  (blink-cursor-mode 0))

;; display line number in all buffers
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;; path in before loaded in core-load-paths
;; ~/.emacs.d/core/themes for path themes

;; load Theme
(load-theme theme t)


(provide 'core-ui)

;;; core-ui ends here
