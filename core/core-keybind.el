;; core-keybind.el
;; next window                                                         
(global-set-key (kbd "C-x C-p") 'other-window)

;; pervious window
(global-set-key (kbd "C-x C-n") 'other-window-backward)

;; unexpand n depth all function
(global-set-key (kbd "C-c ]") 'folding)

;; expand all functions
(global-set-key (kbd "C-c [") '(lambda () (interactive) (folding nil)))

;; open new empty buffer
(global-set-key (kbd "C-c n") 'new-empty-buffer)

;; describe foo at point
(global-set-key (kbd "<f1>") 'describe-foo-at-point)

;; better buffer movement
(global-set-key (kbd "S-<right>") 'next-buffer)
(global-set-key (kbd "S-<left>") 'previous-buffer)

;; disable help/manuals as F1
;; (global-unset-key [f1])
;; (global-unset-key [f11])


(provide 'core-keybind)
;;; core-keybind ends here
