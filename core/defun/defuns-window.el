;; next/pervious window function
;; C-n / C-p
(defun other-window-backward (&optional n)
  "Select Nth previous window."
  (interactive "p")
  (if n (other-window (- n)) ;if n is none-null
        (other-window  -1)))  ;if n is null

;; create a new empty buffer for write things ;p
;; C-c n
(defun new-empty-buffer()
  (interactive)
  (switch-to-buffer (generate-new-buffer "trash"))
  (funcall (and initial-major-mode))
  (setq buffer-offer-save t))
