;; fold/unfold functions
(defun folding (&optional n)
  "fold/unfold block of codes:

   'C-c ]' -> fold codes with depth 1
   -----------------------------------------
   'C-c [' -> unfold all
   -----------------------------------------
   'C-[num]' 'C-c ]' -> fold codes with depth [num]

example:

   'C-2' 'C-c ]' -> fold codes with depth 2"

  (interactive "p")

  (if n (if (eq n t)(set-selective-display 1)
          (set-selective-display n))
    (set-selective-display 0))
  )

;; move (shift) Nth line of lines up or down
(defun shift-line-or-region ()
  "Downcase current word or region."
  (interactive)
  (let (pos1 pos2 bds)
    (if (use-region-p)
        (setq pos1 (region-beginning) pos2 (region-end))
      (progn
        (setq bds (bounds-of-thing-at-point 'symbol))
        (setq pos1 (car bds) pos2 (cdr bds))))

    ;; now, pos1 and pos2 are the starting and ending positions of the
    ;; current word, or current text selection if exist.
    (kill-region pos1 pos2)
     (sit-for 1)
    (yank)
    ))
