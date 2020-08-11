;; core-init.el

;; Don't be so stingy on the memory, we have lots now. It's the distant future.
(setq gc-cons-threshold 20000000)

;; initial mode
(setq initial-major-mode 'text-mode)

;; Allow pasting selection outside of Emacs
(setq x-select-enable-clipboard t)

;; Make searches case insensitive
(setq case-fold-search t)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Transparently open compressed files
(auto-compression-mode t)

;; slower scrolling up/down
(setq mouse-wheel-scroll-amount '(2 ((shift) . 0)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-conservatively 50)
(setq scroll-up-margin 5)
(setq scroll-margin 5)
(setq scroll-step 1)

;; don't blink cursor
(blink-cursor-mode 0)

;; No electric indent
(electric-indent-mode 0)

;; No auto close bracket insertion
(electric-pair-mode 0)

;; Make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

;; replace highlighted text with type or inserting text
(delete-selection-mode t)

;; Reload an open file from disk if it is changed outside of Emacs.
(global-auto-revert-mode t)

;; 80 chars is a good width.
(set-default 'fill-column 80)

;; Never insert tabs( use space)
(setq-default indent-tabs-mode nil)
(setq-default indent-line-function nil)
(setq-default tab-width 4) ;; let's do this :) smaller files
(setq python-indent 4) ;;  also specifically for python

;; Don't break lines
(setq-default truncate-lines t)

;; Real emacs users don't use shift to mark things 'use C+SPE or C+x SPE'
(setq-default shift-select-mode nil)

;; Don't highlight matches with jump-char - it's distracting
(setq jump-char-lazy-highlight-face nil)

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; A saner ediff
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Easily navigate sillycased words
(global-subword-mode 1)

;; UTF-8
(setq locale-coding-system   'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-language-environment    'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system        'utf-8)

(set-locale-environment "en_US.UTF-8")

(setenv "LANG"              "en_US.UTF-8")
(setenv "LC_ALL"            "en_US.UTF-8")
(setenv "LC_CTYPE"          "en_US.UTF-8")
(setenv "LC_NUMERIC"        "en_US.UTF-8")
(setenv "LC_TIME"           "en_US.UTF-8")
(setenv "LC_COLLATE"        "en_US.UTF-8")
(setenv "LC_MONETARY"       "en_US.UTF-8")
(setenv "LC_PAPER"          "en_US.UTF-8")
(setenv "LC_NAME"           "en_US.UTF-8")
(setenv "LC_ADDRESS"        "en_US.UTF-8")
(setenv "LC_TELEPHONE"      "en_US.UTF-8")
(setenv "LC_MEASUREMENT"    "en_US.UTF-8")
(setenv "LC_IDENTIFICATION" "en_US.UTF-8")

;; Make unix unicode default
(setq-default default-buffer-file-coding-system 'utf-8-unix)

;; Don't write lock-files
(setq create-lockfiles nil)

;; Disable auto save and backup files
(setq make-backup-files nil) ; stop creating backup
(setq auto-save-default nil) ; stop creating #autosave# files

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;;;; Backup
;; don't clobber symlinks
(setq backup-by-copying t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
;; use versioned backups
(setq version-control t)

;; write backup files to own directory
(setq backup-directory-alist
      '(("." . (concat trash-directory "backups"))))

;; Keep emacs Custom-settings in another file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Save point position between sessions
(save-place-mode t)
(setq save-place-file (concat trash-directory "places"))

;; If emacs is slow to exit after enabling saveplace,
;; you may be running afoul of save-place-forget-unreadable-files.
;; On exit, it checks that every loaded file is readable before saving
;; its buffer position - potentially very slow if you use NFS.
(setq save-place-forget-unreadable-files nil)

;; change recentf path
(setq recentf-save-file (concat trash-directory "recentf"))

;; auto save directory
(setq auto-save-file-name-transforms
        `((".*" ,(file-name-as-directory (concat trash-directory "auto-saves")) t)))

;; Save a list of recent files visited. (open recent file with C-x f)
(recentf-mode 1)
(setq recentf-max-saved-items 100) ;; just 20 is too recent

;; 80 chars is a good width.
(set-default 'fill-column 80)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Save minibuffer history
(savehist-mode t)
(setq history-length 1000)

;; delete trailing whitespace when file saved
(add-hook 'before-save-hook
          'delete-trailing-whitespace)

;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Remove security vulnerability
(eval-after-load "enriched"
  '(defun enriched-decode-display-prop (start end &optional param)
     (list start end)))

;; When popping the mark, continue popping until the cursor actually moves
;; Also, if the last command was a copy - skip past all the expand-region cruft.
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (when (eq last-command 'save-region-or-current-line)
      ad-do-it
      ad-do-it
      ad-do-it)
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

(setq set-mark-command-repeat-pop t)

;; Offer to create parent directories if they do not exist
(defun create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions 'create-non-existent-directory)

;; Nic says eval-expression-print-level needs to be set to nil (turned off) so
;; that you can always see what's happening. Nic is wrong.
(setq eval-expression-print-level 100)

;; Disable Eldoc mode, which is enabled by default in Emacs.
;; I've found that it makes navigating Elisp files
;; slow, and I don't use it.
(global-eldoc-mode 0)

;; Emacs server
;; (require 'server)
;; (unless (server-running-p)
;;   (server-start))


(provide 'core-init)
;;; core-init end here
