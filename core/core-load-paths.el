;;; core-load-paths.el ---  core file

(defun add-to-load-path (directory)
  "add directory to load-path."
  (add-to-list 'load-path directory))

(defun add-to-load-path-if-exists (directory)
  "If directory exists. add it to `load-path'."
  (when (file-exists-p directory)
    (add-to-load-path directory)))

(defun add-to-load-path-recursive (directory)
  "add projects recursive to path."
  (add-to-load-path directory)
  (dolist (project (directory-files directory t "\\w+"))
    (when (file-directory-p project)
      (add-to-load-path project))))

(defun add-theme-to-load-path-recursive (directory)
  "add theme recursive to path."
  (add-to-list 'custom-theme-load-path directory)
  (dolist (project (directory-files directory t "\\w+"))
    (when (file-directory-p project)
      (add-to-list 'custom-theme-load-path project)
      (add-to-load-path project))))

(defun load-path-recursive (directory)
  "load recursive *.el files "
  (when (file-directory-p directory)
    (dolist (project (directory-files directory t "^[^#].*el$"))
      (load project))))


;; expand path (expand '~/' to '/home/user/.emacs.d/')
(setq user-emacs-directory (expand-file-name user-emacs-directory))

;; core paths
(defconst core-directory         (concat user-emacs-directory  "core/"   ))
(defconst core-defun-directory   (concat core-directory        "defun/"  ))
(defconst core-config-directory (concat core-directory        "config/"))


;; Other dirs
;; trash files like recentf history backup and autosave files
(defconst trash-directory         (concat user-emacs-directory "trash/"   ))
;; themes folder for load user and core themes
(defconst themes-directory        (concat user-emacs-directory "themes/"  ))
;; settings current user
(defconst user-settings-directory (concat user-emacs-directory "users/" user-login-name))

;;; load
;; load recursive
(mapc 'add-to-load-path-recursive
      `(
        ,core-config-directory
        ))

;; load .emacs.d/themes
(add-theme-to-load-path-recursive themes-directory)


(provide 'core-load-paths)
;;; core-load-paths ends here
