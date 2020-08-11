;; Author: unprogramable <unprogramable@gmail.com>
;; Require (( emacs25 or newer))

(unless (>= emacs-major-version 25)
  (error "requires Emacs 25 or newer."))

(deftheme minimax "A dark color theme")

(let (
      ;;;; Default
      ;;==========================================
      (*default-bg*                      "#121212")
      (*default-fg*                      "#7aa"   )
      (*header-line-bg*                  "#303030")
      (*header-line-fg*                  "#e7f6da")
      (*cursor-bg*                       "#e5efae")
      (*text-cursor-bg*                  "#e5efae")
      (*button-fg*                       "#eaaeea")
      (*vertical-border-fg*              "#121312")
      (*info-xref-fg*                    "#eaaeea")

      ;;;; Highlighting faces
      ;;===========================================
      (*secondary-selection-bg*          "#333366")
      (*secondary-selection-fg*          "#f6f3e8")
      (*show-paren-mismatch-bg*          "#FC580C")
      (*show-paren-mismatch-fg*          "#121212")
      (*show-paren-match-bg*             "#333333")
      (*show-paren-match-fg*             "#ffd700")
      (*fringe-bg*                       "#333333")
      (*fringe-fg*                       "#FFD2A7")
      (*line-number-bg*                  "#181818")
      (*line-number-fg*                  "#878"   )
      (*isearch-bg*                      "#E9C"   )
      (*isearch-fg*                      "#292929")
      (*isearch-fail-bg*                 "#FF6C60")
      (*isearch-fail-fg*                 "#000000")
      (*lazy-highlight-bg*               "#988"   )
      (*lazy-highlight-fg*               "#292929")
      (*highlight-bg*                    "#181818")
      (*highlight-fg*                    "#ffffff")
      (*hl-line-bg*                      "#181818")
      (*highlight-face-bg*               "#181818")
      (*region-bg*                       "#292929")
      (*region-fg*                       "#f6f3e8")
      (*link-fg*                         "#8ac6f2")
      (*link-visited-fg*                 "#e5786d")

      ;;;; Mode line faces
      ;;===========================================
      (*mode-line-bg*                    "#444444")
      (*mode-line-fg*                    "#f6f3e8")
      (*mode-line-inactive-bg*           "#444444")
      (*mode-line-inactive-fg*           "#857b6f")
      ;;  Escape and prompt faces
      ;;===========================================
      (*minibuffer-prompt-fg*            "#C6C5FE")
      (*escape-glyph-fg*                 "#ddaa6f")
      (*homoglyph-fg*                    "#ddaa6f")

      ;;;; Font lock faces
      ;;===========================================
      (*font-lock-comment-delimiter-face-fg*     "#615953")
      (*font-lock-doc-face-fg*                   "#615953")
      (*font-lock-doc-string-face-fg*            "#615953")
      (*font-lock-function-name-face-fg*         "#ff5953")
      (*font-lock-keyword-face-fg*               "#739")
      (*font-lock-negation-char-face-fg*         "#FC580C")
      (*font-lock-number-face-fg*                "#878"   )
      (*font-lock-preprocessor-face-fg*          "#eaaeea")
      (*font-lock-reference-face-fg*             "#F38630")
      (*font-lock-regexp-grouping-backslash-fg*  "#E9C"   )
      (*font-lock-regexp-grouping-construct-fg*  "#E9C"   )
      (*font-lock-string-face-fg*                "#448b55")
      (*font-lock-type-face-fg*                  "#988"   )
      (*font-lock-variable-name-face-fg*         "#bbaacc")
      (*font-lock-warning-face-fg*               "#FC580C")
      (*font-lock-builtin-face-fg*               "#b03060")
      (*font-lock-comment-face-fg*               "#615953")
      (*font-lock-constant-face-fg*              "#F38630")

      ;;;; Gnus faces
      ;;===========================================
      ;; news
      (*gnus-group-news-1-fg*            "#95e454")
      (*gnus-group-news-2-fg*            "#cae682")
      (*gnus-group-news-3-fg*            "#ccaa8f")
      (*gnus-group-news-4-fg*            "#99968b")
      (*gnus-group-news-5-fg*            "#cae682")
      (*gnus-group-news-low-fg*          "#99968b")
      (*gnus-group-news-1-low-fg*        "#95e454")
      (*gnus-group-news-2-low-fg*        "#cae682")
      (*gnus-group-news-3-low-fg*        "#ccaa8f")
      (*gnus-group-news-4-low-fg*        "#99968b")
      (*gnus-group-news-5-low-fg*        "#cae682")
      ;; mail
      (*gnus-group-mail-1-fg*            "#95e454")
      (*gnus-group-mail-2-fg*            "#cae682")
      (*gnus-group-mail-3-fg*            "#ccaa8f")
      (*gnus-group-mail-low-fg*          "#99968b")
      (*gnus-group-mail-1-low-fg*        "#95e454")
      (*gnus-group-mail-2-low-fg*        "#cae682")
      (*gnus-group-mail-3-low-fg*        "#ccaa8f")
      ;; headers
      (*gnus-header-content-fg*          "#8ac6f2")
      (*gnus-header-from-fg*             "#95e454")
      (*gnus-header-subject-fg*          "#cae682")
      (*gnus-header-name-fg*             "#8ac6f2")
      (*gnus-header-newsgroups-fg*       "#cae682")

      ;;;; Message faces
      ;;===========================================
      (*message-header-name-fg*          "#8ac6f2")
      (*message-header-cc-fg*            "#95e454")
      (*message-header-other-fg*         "#95e454")
      (*message-header-subject-fg*       "#cae682")
      (*message-header-to-fg*            "#cae682")
      (*message-cited-text-fg*           "#99968b")
      (*message-separator-fg*            "#e5786d"))

  (custom-theme-set-faces
   'minimax
   ;;;; Default
   ;;============================================================
   `(default          ((t (:background, *default-bg*     :foreground, *default-fg* :underline nil :weight normal :width normal :height 120 ))))
   `(header-line      ((t (:background, *header-line-bg* :foreground, *header-line-fg*     ))))
   `(cursor           ((t (:background, *cursor-bg*          )))) ;cursor color
   `(text-cursor      ((t (:background, *text-cursor-bg*     ))))
   `(button           ((t (:foreground, *button-fg*          ))))
   `(vertical-border  ((t (:foreground, *vertical-border-fg* ))))
   `(info-xref        ((t (:foreground, *info-xref-fg*       ))))
   ;;;; Highlighting faces
   ;;============================================================
   `(secondary-selection ((t (:background, *secondary-selection-bg* :foreground, *secondary-selection-fg*              ))))
   `(show-paren-mismatch ((t (:background, *show-paren-mismatch-bg* :foreground, *show-paren-mismatch-fg* :weight bold )))) ;---- () highlight
   `(show-paren-match    ((t (:background, *show-paren-match-bg*    :foreground, *show-paren-match-fg*    :weight bold )))) ;---- miss ) highlight red
   `(fringe              ((t (:background, *fringe-bg*              :foreground, *fringe-fg*       )))) ;---- arrow in out of end line
   `(line-number         ((t (:background, *line-number-bg*         :foreground, *line-number-fg*  )))) ;---- line number
   `(isearch             ((t (:background, *isearch-fg*             :foreground, *isearch-bg*      )))) ;---- search field
   `(isearch-fail        ((t (:background, *isearch-fail-bg*        :foreground, *isearch-fail-fg* )))) ;---- search failed red
   `(lazy-highlight      ((t (:background, *lazy-highlight-bg*      :foreground, *lazy-highlight-fg*           ))))
   `(highlight           ((t (:background, *highlight-bg*           :foreground, *highlight-fg* :underline t   ))))
   `(hl-line             ((t (:background, *hl-line-bg*                     )))) ;---- hight light current line
   `(highlight-face      ((t (:background, *highlight-face-bg*              ))))
   `(region              ((t (:background, *region-bg*                      ))));:foreground, *region-fg* )))) ;---- highlight selection
   `(link                ((t (:foreground, *link-fg*           :underline t ))))
   `(link-visited        ((t (:foreground, *link-visited-fg*   :underline t ))))
   `(vertical-border ((t (:background "dim gray" :foreground "#8a2be2" ))))
   ;;;; Mode line faces
   ;;============================================================
   `(mode-line           ((t (:background, *mode-line-bg*          :foreground, *mode-line-fg*          ))))
   `(mode-line-inactive  ((t (:background, *mode-line-inactive-bg* :foreground, *mode-line-inactive-fg* ))))

   ;;;; Escape and prompt faces
   ;;============================================================
   `(minibuffer-prompt   ((t (:foreground, *minibuffer-prompt-fg*         ))))
   `(escape-glyph        ((t (:foreground, *escape-glyph-fg* :weight bold ))))
   `(homoglyph           ((t (:foreground, *homoglyph-fg*    :weight bold ))))

   ;;;; Font lock faces
   ;;============================================================
   `(font-lock-comment-delimiter-face    ((t (:foreground, *font-lock-comment-delimiter-face-fg*    ))))
   `(font-lock-doc-face                  ((t (:foreground, *font-lock-doc-face-fg*                  ))))
   `(font-lock-doc-string-face           ((t (:foreground, *font-lock-doc-string-face-fg*           ))))
   `(font-lock-function-name-face        ((t (:foreground, *font-lock-function-name-face-fg*        ))))
   `(font-lock-keyword-face              ((t (:foreground, *font-lock-keyword-face-fg* :weight bold ))))
   `(font-lock-negation-char-face        ((t (:foreground, *font-lock-negation-char-face-fg*        ))))
   `(font-lock-number-face               ((t (:foreground, *font-lock-number-face-fg*               ))))
   `(font-lock-preprocessor-face         ((t (:foreground, *font-lock-preprocessor-face-fg*         ))))
   `(font-lock-reference-face            ((t (:foreground, *font-lock-reference-face-fg*            ))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground, *font-lock-regexp-grouping-backslash-fg* ))))
   `(font-lock-regexp-grouping-construct ((t (:foreground, *font-lock-regexp-grouping-construct-fg* ))))
   `(font-lock-string-face               ((t (:foreground, *font-lock-string-face-fg*               ))))
   `(font-lock-type-face                 ((t (:foreground, *font-lock-type-face-fg*  :weight bold   ))))
   `(font-lock-variable-name-face        ((t (:foreground, *font-lock-variable-name-face-fg*        ))))
   `(font-lock-warning-face              ((t (:foreground, *font-lock-warning-face-fg*              ))))
   `(font-lock-builtin-face              ((t (:foreground, *font-lock-builtin-face-fg*              ))))
   `(font-lock-comment-face              ((t (:foreground, *font-lock-comment-face-fg*              ))))
   `(font-lock-constant-face             ((t (:foreground, *font-lock-constant-face-fg*             ))))

   ;;;; Gnus faces
   ;;============================================================
   ;; news
   `(gnus-group-news-1      ((t (:foreground, *gnus-group-news-1-fg* :weight bold ))))
   `(gnus-group-news-2      ((t (:foreground, *gnus-group-news-2-fg* :weight bold ))))
   `(gnus-group-news-3      ((t (:foreground, *gnus-group-news-3-fg* :weight bold ))))
   `(gnus-group-news-4      ((t (:foreground, *gnus-group-news-4-fg* :weight bold ))))
   `(gnus-group-news-5      ((t (:foreground, *gnus-group-news-5-fg* :weight bold ))))
   `(gnus-group-news-low    ((t (:foreground, *gnus-group-news-low-fg*            ))))
   `(gnus-group-news-1-low  ((t (:foreground, *gnus-group-news-1-low-fg*          ))))
   `(gnus-group-news-2-low  ((t (:foreground, *gnus-group-news-2-low-fg*          ))))
   `(gnus-group-news-3-low  ((t (:foreground, *gnus-group-news-3-low-fg*          ))))
   `(gnus-group-news-4-low  ((t (:foreground, *gnus-group-news-4-low-fg*          ))))
   `(gnus-group-news-5-low  ((t (:foreground, *gnus-group-news-5-low-fg*          ))))

   ;; mail
   `(gnus-group-mail-1      ((t (:foreground, *gnus-group-mail-1-fg* :weight bold ))))
   `(gnus-group-mail-2      ((t (:foreground, *gnus-group-mail-2-fg* :weight bold ))))
   `(gnus-group-mail-3      ((t (:foreground, *gnus-group-mail-3-fg* :weight bold ))))
   `(gnus-group-mail-low    ((t (:foreground, *gnus-group-mail-low-fg*            ))))
   `(gnus-group-mail-1-low  ((t (:foreground, *gnus-group-mail-1-low-fg*          ))))
   `(gnus-group-mail-2-low  ((t (:foreground, *gnus-group-mail-2-low-fg*          ))))
   `(gnus-group-mail-3-low  ((t (:foreground, *gnus-group-mail-3-low-fg*          ))))

   ;; headers
   `(gnus-header-name       ((t (:foreground, *gnus-header-name-fg*               ))))
   `(gnus-header-subject    ((t (:foreground, *gnus-header-subject-fg*            ))))
   `(gnus-header-content    ((t (:foreground, *gnus-header-content-fg*            ))))
   `(gnus-header-newsgroups ((t (:foreground, *gnus-header-newsgroups-fg*         ))))
   `(gnus-header-from       ((t (:foreground, *gnus-header-from-fg*  :weight bold ))))

   ;;;; Message faces
   ;;============================================================
   `(message-header-name    ((t (:foreground, *message-header-name-fg* :weight bold ))))
   `(message-header-cc      ((t (:foreground, *message-header-cc-fg*                ))))
   `(message-header-other   ((t (:foreground, *message-header-other-fg*             ))))
   `(message-header-subject ((t (:foreground, *message-header-subject-fg*           ))))
   `(message-header-to      ((t (:foreground, *message-header-to-fg*                ))))
   `(message-cited-text     ((t (:foreground, *message-cited-text-fg*               ))))
   `(message-separator      ((t (:foreground, *message-separator-fg*   :weight bold ))))
   )
  )


;; provide minimax theme
(provide-theme 'minimax)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; minimax-theme.el ends here
