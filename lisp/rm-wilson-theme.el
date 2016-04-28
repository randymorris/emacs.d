(unless (>= emacs-major-version 24)
  (error "requires Emacs 24 or later."))

(deftheme rm-wilson "Additional faces for sublime-themes/wilson")

(let ((wilson-oilstained-eggshell   "#6C6B59")
      (wilson-flying-boots          "#44443C")
      (wilson-darker-flying-boots   "#222222")
      (wilson-spring-grass          "#9BA657")
      (wilson-stained-white         "#BEBFB7")
      (wilson-darker-stained-white  "#A9AAA3")
      (wilson-gray                  "#84857E")
      (wilson-darker-gray           "gray30")
      (wilson-dark-gray             "gray25")
      (wilson-light-gray            "gray20")
      (wilson-lighter-gray          "gray12")
      (wilson-stained-orange        "#B97E56")
      (wilson-darker-stained-orange "#A56F4B")
      (wilson-stained-yellow        "#CFB980")
      (wilson-darker-stained-yellow "#B9A572")
      (rm-wilson-error              "#F56F4B")
      (rm-wilson-warning            "#F5F54B")
      (rm-wilson-info               "#4b6F95")
      (rm-wilson-muted              "#333333"))
  (custom-theme-set-faces
   'rm-wilson

   ;; Region
   `(region ((t (:background ,wilson-stained-yellow :foreground ,wilson-darker-gray))))

   ;; Mode line
   `(mode-line ((t :background ,rm-wilson-muted :foreground ,wilson-stained-yellow :overline ,wilson-flying-boots :underline ,wilson-flying-boots)))
   '(mode-line-buffer-id ((t :weight normal)))

   ;; Whitespace
   `(whitespace-newline ((t (:foreground ,rm-wilson-muted :background ,wilson-darker-flying-boots))))
   `(whitespace-tab ((t (:foreground ,rm-wilson-muted :background ,wilson-darker-flying-boots))))
   `(whitespace-space ((t (:foreground ,rm-wilson-muted :background ,wilson-darker-flying-boots))))

   ;; Term
   `(term-color-black ((t (:foreground ,wilson-darker-gray))))
   `(term-color-red ((t (:foreground ,wilson-stained-orange))))
   `(term-color-green ((t (:foreground ,wilson-spring-grass))))
   `(term-color-yellow ((t (:foreground ,wilson-stained-yellow))))
   `(term-color-blue ((t (:foreground "#729fcf"))))
   `(term-color-magenta((t (:foreground "#ad7fa8"))))
   `(term-color-cyan ((t (:foreground "turquoise"))))
   `(term-color-white ((t (:foreground "#eeeeec"))))

   ;; Helm
   `(helm-buffer-directory ((t (:foreground ,wilson-stained-orange))))
   `(helm-ff-directory ((t (:foreground ,wilson-stained-orange))))
   `(helm-ff-dotted-directory ((t (:foreground ,wilson-oilstained-eggshell))))
   `(helm-ff-prefix ((t (:foreground ,wilson-stained-orange))))
   `(helm-ff-symlink ((t (:foreground ,wilson-spring-grass))))
   `(helm-selection ((t (:background ,wilson-stained-yellow :foreground ,wilson-darker-flying-boots))))
   `(helm-source-header ((t (:foreground ,wilson-stained-yellow :height 140))))
   `(helm-visible-mark ((t (:background ,wilson-darker-stained-orange :foreground "white"))))
   `(helm-candidate-number ((t (:background ,wilson-stained-yellow :foreground ,wilson-darker-flying-boots))))
   '(helm-ff-file ((t nil)))
   '(helm-buffer-file ((t nil)))
   '(helm-buffer-not-saved ((t nil)))
   '(helm-buffer-saved-out ((t nil)))
   '(helm-separator ((t nil)))
   '(helm-header ((t nil)))
   '(helm-prefarg ((t nil)))

   ;; Flycheck
   `(flycheck-fringe-error ((t (:foreground ,rm-wilson-error))))
   `(flycheck-fringe-warning ((t (:foreground ,rm-wilson-warning))))
   `(flycheck-fringe-info ((t (:foreground ,rm-wilson-info))))
   ))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name)))
  (when (not window-system)
    (custom-set-faces '(default ((t (:background nil)))))))

(provide-theme 'rm-wilson)

;; Local Variables:
;; no-byte-compile: t
;; End:
