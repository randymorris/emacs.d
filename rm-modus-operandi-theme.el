;; Allows the mode line to have some padding while still having nice
;; borders.  Probably shouldn't be in the theme file but I'm leaving
;; it for now.
(setq x-underline-at-descent-line t)

(deftheme rm-modus-operandi)
(let ((class '((class color) (min-colors 89)))
      (fg-main "#000000")
      (bg-main "#ffffff")
      (fg-alt "#505050")
      (bg-alt "#f3f1f3")
      (fg-dim "#282828")
      (bg-dim "#f8f8f8")
      (fg-active "#191919")
      (bg-active "#e0e0e0")
      (fg-inactive "#424242")
      (bg-inactive "#efedef")
      (cyan-subtle-bg "#c0efff"))
  (custom-theme-set-faces
   'rm-modus-operandi

   ;; Region
   `(region ((,class (:background ,cyan-subtle-bg))));; :foreground ,fg-main))))

   ;; Mode line
   `(mode-line ((,class (:box ,(list :line-width 4 :color bg-active) :overline ,fg-alt :underline ,fg-alt))))
   `(mode-line-inactive ((,class (:box ,(list :line-width 4 :color bg-inactive) :overline ,fg-alt :underline ,fg-alt))))

   ;; Whitespace
   `(whitespace-newline ((,class (:foreground ,bg-alt :background ,bg-main))))
   `(whitespace-tab ((,class (:foreground ,bg-alt :background ,bg-main))))
   `(whitespace-space ((,class (:foreground ,bg-alt :background ,bg-main))))

   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name)))
  (when (not window-system)
    (custom-set-faces '(default-foreground ((t (:background nil)))))))

(provide-theme 'rm-modus-operandi)

;; Local Variables:
;; no-byte-compile: t
;; End:
