;; Allows the mode line to have some padding while still having nice
;; borders.  Probably shouldn't be in the theme file but I'm leaving
;; it for now.
(setq x-underline-at-descent-line t)

(deftheme rm-white-sand)
(let ((class '((class color) (min-colors 89)))
  (fg1 "#585858")
  (fg2 "#646464")
  (fg3 "#707070")
  (fg4 "#7d7d7d")
  (fg5 "#f9e1d6")
  (bg1 "#f5ebe1")
  (bg2 "#dfd6cd")
  (bg3 "#c9c1b9")
  (bg4 "#b4ada6")
  (key2 "#5f9298")
  (key3 "#43757c")
  (builtin "#1a8591")
  (keyword "#4a858c")
  (const   "#697024")
  (comment "#a9a9a9")
  (func    "#bd745e")
  (str     "#b3534b")
  (type    "#8c4a79")
  (var     "#476238")
  (warning "#ff1276"))
(custom-theme-set-faces
'rm-white-sand

;; Mode line
`(mode-line ((,class (:background ,bg2 :foreground ,fg4 :weight normal :box ,(list :line-width 4 :color bg2) :overline ,bg3 :underline ,bg3))))
`(mode-line-inactive ((,class (:background ,bg2 :foreground ,bg2 :weight normal :box ,(list :line-width 4 :color bg2) :overline ,bg3 :underline ,bg3))))

;; Fringe
`(fringe ((,class (:background ,bg1 :foreground ,fg1))))

;; Whitespace
`(whitespace-newline ((,class (:foreground ,fg5 :background ,bg1))))
`(whitespace-tab ((,class (:foreground ,fg5 :background ,bg1))))
`(whitespace-space ((,class (:foreground ,fg5 :background ,bg1))))

;; Swap red/green colors in term-mode so diffs make sense
`(term-color-red ((,class (:foreground ,type :background ,bg3))))
`(term-color-green ((,class (:foreground ,keyword :background ,bg3))))

))

;;;###autoload
(when load-file-name
(add-to-list 'custom-theme-load-path
           (file-name-as-directory (file-name-directory load-file-name)))
(when (not window-system)
(custom-set-faces '(default-foreground ((t (:background nil)))))))

(provide-theme 'rm-white-sand)

;; Local Variables:
;; no-byte-compile: t
;; End:
