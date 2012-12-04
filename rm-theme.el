;;; color-theme-rm.el

(defmacro color-theme-rm--with-colors (&rest body)
  "Execute `BODY' in a scope with variables bound to the various colors."

  `(let* ((background "#1d1f21")
	  (current-line "#282a2e")
	  (selection "#373b41")
	  (foreground "#c5c8c6")
	  (comment "#969896")
	  (red "#cc6666")
	  (orange "#de935f")
	  (yellow "#f0c674")
	  (green "#b5bd68")
	  (aqua "#8abeb7")
	  (blue "#81a2be")
	  (purple "#b294bb")
	  (mode-line-background "#333333")
	  (mode-line-foreground "#666666")
	  (mode-line-bright "#999999")
          (class '((class color) (min-colors 89))))
     ,@body))

(defmacro color-theme-rm--face-specs ()
  "Return a backquote which defines a list of face specs.

It expects to be evaluated in a scope in which the various color
names to which it refers are bound."
  (quote
   `(;; Standard font lock faces
     (default ((,class (:foreground ,foreground :background ,background))))
     (bold ((,class (:weight bold))))
     (bold-italic ((,class (:slant italic :weight bold))))
     (underline ((,class (:underline t))))
     (italic ((,class (:slant italic))))
     (font-lock-builtin-face ((,class (:foreground ,purple))))
     (font-lock-comment-delimiter-face ((,class (:foreground ,comment :slant italic))))
     (font-lock-comment-face ((,class (:foreground ,comment :slant italic))))
     (font-lock-constant-face ((,class (:foreground ,orange))))
     (font-lock-doc-face ((,class (:foreground ,purple))))
     (font-lock-doc-string-face ((,class (:foreground ,yellow))))
     (font-lock-function-name-face ((,class (:foreground ,blue))))
     (font-lock-keyword-face ((,class (:foreground ,green))))
     (font-lock-negation-char-face ((,class (:foreground ,green))))
     (font-lock-preprocessor-face ((,class (:foreground ,purple))))
     (font-lock-regexp-grouping-backslash ((,class (:foreground ,yellow))))
     (font-lock-regexp-grouping-construct ((,class (:foreground ,purple))))
     (font-lock-string-face ((,class (:foreground ,aqua))))
     (font-lock-type-face ((,class (:foreground ,orange))))
     (font-lock-variable-name-face ((,class (:foreground ,yellow))))
     (font-lock-warning-face ((,class (:weight bold :foreground ,red))))
     (shadow ((,class (:foreground ,comment))))
     (success ((,class (:foreground ,green))))
     (error ((,class (:foreground ,red))))
     (warning ((,class (:foreground ,orange))))

     ;; Flymake
     (flymake-warnline ((,class (:underline ,orange :background ,background))))
     (flymake-errline ((,class (:underline ,red :background ,background))))

     ;; Clojure errors
     (clojure-test-failure-face ((,class (:background nil :inherit flymake-warnline))))
     (clojure-test-error-face ((,class (:background nil :inherit flymake-errline))))
     (clojure-test-success-face ((,class (:background nil :foreground nil :underline ,green))))

     ;; For Brian Carper's extended clojure syntax table
     (clojure-keyword ((,class (:foreground ,yellow))))
     (clojure-parens ((,class (:foreground ,foreground))))
     (clojure-braces ((,class (:foreground ,green))))
     (clojure-brackets ((,class (:foreground ,yellow))))
     (clojure-double-quote ((,class (:foreground ,aqua :background nil))))
     (clojure-special ((,class (:foreground ,blue))))
     (clojure-java-call ((,class (:foreground ,purple))))

     ;; Rainbow-delimiters
     (rainbow-delimiters-depth-1-face ((,class (:foreground ,foreground))))
     (rainbow-delimiters-depth-2-face ((,class (:foreground ,aqua))))
     (rainbow-delimiters-depth-3-face ((,class (:foreground ,yellow))))
     (rainbow-delimiters-depth-4-face ((,class (:foreground ,green))))
     (rainbow-delimiters-depth-5-face ((,class (:foreground ,blue))))
     (rainbow-delimiters-depth-6-face ((,class (:foreground ,foreground))))
     (rainbow-delimiters-depth-7-face ((,class (:foreground ,aqua))))
     (rainbow-delimiters-depth-8-face ((,class (:foreground ,yellow))))
     (rainbow-delimiters-depth-9-face ((,class (:foreground ,green))))
     (rainbow-delimiters-unmatched-face ((,class (:foreground ,red))))

     ;; MMM-mode
     (mmm-code-submode-face ((,class (:background ,current-line))))
     (mmm-comment-submode-face ((,class (:inherit font-lock-comment-face))))
     (mmm-output-submode-face ((,class (:background ,current-line))))

     ;; Search
     (match ((,class (:foreground ,blue :background ,background :inverse-video t))))
     (isearch ((,class (:foreground ,yellow :background ,background :inverse-video t))))
     (isearch-lazy-highlight-face ((,class (:foreground ,aqua :background ,background :inverse-video t))))
     (isearch-fail ((,class (:background ,background :inherit font-lock-warning-face :inverse-video t))))

     ;; IDO
     (ido-subdir ((,class (:foreground ,purple))))
     (ido-first-match ((,class (:foreground ,orange))))
     (ido-only-match ((,class (:foreground ,green))))
     (ido-indicator ((,class (:foreground ,red :background ,background))))
     (ido-virtual ((,class (:foreground ,comment))))

     ;; Emacs interface
     (cursor ((,class (:background ,red))))
     (fringe ((,class (:background ,current-line))))
     (linum ((,class (:background ,current-line))))
     (border ((,class (:background ,current-line))))
     (border-glyph ((,class (nil))))
     (highlight ((,class (:inverse-video nil :background ,current-line))))
     (gui-element ((,class (:background ,current-line :foreground ,foreground))))
     (mode-line-buffer-id ((,class (:foreground ,mode-line-bright :background nil :weight bold))))
     (mode-line ((,class (:foreground ,mode-line-foreground 
				      :background ,mode-line-background
                                      :box (:line-width 1 :color ,mode-line-foreground)))))
     (mode-line-inactive ((,class (:inherit mode-line
                                       :foreground ,mode-line-foreground
                                       :background ,mode-line-background
                                       :box (:line-width 1 :color ,mode-line-background)))))
     (mode-line-emphasis ((,class (:foreground ,mode-line-foreground :slant italic))))
     (mode-line-highlight ((,class (:foreground ,mode-line-foreground :box nil :weight bold))))
     (minibuffer-prompt ((,class (:foreground ,blue))))
     (region ((,class (:background ,selection))))
     (secondary-selection ((,class (:background ,current-line))))

     (header-line ((,class (:inherit mode-line :foreground ,purple :background nil))))

     (trailing-whitespace ((,class (:foreground ,red :inverse-video t :underline nil))))
     (whitespace-trailing ((,class (:foreground ,red :inverse-video t :underline nil))))
     (whitespace-space-after-tab ((,class (:foreground ,red :inverse-video t :underline nil))))
     (whitespace-space-before-tab ((,class (:foreground ,red :inverse-video t :underline nil))))
     (whitespace-empty ((,class (:foreground ,red :inverse-video t :underline nil))))
     (whitespace-line ((,class (:background nil :foreground ,red))))
     (whitespace-indentation ((,class (:background nil :foreground ,aqua))))
     (whitespace-space ((,class (:background nil :foreground ,selection))))
     (whitespace-newline ((,class (:background nil :foreground ,selection))))
     (whitespace-tab ((,class (:background nil :foreground ,selection))))
     (whitespace-hspace ((,class (:background nil :foreground ,selection))))

     ;; Parenthesis matching (built-in)
     (show-paren-match ((,class (:background nil :foreground nil :inverse-video t))))
     (show-paren-mismatch ((,class (:background ,purple :foreground ,background))))

     ;; Parenthesis matching (mic-paren)
     (paren-face-match ((,class (:foreground nil :background nil :inherit show-paren-match))))
     (paren-face-mismatch ((,class (:foreground nil :background nil :inherit show-paren-mismatch))))
     (paren-face-no-match ((,class (:foreground nil :background nil :inherit show-paren-mismatch))))

     ;; Parenthesis dimming (parenface)
     (paren-face ((,class (:foreground ,comment :background nil))))

     (sh-heredoc ((,class (:foreground nil :inherit font-lock-string-face :weight normal))))
     (sh-quoted-exec ((,class (:foreground nil :inherit font-lock-preprocessor-face))))
     (slime-highlight-edits-face ((,class (:weight bold))))
     (slime-repl-input-face ((,class (:weight normal :underline nil))))
     (slime-repl-prompt-face ((,class (:underline nil :weight bold :foreground ,purple))))
     (slime-repl-result-face ((,class (:foreground ,green))))
     (slime-repl-output-face ((,class (:foreground ,blue :background ,background))))

     (csv-separator-face ((,class (:foreground ,orange))))

     (diff-added ((,class (:foreground ,green))))
     (diff-changed ((,class (:foreground ,purple))))
     (diff-removed ((,class (:foreground ,orange))))
     (diff-header ((,class (:foreground ,aqua :background nil))))
     (diff-file-header ((,class (:foreground ,blue :background nil))))
     (diff-hunk-header ((,class (:foreground ,purple))))

     ;; undo-tree
     (undo-tree-visualizer-default-face ((,class (:foreground ,foreground))))
     (undo-tree-visualizer-current-face ((,class (:foreground ,green :weight bold))))
     (undo-tree-visualizer-active-branch-face ((,class (:foreground ,red))))
     (undo-tree-visualizer-register-face ((,class (:foreground ,yellow))))

     ;; dired+
     (diredp-dir-heading ((,class (:foreground nil :background nil :inherit heading))))
     (diredp-dir-priv ((,class (:foreground ,aqua :background nil))))
     (diredp-exec-priv ((,class (:foreground ,blue :background nil))))
     (diredp-file-name ((,class (:foreground ,yellow))))
     (diredp-file-suffix ((,class (:foreground ,green))))
     (diredp-flag-mark-line ((,class (:background nil :inherit highlight))))
     (diredp-ignored-file-name ((,class (:foreground ,comment))))
     (diredp-link-priv ((,class (:background nil :foreground ,purple))))
     (diredp-mode-line-flagged ((,class (:foreground ,red))))
     (diredp-no-priv ((,class (:background nil))))
     (diredp-number ((,class (:foreground ,yellow))))
     (diredp-other-priv ((,class (:background nil :foreground ,purple))))
     (diredp-rare-priv ((,class (:foreground ,red :background nil))))
     (diredp-read-priv ((,class (:foreground ,green :background nil))))
     (diredp-symlink ((,class (:foreground ,purple))))
     (diredp-write-priv ((,class (:foreground ,yellow :background nil))))

     ;; Magit (a patch is pending in magit to make these standard upstream)
     (magit-branch ((,class (:foreground ,green))))
     (magit-header ((,class (:inherit nil :weight bold))))
     (magit-item-highlight ((,class (:inherit highlight :background nil))))
     (magit-log-graph ((,class (:foreground ,comment))))
     (magit-log-sha1 ((,class (:foreground ,yellow))))
     (magit-log-head-label-bisect-bad ((,class (:foreground ,red))))
     (magit-log-head-label-bisect-good ((,class (:foreground ,green))))
     (magit-log-head-label-default ((,class (:foreground ,yellow :box nil :weight bold))))
     (magit-log-head-label-local ((,class (:foreground ,purple :box nil :weight bold))))
     (magit-log-head-label-remote ((,class (:foreground ,purple :box nil :weight bold))))
     (magit-log-head-label-tags ((,class (:foreground ,aqua :box nil :weight bold))))
     (magit-section-title ((,class (:foreground ,blue :weight bold))))

     (link ((,class (:foreground nil :underline t))))
     (widget-button ((,class (:underline t))))
     (widget-field ((,class (:background ,current-line :box (:line-width 1 :color ,foreground)))))

     ;; Compilation (most faces politely inherit from 'success, 'error, 'warning etc.)
     (compilation-column-number ((,class (:foreground ,yellow))))
     (compilation-line-number ((,class (:foreground ,yellow))))
     (compilation-message-face ((,class (:foreground ,blue))))

     ;; Grep
     (grep-context-face ((,class (:foreground ,comment))))
     (grep-error-face ((,class (:foreground ,red :weight bold :underline t))))
     (grep-hit-face ((,class (:foreground ,blue))))
     (grep-match-face ((,class (:foreground nil :background nil :inherit match))))

     (regex-tool-matched-face ((,class (:foreground nil :background nil :inherit match))))

     ;; mark-multiple
     (mm/master-face ((,class (:inherit region :foreground nil :background nil))))
     (mm/mirror-face ((,class (:inherit region :foreground nil :background nil))))

     (org-date ((,class (:foreground ,blue :underline t))))
     (org-agenda-structure ((,class (:foreground ,purple))))
     (org-agenda-date ((,class (:foreground ,blue :underline nil))))
     (org-agenda-done ((,class (:foreground ,green))))
     (org-agenda-dimmed-todo-face ((,class (:foreground ,comment))))
     (org-block ((,class (:foreground ,orange))))
     (org-code ((,class (:foreground ,yellow))))
     (org-column ((,class (:background ,current-line))))
     (org-document-info ((,class (:foreground ,aqua))))
     (org-document-info-keyword ((,class (:foreground ,green))))
     (org-document-title ((,class (:weight bold :foreground ,orange :height 1.44))))
     (org-done ((,class (:foreground ,green))))
     (org-formula ((,class (:foreground ,red))))
     (org-link ((,class (:foreground ,blue :underline t))))
     (org-scheduled ((,class (:foreground ,green))))
     (org-scheduled-previously ((,class (:foreground ,orange))))
     (org-scheduled-today ((,class (:foreground ,green))))
     (org-special-keyword ((,class (:foreground ,orange))))
     (org-table ((,class (:foreground ,purple))))
     (org-todo ((,class (:foreground ,red))))
     (org-upcoming-deadline ((,class (:foreground ,orange))))
     (org-warning ((,class (:weight bold :foreground ,red))))

     (markdown-header-face ((,class (:inherit header-line))))
     (markdown-url-face ((,class (:inherit link))))
     (markdown-link-face ((,class (:foreground ,blue :underline t))))

     (hl-sexp-face ((,class (:background ,current-line))))
     (highlight-80+ ((,class (:background ,current-line))))

     ;; Python-specific overrides
     (py-builtins-face ((,class (:foreground ,orange :weight normal))))

     ;; js2-mode
     (js2-warning-face ((,class (:underline ,orange))))
     (js2-error-face ((,class (:foreground nil :underline ,red))))
     (js2-external-variable-face ((,class (:foreground ,purple))))
     (js2-function-param-face ((,class (:foreground ,blue))))
     (js2-instance-member-face ((,class (:foreground ,blue))))
     (js2-private-function-call-face ((,class (:foreground ,red))))

     ;; js3-mode
     (js3-warning-face ((,class (:underline ,orange))))
     (js3-error-face ((,class (:foreground nil :underline ,red))))
     (js3-external-variable-face ((,class (:foreground ,purple))))
     (js3-function-param-face ((,class (:foreground ,blue))))
     (js3-jsdoc-tag-face ((,class (:foreground ,orange))))
     (js3-jsdoc-type-face ((,class (:foreground ,aqua))))
     (js3-jsdoc-value-face ((,class (:foreground ,yellow))))
     (js3-jsdoc-html-tag-name-face ((,class (:foreground ,blue))))
     (js3-jsdoc-html-tag-delimiter-face ((,class (:foreground ,green))))
     (js3-instance-member-face ((,class (:foreground ,blue))))
     (js3-private-function-call-face ((,class (:foreground ,red))))

     ;; nxml
     (nxml-name-face ((,class (:foreground unspecified :inherit font-lock-constant-face))))
     (nxml-attribute-local-name-face ((,class (:foreground unspecified :inherit font-lock-variable-name-face))))
     (nxml-ref-face ((,class (:foreground unspecified :inherit font-lock-preprocessor-face))))
     (nxml-delimiter-face ((,class (:foreground unspecified :inherit font-lock-keyword-face))))
     (nxml-delimited-data-face ((,class (:foreground unspecified :inherit font-lock-string-face))))
     (rng-error-face ((,class (:underline ,red))))

     ;; RHTML
     (erb-delim-face ((,class (:background ,current-line))))
     (erb-exec-face ((,class (:background ,current-line :weight bold))))
     (erb-exec-delim-face ((,class (:background ,current-line))))
     (erb-out-face ((,class (:background ,current-line :weight bold))))
     (erb-out-delim-face ((,class (:background ,current-line))))
     (erb-comment-face ((,class (:background ,current-line :weight bold :slant italic))))
     (erb-comment-delim-face ((,class (:background ,current-line))))

     ;; Message-mode
     (message-header-other ((,class (:inherit header-line :foreground nil :background nil :weight normal))))
     (message-header-subject ((,class (:inherit message-header-other :weight bold :foreground ,yellow))))
     (message-header-to ((,class (:inherit message-header-other :weight bold :foreground ,orange))))
     (message-header-cc ((,class (:inherit message-header-to :foreground nil))))
     (message-header-name ((,class (:inherit header-line :foreground ,green :background nil))))
     (message-header-newsgroups ((,class (:foreground ,aqua :background nil :slant normal))))
     (message-separator ((,class (:foreground ,purple))))

     ;; Jabber
     (jabber-chat-prompt-local ((,class (:foreground ,yellow))))
     (jabber-chat-prompt-foreign ((,class (:foreground ,orange))))
     (jabber-chat-prompt-system ((,class (:foreground ,yellow :weight bold))))
     (jabber-chat-text-local ((,class (:foreground ,yellow))))
     (jabber-chat-text-foreign ((,class (:foreground ,orange))))
     (jabber-chat-text-error ((,class (:foreground ,red))))

     (jabber-roster-user-online ((,class (:foreground ,green))))
     (jabber-roster-user-xa ((,class :foreground ,comment)))
     (jabber-roster-user-dnd ((,class :foreground ,yellow)))
     (jabber-roster-user-away ((,class (:foreground ,orange))))
     (jabber-roster-user-chatty ((,class (:foreground ,purple))))
     (jabber-roster-user-error ((,class (:foreground ,red))))
     (jabber-roster-user-offline ((,class (:foreground ,comment))))

     (jabber-rare-time-face ((,class (:foreground ,comment))))
     (jabber-activity-face ((,class (:foreground ,purple))))
     (jabber-activity-personal-face ((,class (:foreground ,aqua))))

     ;; Gnus
     (gnus-cite-1 ((,class (:inherit outline-1 :foreground nil))))
     (gnus-cite-2 ((,class (:inherit outline-2 :foreground nil))))
     (gnus-cite-3 ((,class (:inherit outline-3 :foreground nil))))
     (gnus-cite-4 ((,class (:inherit outline-4 :foreground nil))))
     (gnus-cite-5 ((,class (:inherit outline-5 :foreground nil))))
     (gnus-cite-6 ((,class (:inherit outline-6 :foreground nil))))
     (gnus-cite-7 ((,class (:inherit outline-7 :foreground nil))))
     (gnus-cite-8 ((,class (:inherit outline-8 :foreground nil))))
     ;; there are several more -cite- faces...
     (gnus-header-content ((,class (:inherit header-line :foreground nil :background nil :weight normal))))
     (gnus-header-subject ((,class (:inherit gnus-header-content :weight bold :foreground ,yellow))))
     (gnus-header-from ((,class (:inherit gnus-header-content :weight bold :foreground ,orange))))
     (gnus-header-name ((,class (:inherit header-line :foreground ,green :background nil))))
     (gnus-button ((,class (:inherit link :foreground nil))))
     (gnus-signature ((,class (:inherit font-lock-comment-face))))

     (gnus-summary-normal-unread ((,class (:foreground ,blue :weight normal))))
     (gnus-summary-normal-read ((,class (:foreground ,foreground :weight normal))))
     (gnus-summary-normal-ancient ((,class (:foreground ,aqua :weight normal))))
     (gnus-summary-normal-ticked ((,class (:foreground ,orange :weight normal))))
     (gnus-summary-low-unread ((,class (:foreground ,comment :weight normal))))
     (gnus-summary-low-read ((,class (:foreground ,comment :weight normal))))
     (gnus-summary-low-ancient ((,class (:foreground ,comment :weight normal))))
     (gnus-summary-high-unread ((,class (:foreground ,yellow :weight normal))))
     (gnus-summary-high-read ((,class (:foreground ,green :weight normal))))
     (gnus-summary-high-ancient ((,class (:foreground ,green :weight normal))))
     (gnus-summary-high-ticked ((,class (:foreground ,orange :weight normal))))
     (gnus-summary-cancelled ((,class (:foreground ,red :background nil :weight normal))))

     (gnus-group-mail-low ((,class (:foreground ,comment))))
     (gnus-group-mail-low-empty ((,class (:foreground ,comment))))
     (gnus-group-mail-1 ((,class (:foreground nil :weight normal :inherit outline-1))))
     (gnus-group-mail-2 ((,class (:foreground nil :weight normal :inherit outline-2))))
     (gnus-group-mail-3 ((,class (:foreground nil :weight normal :inherit outline-3))))
     (gnus-group-mail-4 ((,class (:foreground nil :weight normal :inherit outline-4))))
     (gnus-group-mail-5 ((,class (:foreground nil :weight normal :inherit outline-5))))
     (gnus-group-mail-6 ((,class (:foreground nil :weight normal :inherit outline-6))))
     (gnus-group-mail-1-empty ((,class (:inherit gnus-group-mail-1 :foreground ,comment))))
     (gnus-group-mail-2-empty ((,class (:inherit gnus-group-mail-2 :foreground ,comment))))
     (gnus-group-mail-3-empty ((,class (:inherit gnus-group-mail-3 :foreground ,comment))))
     (gnus-group-mail-4-empty ((,class (:inherit gnus-group-mail-4 :foreground ,comment))))
     (gnus-group-mail-5-empty ((,class (:inherit gnus-group-mail-5 :foreground ,comment))))
     (gnus-group-mail-6-empty ((,class (:inherit gnus-group-mail-6 :foreground ,comment))))

     (erc-direct-msg-face ((,class (:foreground ,orange))))
     (erc-error-face ((,class (:foreground ,red))))
     (erc-header-face ((,class (:foreground ,foreground :background ,selection))))
     (erc-input-face ((,class (:foreground ,green))))
     (erc-keyword-face ((,class (:foreground ,yellow))))
     (erc-current-nick-face ((,class (:foreground ,green))))
     (erc-my-nick-face ((,class (:foreground ,green))))
     (erc-nick-default-face ((,class (:weight normal :foreground ,purple))))
     (erc-nick-msg-face ((,class (:weight normal :foreground ,yellow))))
     (erc-notice-face ((,class (:foreground ,comment))))
     (erc-pal-face ((,class (:foreground ,orange))))
     (erc-prompt-face ((,class (:foreground ,blue))))
     (erc-timestamp-face ((,class (:foreground ,aqua))))

     (custom-variable-tag ((,class (:foreground ,blue))))
     (custom-group-tag ((,class (:foreground ,blue))))
     (custom-state ((,class (:foreground ,green))))
     )))

(defmacro color-theme-rm--define-theme ()
  (let ((name 'rm)
        (doc "A theme to match the default colors used in rm."))
    `(progn
       (deftheme ,name ,doc)
       (color-theme-rm--with-colors
        (apply 'custom-theme-set-faces ',name
               (color-theme-rm--face-specs))
        (custom-theme-set-variables
         ',name
         `(fci-rule-color ,current-line)
         `(ansi-color-names-vector (vector ,foreground ,red ,green ,yellow ,blue ,purple ,aqua ,background))
         '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])))
       (provide-theme ',name))))


;;;###autoload
(defun color-theme-rm ()
  (interactive)
  (if (fboundp 'load-theme)
      (if (> emacs-major-version 23)
	  (load-theme "rm" t)
	(load-theme "rm"))))

;;;###autoload
(when (boundp 'custom-theme-load-path)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name)))
  (color-theme-rm--define-theme))

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; color-theme-rm.el ends here
