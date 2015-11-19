;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package sublime-themes
  ;; Needed for wilson theme
  :ensure t
  :init (progn
          (load-theme 'wilson t)
          (load-theme 'rm-wilson t)))

(use-package diminish
  ;; Remove minor-mode cruft from the status line
  :ensure t)

(use-package smartrep
  ;; Easy repeating keybinds
  :ensure t
  :init (require 'smartrep))

(use-package recentf
  ;; Stores recent files for easy access
  :ensure t
  :config (setq recentf-save-file "~/.emacs.d/tmp/recent-files"))

(use-package term
  ;; term and ansi-term
  :config (progn
            (defun rm-ansi-term ()
              (interactive)
              (let ((buffer (get-buffer "*ansi-term*")))
                (if buffer
                    (switch-to-buffer buffer)
                  (ansi-term (getenv "SHELL")))))
            ;; temp hack until emacs:beaab89896 is reverted
            (setenv "EMACS" (format "%s (term:%s)" emacs-version term-protocol-version))
            (define-key rm-map (kbd "t") 'rm-ansi-term)
            (define-key term-raw-map (kbd "C-h") nil)
            (define-key term-raw-map (kbd "M-x") nil)))

(use-package js2-mode
  ;; A better javascript mode
  :ensure t
  :config (setq js2-global-externs '("require" "module" "jest" "jasmine"
                                     "it" "expect" "describe" "beforeEach")))

(use-package web-mode
  :ensure t
  :requires js2-mode
  :mode "\\.js\\'\\|\\.html\\'"
  :config (progn
            (setq web-mode-attr-indent-offset 4)
            (defun rm-maybe-jsx-mode ()
              (when (string-equal "jsx" web-mode-content-type)
                (subword-mode 1)
                (js2-minor-mode 1)))
            (add-hook 'web-mode-hook 'rm-maybe-jsx-mode)
            (add-to-list 'web-mode-content-types '("jsx" . "jsx/.*\\.js\\'"))))

(use-package jinja2-mode
  ;; Support for jinja-style templates
  :ensure t
  :mode "\\.jinja\\|\\.html\\'")

(use-package python
  ;; fgallina's python mode
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :config (progn
            (define-key python-mode-map (kbd "RET") 'newline-and-indent)
            (defun rm-fix-python-tab-width ()
              (setq tab-width 4
                    python-indent-offset 4))
            (add-hook 'python-mode-hook 'rm-fix-python-tab-width)))

(use-package whitespace
  ;; Display whitespace as meaningful characters
  :ensure t
  :diminish whitespace-mode
  :init (add-hook 'prog-mode-hook (lambda () (whitespace-mode 1)))
  :config (setq whitespace-style
                '(face tabs spaces trailing
                       space-before-tab newline
                       space-mark tab-mark newline-mark)
                whitespace-display-mappings
                '((space-mark ?\s [?·])
                  (newline-mark ?\n [?⏎ ?\n])
                  (tab-mark ?\t [?⇥ ?\t]))))

(use-package uniquify
  ;; Better display duplicate buffer names
  :init (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(use-package org
  ;; Org mode
  :ensure t
  :config (progn
            (setq org-startup-indented t
                  org-default-notes-file "~/org/todo.org"
                  org-archive-location "~/org/archive.org::* From %s"
                  org-agenda-files '("~/org/todo.org" "~/org/meetings/")
                  org-plantuml-jar-path (expand-file-name "~/bin/plantuml.jar")
                  org-src-fontify-natively t
                  org-todo-keywords '((sequence "TODO" "IN PROGRESS" "|" "DONE")
                                      (sequence "|" "CANCELED"))
                  org-todo-keyword-faces '(("IN PROGRESS" . "#fff68f"))
                  org-agenda-custom-commands
                  '(("u" "Agenda and all unscheduled TODO's"
                     ((agenda "")
                      (todo "" ((org-agenda-todo-ignore-scheduled t)
                                (org-agenda-todo-ignore-deadlines t))))))
                  org-agenda-sorting-strategy
                  '((agenda habit-down time-up priority-down category-keep todo-state-down)
                    (todo priority-down category-keep todo-state-down)
                    (tags priority-down category-keep)
                    (search category-keep)))

            (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))))

(use-package org-capture
  ;; Quickly make notes to reference later
  :requires org
  :config (progn
            (setq org-capture-templates
                  '(("t" "Todo" entry (file org-default-notes-file))))
            (define-key rm-map (kbd "c") 'org-capture)))

(use-package multiple-cursors
  ;; Run commands on multiple parts of the buffer simultaniously
  :ensure t
  :config (smartrep-define-key global-map "C-h"
            '(("n" . 'mc/mark-next-like-this)
              ("s" . 'mc/skip-to-next-like-this)
              ("p" . 'mc/unmark-next-like-this))))

(use-package expand-region
  ;; Incrementally mark semantic blocks of text
  :ensure t
  :config (define-key rm-map "e" 'er/expand-region))

(use-package ack
  ;; Replacement for M-x find-grep
  :ensure t
  :init (defalias 'ag 'ack)
  :config (progn
            (defun rm-ack-symbol-in-project ()
              (interactive)
              (let ((target (thing-at-point 'symbol t))
                    (root (ack-default-directory 4)))
                (ack (format "%s %s %s" ack-command target root))))

            (define-key rm-map (kbd "a") 'rm-ack-symbol-in-project)

            (setq ack-command
                  (concat (cond ((executable-find "ag"))
                                ((executable-find "ack-grep"))
                                ((executable-find "ack"))) " "))))

(use-package ps-print
  ;; Pretty printing
  :ensure t
  :config (setq ps-number-of-columns 2
                ps-landscape-mode t
                ps-header-font-size '(8.5 . 10)
                ps-font-size '(6 . 7.5)
                ps-print-color-p 'black-white
                ps-header-offset 14
                ps-inter-column 40
                ps-left-margin 40
                ps-right-margin 40))

(use-package tramp
  ;; Transparent Remote Access
  :ensure t
  :config (setq tramp-default-method "ssh"
                tramp-use-ssh-controlmaster-options nil
                tramp-persistency-file-name "~/.emacs.d/tmp/tramp"
                tramp-shell-prompt-pattern
                "\\(?:^\\|\\)[^]#$%>\n]*#?[]#$%>]+ *\\(\\[[0-9;]*[a-zA-Z] *\\)*"
                tramp-password-prompt-regexp
                "^.*\\([pP]assword\\|[pP]assphrase\\|PASSCODE\\).*:"))

(use-package tramp-term
  ;; Create remote ansi-terms that automatically track pwd
  :ensure t
  :init (defalias 'ssh 'tramp-term)
  :commands tramp-term)

(use-package saveplace
  ;; Restore point position when revisiting a file
  :init (save-place-mode t)
  :config (progn
            (setq-default save-place t)
            (setq save-place-file "~/.emacs.d/tmp/places")))

(use-package savehist
  ;; Restore minibuffer history
  :init (savehist-mode 1)
  :config (setq savehist-file "~/.emacs.d/tmp/history"))

(use-package hippie-exp
  ;; Extensive list of completion methods
  :bind (("M-/" . hippie-expand)))

(use-package magit
  :ensure t
  :config (progn
            (setq magit-status-buffer-switch-function 'switch-to-buffer
                  magit-save-some-buffers t
                  magit-process-popup-time 10
                  magit-diff-refine-hunk 'all
                  magit-last-seen-setup-instructions "1.4.0")

            (defun rm-magit-status ()
              (interactive)
              (magit-status)
              (delete-other-windows))

            (define-key rm-map (kbd "s") 'rm-magit-status)))

(use-package magit-svn
  :ensure t)

(use-package flycheck
  ;; Syntax checking on the fly
  :ensure t
  :config (progn
            (smartrep-define-key rm-map "F"
              '(("n" . 'flycheck-next-error)
                ("p" . 'flycheck-previous-error)))
            (setq flycheck-flake8rc "~/.config/flake8rc")))

(use-package sane-term
  ;; Quickly cycle term-mode buffers
  :ensure t
  :init (require 'sane-term)
  :config (progn
            (setq sane-term-next-on-kill nil)
            (define-key rm-map (kbd "t") 'sane-term)
            (define-key rm-map (kbd "C-t") 'sane-term-create)))

(use-package elec-pair
  ;; Auto-insert matching pairs
  :init (electric-pair-mode t))

(use-package helm
  ;; Alternate completing-read implementaion + helper functions for
  ;; opening files and buffers.
  :ensure t
  :diminish ""
  :init (progn
          (helm-mode 1)
          (define-key ctl-x-map (kbd "C-f") 'helm-find-files)
          (define-key ctl-x-map (kbd "b") 'helm-mini)
          (define-key global-map (kbd "M-x") 'helm-M-x))
  :config (progn
            (define-key helm-map (kbd "s-SPC") 'helm-toggle-visible-mark)

            (defun helm-hide-minibuffer-maybe ()
              (when (with-helm-buffer helm-echo-input-in-header-line)
                (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
                  (overlay-put ov 'window (selected-window))
                  (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                                          `(:background ,bg-color :foreground ,bg-color)))
                  (setq-local cursor-type nil))))

            (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

            (setq helm-display-header-line nil
                  helm-echo-input-in-header-line t
                  helm-truncate-lines t
                  helm-autoresize-max-height 30
                  helm-autoresize-min-height 30
                  helm-M-x-fuzzy-match t
                  helm-buffers-fuzzy-matching t
                  helm-recentf-fuzzy-match t
                  helm-ff-skip-boring-files t
                  helm-ff-auto-update-initial-value t
                  helm-boring-buffer-regexp-list
                  '("\\` "           "\\*helm"          "\\*helm-mode"
                    "\\*Echo Area"   "\\*Minibuf"       "\\*Messages"
                    "\\*Compile-Log" "\\*Completions"   "\\*Help"
                    "\\*Buffer List" "\\*magit-process" "\\*tramp"))
            (helm-autoresize-mode 1)))

(use-package helm-projectile
  ;; Projectile integration for helm
  :ensure t
  :requires helm
  :init (define-key rm-map (kbd "C-f") 'helm-projectile))

(provide 'rm-packages)
