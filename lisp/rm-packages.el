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
  :bind (:map term-raw-map
              ("C-h" . nil)
              ("M-x" . nil)))

(use-package js2-mode
  ;; A better javascript mode
  :ensure t
  :config (setq js2-global-externs '("require" "module" "jest" "jasmine"
                                     "it" "expect" "describe" "beforeEach")))

(use-package web-mode
  :ensure t
  :requires js2-mode
  :mode "\\.jsx?\\'\\|\\.html\\'"
  :config (progn
            (setq web-mode-attr-indent-offset 4
                  web-mode-code-indent-offset 2
                  web-mode-markup-indent-offset 2)
            (defun rm-maybe-jsx-mode ()
              (when (string-equal "jsx" web-mode-content-type)
                (subword-mode 1)))
            (add-hook 'web-mode-hook 'rm-maybe-jsx-mode)
            (add-hook 'web-mode-hook (lambda () (whitespace-mode -1)))
            (add-to-list 'web-mode-engine-file-regexps '("django" . "templates/.*\\.html\\'"))
            (add-to-list 'web-mode-content-types '("jsx" . ".*\\.jsx?\\'"))
            (add-to-list 'web-mode-content-types '("jsx" . "jsx/.*\\.js\\'"))))

(use-package python
  ;; fgallina's python mode
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :bind (:map python-mode-map ("RET" . newline-and-indent))
  :config (progn
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
                '((space-mark ?\s [?␣])
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

(use-package multiple-cursors
  ;; Run commands on multiple parts of the buffer simultaniously
  :ensure t
  :config (progn
            (smartrep-define-key global-map "C-h"
              '(("n" . 'mc/mark-next-like-this)
                ("s" . 'mc/skip-to-next-like-this)
                ("p" . 'mc/unmark-next-like-this)))
            (setq mc/list-file "~/.emacs/tmp/mc-lists.el")))

(use-package expand-region
  ;; Incrementally mark semantic blocks of text
  :ensure t
  :bind (:map rm-map ("e" . er/expand-region)))

(use-package ack
  ;; Replacement for M-x find-grep
  :ensure t
  :init (defalias 'ag 'ack)
  :bind (:map rm-map ("a" . rm-ack-symbol-in-project))
  :config (progn
            (defun rm-ack-symbol-in-project ()
              (interactive)
              (let ((target (thing-at-point 'symbol t))
                    (root (ack-default-directory 4)))
                (ack (format "%s %s %s" ack-command target root))))

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
  :bind (:map rm-map ("s" . rm-magit-status))
  :config (progn
            (setq magit-status-buffer-switch-function 'switch-to-buffer
                  magit-save-some-buffers t
                  magit-process-popup-time 10
                  magit-diff-refine-hunk 'all
                  magit-last-seen-setup-instructions "1.4.0")

            (defun rm-magit-status ()
              (interactive)
              (magit-status)
              (delete-other-windows))))

(use-package magit-svn
  :ensure t)

(use-package flycheck
  ;; Syntax checking on the fly
  :ensure t
  :config (progn
            (smartrep-define-key rm-map "F"
              '(("n" . 'flycheck-next-error)
                ("p" . 'flycheck-previous-error)))))

(use-package sane-term
  ;; Quickly cycle term-mode buffers
  :ensure t
  :init (require 'sane-term)
  :bind (:map rm-map
              ("t" . sane-term)
              ("C-t" . sane-term-create))
  :config (setq sane-term-next-on-kill nil))

(use-package elec-pair
  ;; Auto-insert matching pairs
  :init (electric-pair-mode t))


(use-package flx
  ;; Provides fuzzy matching for ivy completion
  :ensure t)

(use-package ivy
  ;; Better completion than ido, simliar to ido-vertical-mode
  :ensure t
  :requires flx
  :diminish ivy-mode
  :init (ivy-mode 1)
  :bind (:map ivy-minibuffer-map ("C-m" . ivy-alt-done))
  :config (setq ivy-count-format ""
                ivy-display-style nil
                ivy-minibuffer-faces nil
                ivy-use-virtual-buffers t
                ivy-re-builders-alist '((t . ivy--regex-fuzzy))))

(use-package counsel
  :ensure t
  :requires ivy
  :bind (("M-x" . counsel-M-x))
  :config (setq smex-save-file "~/.emacs.d/tmp/smex-items"))

(use-package projectile
  ;; Project-specific navigation
  :ensure t
  :requires ivy
  :bind (:map rm-map ("C-f" . projectile-find-file))
  :config (setq projectile-completion-system 'ivy))

(use-package avy
  :ensure t
  :diminish avy-mode
  :bind (:map rm-map
              ("l" . avy-goto-line)
              ("c" . avy-goto-char)
              ("w" . avy-goto-word-or-subword-1)))

(provide 'rm-packages)
