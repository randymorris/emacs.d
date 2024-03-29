;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(setq use-package-always-ensure t)

(when (string-equal system-type "darwin")
  (use-package exec-path-from-shell
    :init (exec-path-from-shell-initialize)))

;; Remove minor-mode cruft from the status line
(use-package diminish)

(use-package hydra
  ;; Easy repeating keybinds
  :init (require 'hydra))

(use-package recentf
  ;; Stores recent files for easy access
  :config (setq recentf-save-file (locate-user-emacs-file "tmp/recent-files")))

(use-package js2-mode
  ;; A better javascript mode
  :config (setq js2-global-externs '("require" "module" "jest" "jasmine"
                                     "it" "expect" "describe" "beforeEach")))

(use-package python
  ;; fgallina's python mode
  :mode ("\\.py\\'" . python-mode)
  :bind (:map python-mode-map ("RET" . newline-and-indent))
  :config (progn
            (defun rm-fix-python-tab-width ()
              (setq tab-width 4
                    python-indent-offset 4))
            (add-hook 'python-mode-hook 'rm-fix-python-tab-width)))

(use-package whitespace
  ;; Display whitespace as meaningful characters
  :diminish whitespace-mode
  ;; :init (add-hook 'prog-mode-hook (lambda () (whitespace-mode 1)))
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
  :ensure nil
  :init (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(use-package org
  ;; Org mode
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
  :requires hydra
  :init (setq mc/list-file (locate-user-emacs-file "tmp/mc-lists.el"))
  :config (defhydra rm-multiple-cursors-hydra
            (rm-map "n" :hint nil)
              "
     ^Up^            ^Down^        ^Other^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regexp
^ ^             ^ ^             [_q_] Quit
"
              ("l" mc/edit-lines :exit t)
              ("a" mc/mark-all-like-this :exit t)
              ("n" mc/mark-next-like-this)
              ("N" mc/skip-to-next-like-this)
              ("M-n" mc/unmark-next-like-this)
              ("p" mc/mark-previous-like-this)
              ("P" mc/skip-to-previous-like-this)
              ("M-p" mc/unmark-previous-like-this)
              ("r" mc/mark-all-in-region-regexp :exit t)
              ("q" nil)))

(use-package expand-region
  ;; Incrementally mark semantic blocks of text
  :bind (:map rm-map ("e" . rm-expand-region-hydra/body))
  :config (progn
            (setq expand-region-fast-keys-enabled nil)
            (defhydra rm-expand-region-hydra
              (:body-pre (er/expand-region 1))
              "Expand region"
              ("e" er/expand-region)
              ("r" er/contract-region)
              ("q" nil))))

(use-package ack
  ;; Replacement for M-x find-grep
  :init (defalias 'ag 'ack)
  :bind (:map rm-map ("a" . rm-ack-symbol-in-project))
  :config (progn
            (defun rm-ack-symbol-in-project ()
              (interactive)
              (let ((target (if (use-region-p)
                                (buffer-substring-no-properties (region-beginning) (region-end))
                              (thing-at-point 'symbol t)))
                    (root (ack-default-directory 4)))
                (ack (format "%s %s %s" ack-command target root))))

            (setq ack-command
                  (concat (cond ((executable-find "ag"))
                                ((executable-find "ack-grep"))
                                ((executable-find "ack"))) " "))))

(use-package ps-print
  ;; Pretty printing
  :config (setq ps-number-of-columns 2
                ps-landscape-mode t
                ps-header-title-font-size '(8 . 10)
                ps-header-font-size '(6 . 8.5)
                ps-font-size '(6 . 7.5)
                ps-print-color-p 'black-white
                ps-header-offset 14
                ps-inter-column 40
                ps-left-margin 40
                ps-right-margin 40))

(use-package tramp
  ;; Transparent Remote Access
  :config (setq tramp-default-method "ssh"
                tramp-use-ssh-controlmaster-options nil
                tramp-persistency-file-name (locate-user-emacs-file "tmp/tramp")
                tramp-shell-prompt-pattern
                "\\(?:^\\|\\)[^]#$%>\n]*#?[]#$%>]+ *\\(\\[[0-9;]*[a-zA-Z] *\\)*"
                tramp-password-prompt-regexp
                "^.*\\([pP]assword\\|[pP]assphrase\\|PASSCODE\\).*:"))

(use-package saveplace
  ;; Restore point position when revisiting a file
  :ensure nil
  :init (save-place-mode t)
  :config (progn
            (setq-default save-place t)
            (setq save-place-file (locate-user-emacs-file "tmp/places"))))

(use-package savehist
  ;; Restore minibuffer history
  :ensure nil
  :init (savehist-mode 1)
  :config (setq savehist-file (locate-user-emacs-file "tmp/history")))

(use-package hippie-exp
  ;; Extensive list of completion methods
  :ensure nil
  :bind (("M-/" . hippie-expand)))

(use-package magit
  :bind (:map rm-map ("s" . magit-status))
  :config (progn
            (setq magit-status-buffer-switch-function 'switch-to-buffer
                  magit-save-some-buffers t
                  magit-process-popup-time 10
                  magit-diff-refine-hunk 'all
                  magit-section-visibility-indicator nil
                  magit-last-seen-setup-instructions "1.4.0")

            (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)

            (defadvice magit-status (around magit-fullscreen activate)
              (window-configuration-to-register :magit-fullscreen)
              ad-do-it
              (delete-other-windows))

            (defadvice magit-quit-window (after magit-restore-screen activate)
              (jump-to-register :magit-fullscreen))))

(use-package magit-svn)

(use-package flycheck
  ;; Syntax checking on the fly
  :requires hydra
  :bind (:map rm-map ("F" . rm-flycheck-hydra/body))
  :config (defhydra rm-flycheck-hydra
            (:body-pre (flycheck-next-error 1))
            "Flycheck"
            ("n" flycheck-next-error "Next")
            ("p" flycheck-previous-error "Previous")))

;; Provides fuzzy matching for ivy completion
(use-package flx)

(use-package ivy
  ;; Better completion than ido, simliar to ido-vertical-mode
  :requires flx
  :diminish ivy-mode
  :init (ivy-mode 1)
  :bind (:map ivy-minibuffer-map ("C-m" . ivy-alt-done))
  :config (setq ivy-count-format ""
                ivy-display-style nil
                ivy-minibuffer-faces nil
                ivy-use-virtual-buffers t
                ivy-re-builders-alist '((t . ivy--regex-fuzzy))))

(use-package ivy-hydra
  :requires (ivy hydra))

;; Used for counsel-M-x to show most recently used commands
(use-package smex)

(use-package markdown-mode)
(use-package yaml-mode)
(use-package json-mode)

(use-package counsel
  :requires (ivy smex)
  :bind (("M-x" . counsel-M-x))
  :config (setq smex-save-file (locate-user-emacs-file "tmp/smex-items")))

(use-package projectile
  ;; Project-specific navigation
  :requires ivy
  :config (setq projectile-completion-system 'ivy))

(use-package counsel-projectile
  :bind (:map rm-map
              ("C-f" . counsel-projectile-find-file)
              ("C-b" . counsel-projectile-switch-to-buffer)))

(use-package dumb-jump
  :bind (:map rm-map
              ("." . dumb-jump-go)
              ("," . dumb-jump-back))
  :config (setq dumb-jump-selector 'ivy))

(use-package transient
  :config (setq transient-history-file (locate-user-emacs-file "tmp/transient-history.el")))


(use-package python-black
  :init (setq python-black-program (locate-user-emacs-file "python_env/bin/black"))
  :demand t
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))


(use-package nano
  :load-path "lisp/nano-emacs"
  :config (progn
            (setq-default shell-file-name "/bin/bash")
            (setq explicit-shell-file-name "/bin/bash")))

(use-package vterm
  :config (setq vterm-kill-buffer-on-exit t))

(use-package multi-vterm
  :requires vterm
  :bind (:map rm-map
              ("t" . multi-vterm-next)
              ("C-t" . multi-vterm))
  :config (setq multi-vterm-buffer-name "vterm"))

(provide 'rm-packages)
