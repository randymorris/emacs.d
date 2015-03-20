;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package sublime-themes
  ;; Needed for wilson theme
  :ensure t
  :init (load-theme 'wilson t)
  :config (let ((fg "#333333") (bg "#222222"))
            (set-face-attribute 'default nil :height 140)

            (set-face-attribute 'region nil :background "#cfb980")

            ;; whitespace
            (require 'whitespace)
            (set-face-attribute 'whitespace-newline nil :background bg :foreground fg)
            (set-face-attribute 'whitespace-space nil :background bg :foreground fg)
            (set-face-attribute 'whitespace-tab nil :background bg :foreground fg)

            (require 'term)
            (set-face-attribute 'term-color-black   nil :foreground "#888a85")
            (set-face-attribute 'term-color-red     nil :foreground "tomato")
            (set-face-attribute 'term-color-green   nil :foreground "#8ae234")
            (set-face-attribute 'term-color-yellow  nil :foreground "#edd400")
            (set-face-attribute 'term-color-blue    nil :foreground "#729fcf")
            (set-face-attribute 'term-color-magenta nil :foreground "#ad7fa8")
            (set-face-attribute 'term-color-cyan    nil :foreground "pale-turquoise")
            (set-face-attribute 'term-color-white   nil :foreground "#eeeeec")

            ;; mode-line
            (set-face-attribute 'mode-line nil :background "#303030" :foreground "#cfb980" :overline "#44443c" :underline "#44443c")
            (set-face-attribute 'mode-line-buffer-id nil :weight 'normal)))

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
            (define-key rm-map (kbd "t") 'rm-ansi-term)
            (define-key term-raw-map (kbd "C-h") nil)
            (define-key term-raw-map (kbd "M-x") nil)))

(use-package js2-mode
  ;; A better javascript mode
  :ensure t
  :mode "\\.js\\|\\.jsx\\'"
  :config (progn
            (setq-default js2-basic-offset 2)
            (add-hook 'js2-mode-hook 'subword-mode)))

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

(use-package ace-jump-mode
  ;; Precisely place point in a few keystrokes
  :ensure t
  :config (define-key rm-map (kbd "C-h") 'ace-jump-char-mode))

(use-package ack
  ;; Replacement for M-x find-grep
  :ensure t
  :init (defalias 'ag 'ack)
  :config (setq ack-command
                (concat (cond ((executable-find "ag"))
                              ((executable-find "ack-grep"))
                              ((executable-find "ack"))) " ")))

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
                tramp-persistency-file-name "~/.emacs.d/tmp/tramp"
                tramp-shell-prompt-pattern
                "\\(?:^\\|\\)[^]#$%>\n]*#?[]#$%>]+ *\\(\\[[0-9;]*[a-zA-Z] *\\)*"))

(use-package tramp-term
  ;; Create remote ansi-terms that automatically track pwd
  :ensure t
  :init (defalias 'ssh 'tramp-term)
  :commands tramp-term)

(use-package saveplace
  ;; Restore point position when revisiting a file
  :init (require 'saveplace)
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
                  magit-process-popup-time 10)
            (define-key rm-map (kbd "s") 'magit-status)))

(use-package magit-svn
  :ensure t
  :init (progn
          (require 'magit-svn)
          (defun init-magit-svn-mode-maybe ()
            (if (magit-svn-get-ref-info)
                (magit-svn-mode)))
          (add-hook 'magit-mode-hook 'init-magit-svn-mode-maybe)))

(use-package flycheck
  ;; Syntax checking on the fly
  :ensure t
  :config (smartrep-define-key rm-map "F"
            '(("n" . 'flycheck-next-error)
              ("p" . 'flycheck-previous-error))))

(use-package sane-term
  ;; Quickly cycle term-mode buffers
  :ensure t
  :init (require 'sane-term)
  :config (progn
            (setq sane-term-next-on-kill nil)
            (define-key rm-map (kbd "t") 'sane-term)
            (define-key rm-map (kbd "C-t") 'sane-term-create)))

(provide 'rm-packages)
