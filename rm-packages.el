;; ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package diminish
  ;; Remove minor-mode cruft from the status line
  :ensure t)

(use-package ido
  ;; Nicer file/buffer/etc. switching
  :ensure t
  :init (ido-mode 1)
  :config (setq ido-save-directory-list-file "~/.emacs.d/tmp/ido"
                ido-enable-flex-matching t
                ido-use-virtual-buffers t))

(use-package flx-ido
  ;; Flex-matching algorithm for ido
  :ensure t
  :init (flx-ido-mode 1)
  :config (setq flx-ido-use-faces nil))

(use-package ido-ubiquitous
  ;; Allow ido-style completion in more places
  :ensure t
  :requires ido
  :init (ido-ubiquitous-mode 1))

(use-package ido-vertical-mode
  ;; Display ido completions in a vertical list
  :ensure t
  :requires (ido ido-ubiquitous)
  :init (ido-vertical-mode 1))

(use-package recentf
  ;; Stores recent files for easy access
  :ensure t
  :config (setq recentf-save-file "~/.emacs.d/tmp/recent-files"))

(use-package smex
  ;; Smarter M-x with ido completion
  :ensure t
  :requires (recentf ido)
  :init (smex-initialize)
  :config (setq smex-save-file "~/.emacs.d/tmp/smex-items")
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c M-x" . execute-extended-command)))

(use-package js2-mode
  ;; A better javascript mode
  :ensure t
  :mode "\\.js\\'")

(use-package jinja2-mode
  ;; Support for jinja-style templates
  :ensure t
  :mode "\\.jinja\\|\\.html\\'")

(use-package python
  ;; fgallina's python modepp
  :ensure t
  :mode "\\.py\\'"
  :config
  (progn
    (define-key python-mode-map (kbd "RET") 'newline-and-indent)
    (add-hook 'python-mode-hook
              '(lambda ()
                 "Infer indentation settings"
                 (save-excursion
                   (goto-char (point-max))
                   (while (and (re-search-backward "^\\s-" nil t)
                               (python-syntax-comment-or-string-p)))
                   (setq-local indent-tabs-mode (eql (char-after) ?\t)))))))


(use-package whitespace
  ;; Display whitespace as meaningful characters
  :ensure t
  :diminish whitespace-mode
  :init (add-hook 'prog-mode-hook #'(lambda () (whitespace-mode 1)))
  :config
  (progn
    (setq whitespace-style
          '(face tabs spaces trailing
            space-before-tab newline
            space-mark tab-mark newline-mark)
          whitespace-display-mappings
          '((space-mark ?\s [?·])
            (newline-mark ?\n [?⏎ ?\n])
            (tab-mark ?\t [?⇥ ?\t])))))

(use-package uniquify
  ;; Better display duplicate buffer names
  :init (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(use-package org
  ;; Org mode
  :ensure t
  :config (setq org-startup-indented t
                org-default-notes-file "~/todo.org"
                org-agenda-files '("~/todo.org")))

(use-package multiple-cursors
  ;; Run commands on multiple parts of the buffer simultaniously
  :ensure t
  :bind (("C-c m" . mc/mark-next-like-this)
         ("C-c M" . mc/mark-all-like-this-dwim)))

(use-package ack
  ;; Replacement for M-x find-grep
  :ensure t
  :init (defalias 'ag 'ack)
  :config
  (setq ack-command
        (concat (cond ((executable-find "ag"))
                      ((executable-find "ack-grep"))
                      ((executable-find "ack"))) " ")))

(use-package ps-print
  ;; Pretty printing
  :ensure t
  :config
  (setq ps-number-of-columns 2
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
  :config
  (setq tramp-default-method "ssh"
        tramp-persistency-file-name "~/.emacs.d/tmp/tramp"))

(use-package tramp-term
  ;; Create remote ansi-terms that automatically track pwd
  :init (defalias 'ssh 'tramp-term)
  :commands tramp-term)

(use-package saveplace
  ;; Restore point position when revisiting a file
  :init (require 'saveplace)
  :config
  (progn
    (setq-default save-place t)
    (setq save-place-file "~/.emacs.d/tmp/places")))

(use-package savehist
  ;; Restore minibuffer history
  :init (savehist-mode 1)
  :config (setq savehist-file "~/.emacs.d/tmp/history"))

(use-package hippie-exp
  ;; Extensive list of completion methods
  :bind (("M-/" . hippie-expand)))

(use-package hideshow
  ;; Code folding
  :diminish hs-minor-mode
  :init (progn
          (defun rm-hs-toggle (arg)
            "Simple single-key folding"
            (interactive "p")
            (call-interactively
             (cond ((>= arg 16) 'hs-show-all)
                   ((>= arg 4) 'hs-hide-all)
                   (t 'hs-toggle-hiding))))
          (add-hook 'prog-mode-hook #'(lambda () (hs-minor-mode 1))))
  :bind ("C-z" . rm-hs-toggle))

(provide 'rm-packages)
