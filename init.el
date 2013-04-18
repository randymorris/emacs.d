(add-to-list 'load-path "~/.emacs.d/site-lisp")
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'exec-path "~/bin")

(require 'rm-defs)
(require 'rm-auth)
(load "~/.emacs.d/rm-theme.el")

;; display
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; behavior
(setq inhibit-startup-screen t
      require-final-newline t
      scroll-conservatively 1
      backup-inhibited t
      auto-save-list-file-prefix "~/.emacs.d/tmp/autosaves/"
      url-configuration-directory "~/.emacs.d/tmp/url/")

;; disabled commands
(put 'narrow-to-region 'disabled nil)

;; tabs
(setq-default indent-tabs-mode nil)

;; swap meta and command on OSX
(setq mac-option-modifier 'super
      mac-command-modifier 'meta)

;; customize
(setq custom-file "~/.emacs.d/custom.el")
(load-file custom-file)

;; package.el setup
(require 'package)
(setq package-user-dir "~/.emacs.d/vendor/")
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; package configuration
(require 'configure-package)

(configure-package expand-region
  "Work more easily with semantic regions of text"
  :bind (("C-'" . er/expand-region)
         ("C-;" . er/contract-region)))

(configure-package change-inner
  "Convenience functions for changing delimited blocks of text"
  :require (expand-region)
  :bind (("M-i" . change-inner)
         ("M-o" . change-outer)))

(configure-package hippie-exp
  "Extensive list of completion methods"
  :bind (("M-SPC" . hippie-expand)))

(configure-package ido
  "Nicer method of visiting files and selecting buffers"
  :init (ido-mode 1)
  :after (setq ido-save-directory-list-file "~/.emacs.d/tmp/ido"
               ido-enable-flex-matching t
               ido-ignore-buffers '("\\` " "\\\*Messages\\\*" "\\\*Completions\\\*" "\\\*Help\\\*")
               ido-decorations (append `(,(format "\n%s " (string #x25ba)) "" "\n  " "\n  ...") (nthcdr 4 ido-decorations))))

(configure-package saveplace
  "Restore point position when revisiting a file"
  :init (require 'saveplace)
  :after (progn
           (setq-default save-place t)
           (setq save-place-file "~/.emacs.d/tmp/places")))

(configure-package tramp-term
  "Transparent tramp + ansi-term integration"
  :init (defalias 'ssh 'tramp-term)
  :autoload (tramp-term))

(configure-package rcirc
  "IRC client"
  :init (progn
          (setq rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY")
                rcirc-fill-column 'frame-width
                rcirc-buffer-maximum-lines 250
                rcirc-server-alist `((,(plist-get rm-auth-irc :host)
                                      :port ,(plist-get rm-auth-irc :port)
                                      :nick ,(plist-get rm-auth-irc :nick)
                                      :password ,(plist-get rm-auth-irc :password)
                                      :encryption tls)))
          (add-hook 'rcirc-mode-hook
                    (lambda ()
                      (set (make-local-variable 'scroll-conservatively) 999)
                      (rcirc-omit-mode)
                      (rcirc-track-minor-mode)
                      (flyspell-mode 1))))
  :after (progn
           (defvar rcirc-max-nick-length 10 "Max length for right aligned nicks.")

           (defun rcirc-markup-custom-format-nicks (sender response)
             "Format and right align nicks, truncating at `rcirc-max-nick-length'."
             (goto-char (point-min))
             (string-match "^..:.. <\\([^>]+\\)>" (buffer-string))
             (let* ((length rcirc-max-nick-length)
                    (nick (match-string 1 (buffer-string)))
                    (nick* (format (format "%%%ds" length) nick))
                    (nick* (truncate-string-to-width nick* length)))
               (while (search-forward (format "<%s>" nick) nil t)
                 (replace-match (format "%s %s" nick* (string #x2237)) nil t))))

           (add-to-list 'rcirc-markup-text-functions 'rcirc-markup-custom-format-nicks)))

(configure-package tramp
  "Transparent Remote Access"
  :init (setq tramp-default-method "ssh"
              tramp-persistency-file-name "~/.emacs.d/tmp/tramp"))

(configure-package python
  "fgallina's python.el"
  :after (progn
           (define-key python-mode-map (kbd "RET") 'newline-and-indent)
           (add-hook 'python-mode-hook
                     '(lambda ()
                        "Indent with tabs if this file already uses tabs for indentation."
                        (save-excursion
                          (goto-char (point-max))
                          (while (and (re-search-backward "^\\s-" nil t)
                                      (python-syntax-comment-or-string-p)))
                          (setq-local indent-tabs-mode (eql (char-after) ?\t)))))

           (defvar python-outline-regex "^\\(if \\|\\s-*?\\(class\\|def\\)\\)")
           (defun python-occur-outline ()
             "Displays a clickable rudimentary outline using occur"
             (interactive)
             (occur python-outline-regex)
             (save-excursion
               (set-buffer (get-buffer "*Occur*"))
               (goto-char (point-min))
               (let ((inhibit-read-only t))
                 (when (looking-at "^[0-9]+ match\\(es\\) for \"")
                   (zap-to-char 1 ?:)
                   (insert "# Outline:"))
                 (forward-line 1)
                 (set-text-properties (point-min) (point) '(face font-lock-comment-face))
                 (while (re-search-forward "^[ \t]*[0-9]+:" (point-max) t)
                   (replace-match "")
                   (forward-line 1))

                 (goto-char (point-min))
                 (forward-line 1)
                 (while (re-search-forward python-outline-regex (point-max) t)
                   (set-text-properties (line-beginning-position) (point)
                                        '(face font-lock-keyword-face)))
                 (goto-char (point-min))
                 (forward-line 1)
                 (while (re-search-forward "^[^ \t]" (point-max) t)
                   (beginning-of-line)
                   (newline)
                   (forward-line 1)))))

           (define-key python-mode-map (kbd "C-c o") 'python-occur-outline)))

(configure-package ps-print
  "Pretty printing"
  :after (setq ps-number-of-columns 2
               ps-landscape-mode t
               ps-header-font-size '(8.5 . 10)
               ps-font-size '(6 . 7.5)
               ps-print-color-p 'black-white
               ps-header-offset 14
               ps-inter-column 40
               ps-left-margin 40
               ps-right-margin 40))

(configure-package shell
  "Shell inside Emacs"
  :after (define-key shell-mode-map (kbd "C-l")
           '(lambda ()
              (interactive)
              (let ((comint-buffer-maximum-size 0))
                (comint-truncate-buffer)))))

(configure-package ack
  "Replacement for M-x find-grep"
  :init (defalias 'ag 'ack)
  :after (setq ack-command (concat (cond ((executable-find "ag"))
                                         ((executable-find "ack-grep"))
                                         ((executable-find "ack"))) " ")))

(configure-package eshell
  "Shell written in Elisp"
  :after (progn
           (setq eshell-history-file-name "~/.emacs.d/tmp/eshell-history")
           (defun eshell/ack (arg)
             (require 'ack)
             (ack (format "%s %s" ack-command arg)))
           (defalias 'eshell/ag 'eshell/ack)))

;; load machine-specific configuration
(load "~/.emacs.d/local-configuration.el" t)
