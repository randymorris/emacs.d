(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/site-lisp")

;; display
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)

;; behavior
(setq backup-inhibited t
      custom-file "~/.emacs.d/custom.el"
      mac-command-modifier 'meta
      mac-option-modifier 'super
      mouse-yank-at-point t
      require-final-newline t
      scroll-conservatively 1
      inhibit-startup-screen t)

;; default indentation
(setq-default indent-tabs-mode nil
              tab-width 4)
(add-hook 'prog-mode-hook
          #'(lambda ()
              (save-excursion
                (goto-char (point-max))
                (while (and (re-search-backward "^\\s-" nil t)
                            (null (nth 8 (syntax-ppss)))))
                (setq-local indent-tabs-mode (eql (char-after) ?\t)))))

;; temp files
(setq auto-save-list-file-prefix "~/.emacs.d/tmp/autosaves/"
      url-configuration-directory "~/.emacs.d/tmp/url/")

;; enable disabled commands
(put 'narrow-to-region 'disabled nil)

;; create my own keymap for bindings
(define-prefix-command 'rm-map)
(global-set-key (kbd "C-h") 'rm-map)

;; preserve commonly-used help-map bindings
(define-key rm-map (kbd "k") 'describe-key)
(define-key rm-map (kbd "v") 'describe-variable)
(define-key rm-map (kbd "f") 'describe-function)
(define-key rm-map (kbd "w") 'where-is)

(load custom-file t)
(load "~/.emacs.d/local-configuration.el" t)

;; package configuration
(require 'package)
(setq package-user-dir "~/.emacs.d/vendor/")
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(require 'rm-packages)

(load "~/.emacs.d/local-packages.el" t)
