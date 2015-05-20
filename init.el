(add-to-list 'load-path "~/.emacs.d/lisp")

;; Display
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)

;; Behavior
(setq backup-inhibited t
      custom-file "~/.emacs.d/lisp/custom.el"
      mouse-yank-at-point t
      require-final-newline t
      scroll-conservatively 1
      inhibit-startup-screen t)

;; Default indentation
(setq-default indent-tabs-mode nil
              tab-width 4)


;; Platform specific settings
(when (string-equal system-type "darwin")
  (setenv "PATH" "/usr/local/bin:$PATH" t)
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super)
  (global-set-key (kbd "M-q") 'delete-frame))

(defun rm-guess-indent-tabs-mode ()
  "Attempts to set `indent-tabs-mode' by examining indentation at
the end of the file."
  (save-excursion
    (goto-char (point-max))
    (while (and (re-search-backward "^[ \t]" nil t)
                (null (nth 8 (syntax-ppss)))))
    (setq-local indent-tabs-mode (eql (char-after) ?\t))))
(add-hook 'prog-mode-hook 'rm-guess-indent-tabs-mode)

;; Temp files
(setq auto-save-list-file-prefix "~/.emacs.d/tmp/autosaves/"
      url-configuration-directory "~/.emacs.d/tmp/url/")

;; Enable disabled commands
(put 'narrow-to-region 'disabled nil)

;; Create my own keymap for bindings
(define-prefix-command 'rm-map)
(global-set-key (kbd "C-h") 'rm-map)

;; Preserve commonly-used help-map bindings
(define-key rm-map (kbd "k") 'describe-key)
(define-key rm-map (kbd "v") 'describe-variable)
(define-key rm-map (kbd "f") 'describe-function)
(define-key rm-map (kbd "w") 'where-is)

(defun rm-other-window-or-frame ()
  "Switch to another frame if only one window exists."
  (interactive)
  (let ((try-other-frames
         (and (> (length (frame-list)) 1)
              (eq (length (window-list)) 1))))
    (if try-other-frames            ; This should be unnecessary
        (other-frame 1)             ; but I'm too lazy to fix it
      (other-window 1))))

;; Let C-x o work across frames if there is only one window
(define-key global-map (kbd "C-x o") 'rm-other-window-or-frame)

(load custom-file t)
(load "~/.emacs.d/lisp/local-configuration.el" t)

;; Package configuration
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(require 'rm-packages)

(load "~/.emacs.d/lisp/local-packages.el" t)
