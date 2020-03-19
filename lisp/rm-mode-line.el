(defvar rm-mode-line--active-window nil
  "The currently active window in the currently acitve frame.
Used to change the color of the buffer name in the active window
only.")

(defvar rm-mode-line--flycheck-previous-state ""
  "Saves the previous output of flycheck to prevent flashing
    the modeline while calculating errors")

(defvar rm-mode-line-show-shelf-right nil
  "When non-nil, show extra shelf on the right of the mode line.")

(defvar rm-mode-line-show-shelf-left nil
  "When non-nil, show extra shelf on the left of the mode line.")

(defvar rm-mode-line-indicator-shelf-right-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'rm-mode-line-toggle-shelf-right)
    map))

(defvar rm-mode-line-indicator-shelf-left-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'rm-mode-line-toggle-shelf-left)
    map))

(defun rm-record-selected-window (&optional frame)
  (with-selected-frame (or frame (selected-frame))
    (setq rm-mode-line--active-window (selected-window))))

(add-hook 'post-command-hook 'rm-record-selected-window)
(add-hook 'after-make-frame-functions 'rm-record-selected-window)
(add-hook 'buffer-list-update-hook #'(lambda () (force-mode-line-update t)))

(defun rm-mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT
 aligned respectively."
  (let ((available-width (- (window-width) (length left))))
    (format (format "%%s %%%ds" available-width) left right)))

(defun rm-mode-line-vc-mode ()
  "Customized version of vc-mode.
Uses a shorter display for Git and SVN repos.  More importantly
this displays the vc info even when not visiting a file under
version control by using the default-directory instead."
  (let ((result nil)
        (display-map '((git . "⌥ ")
                       (svn . "𝑟"))))
    ;; Not sure how to handle remote files yet
    (unless (tramp-handle-file-remote-p default-directory)
      (if vc-mode
          (progn
            (setq result (string-trim (substring-no-properties vc-mode)))
            (setq result (pcase (substring result 0 3)
                           ("Git" (replace-regexp-in-string "Git." (alist-get 'git display-map) result))
                           ("SVN" (replace-regexp-in-string "SVN." (alist-get 'svn display-map) result))
                           (_ result))))
        (let ((commands '("git rev-parse --abbrev-ref HEAD"
                          "svn info --show-item revision")))
          (while commands
            (let* ((command (pop commands))
                   (command-name (intern (car (split-string command)))))
              (setq result (string-trim (shell-command-to-string (format "%s 2>/dev/null" command))))
              (when (not (string-equal result ""))
                (setq result (concat (alist-get command-name display-map) result))
                (setq commands nil))))))
      (if (string-equal result "")
          result
        (concat "   " result)))))

(defun rm-mode-line-flycheck ()
  (when (bound-and-true-p flycheck-mode)
    (let ((counts (string-trim (flycheck-mode-line-status-text))))
      (if (string-equal counts (format "%s*" flycheck-mode-line-prefix))
          rm-mode-line--flycheck-previous-state
        (unless (string-equal counts flycheck-mode-line-prefix)
          (let* ((counts (split-string (string-trim counts "FlyC:") "/"))
                 (errors (car counts))
                 (warnings (cadr counts))
                 (issues (apply '+ (mapcar 'string-to-number counts)))
                 (results ""))
            (unless (eq issues 0)
              (setq results (concat results (propertize (format "    ☓ %s %s" issues (rm-mode-line--pluralize issues "issue"))
                                                        'font-lock-face 'font-lock-warning-face))))
            (setq rm-mode-line--flycheck-previous-state results)))))))

(defun rm-mode-line-toggle-shelf-right ()
  (interactive)
  (setq rm-mode-line-show-shelf-right (not rm-mode-line-show-shelf-right))
  (force-mode-line-update))

(defun rm-mode-line-toggle-shelf-left ()
  (interactive)
  (setq rm-mode-line-show-shelf-left (not rm-mode-line-show-shelf-left))
  (force-mode-line-update))

(defun rm-mode-line-indicator-right ()
  (let ((indicator (if rm-mode-line-show-shelf-right "  ❯❯" "  ❮❮")))
    (propertize indicator 'local-map rm-mode-line-indicator-shelf-right-map 'mouse-face 'mode-line-highlight)))

(defun rm-mode-line-indicator-left ()
  (let ((indicator (if rm-mode-line-show-shelf-left "❮❮  " "❯❯  ")))
    (propertize indicator 'local-map rm-mode-line-indicator-shelf-left-map 'mouse-face 'mode-line-highlight)))

(defun rm-mode-line-mode-list ()
  (let ((modes mode-name))
    (when rm-mode-line-show-shelf-right
      (setq modes (string-trim (substring-no-properties
                                (format-mode-line mode-line-modes))
                               "(" ") ")))
    modes))

(defun rm-mode-line-read-only ()
  (cond ((derived-mode-p 'term-mode) "")
        ((and buffer-read-only
              (eq rm-mode-line--active-window (selected-window)))
         (propertize "⌀ " 'font-lock-face 'font-lock-warning-face))
        (t "")))

(defun rm-mode-line-modified ()
  (let ((modified-flag ""))
    (when (and (buffer-modified-p) (buffer-file-name))
      (setq modified-flag "☀"))
    (propertize (format "%1s" modified-flag) 'font-lock-face 'font-lock-keyword-face)))


(defun rm-mode-line-buffer-id ()
  (let ((buffer-name-face 'mode-line-buffer-id))
    (when (not (eq rm-mode-line--active-window (selected-window)))
      (setq buffer-name-face 'mode-line))
    (concat
       (when buffer-file-name
         (let ((fname (string-trim (file-name-directory buffer-file-name) " " "/")))
           (when (not rm-mode-line-show-shelf-left)
             (setq fname (file-name-nondirectory fname)))
           (propertize (format "%s/" fname)
                       'font-lock-face '(:weight extra-light))))
       (propertize "%b" 'font-lock-face buffer-name-face))))


(defun rm-mode-line--pluralize (num str)
  (if (eq num 1)
      str
    (concat str "s")))

(defun rm-mode-line-shelf-left ()
  (when rm-mode-line-show-shelf-left))

(defun rm-mode-line-shelf-right ()
  (when rm-mode-line-show-shelf-right
    "%lL:%cC    "))


(setq rm-original-mode-line mode-line-format)

(setq-default
 mode-line-format
 '((:eval
    (rm-mode-line-render
     ;; left
     (format-mode-line
      '(" "
        (:eval (rm-mode-line-indicator-left))
        (:eval (rm-mode-line-shelf-left))
        (:eval (rm-mode-line-read-only))
        (:eval (rm-mode-line-buffer-id))
        (:eval (rm-mode-line-modified))
        (:eval (rm-mode-line-vc-mode))
        (:eval (rm-mode-line-flycheck))))

     ;; right
     (format-mode-line
      '((:eval (rm-mode-line-shelf-right))
        (:eval (rm-mode-line-mode-list))
        (:eval (rm-mode-line-indicator-right))
        " "))))))

(provide 'rm-mode-line)
