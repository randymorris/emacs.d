;;; Custom function/macro/advice/keymap definitions


;;; advices

(defadvice list-buffers (around minimize-list-buffers)
  "Show buffer list in a small vertically split window and make
the buffer list the active window."
  (let ((split-window-preferred-function 'split-window-vertically))
    ad-do-it)
  (pop-to-buffer "*Buffer List*")
  (shrink-window-if-larger-than-buffer))
(ad-activate 'list-buffers)

(defadvice switch-to-buffer (after after-switch-to-buffer)
  (when (string-equal major-mode "rcirc-mode")
    (rm-force-point-to-bottom)))
(ad-activate 'switch-to-buffer)

;;; functions

(defun rm-clean-scratch-buffer ()
  "Empty the scratch buffer and re-insert `initial-scratch-message'."
  (interactive)
  (when (string-equal (buffer-name) "*scratch*")
    (delete-region (point-min) (point-max))
    (insert initial-scratch-message)))
(define-key lisp-interaction-mode-map (kbd "C-c C-l") 'rm-clean-scratch-buffer)

(defun rm-force-point-to-bottom (&optional &rest args)
  (let ((recenter-positions '(bottom)))
    (recenter-top-bottom)))

(defun raise-ansi-term (buffer-name)
  (interactive)
  (let ((buffer (get-buffer (format "*%s*" buffer-name))))
    (if buffer
        (switch-to-buffer buffer)
      (ansi-term shell-file-name buffer-name))))

(provide 'rm-defs)
