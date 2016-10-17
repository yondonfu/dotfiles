;; Turn off delete trailing whitespace for Markdown mode
(defun turn-off-delete-trailing-whitespace ()
  (remove-hook 'before-save-hook 'delete-trailing-whitespace))

(turn-off-delete-trailing-whitespace)
