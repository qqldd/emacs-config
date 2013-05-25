
(defvar dark-background nil)

(defun toggle-dark-background ()
  (interactive)
  (let ((difficult-colors
         '("red" "blue" "medium blue")))
    (mapc
     (lambda (face)
       (and (member (face-attribute face :foreground)  difficult-colors)
            (set-face-bold-p face (not dark-background))))
     (face-list)))
  (setq dark-background (not dark-background)))
