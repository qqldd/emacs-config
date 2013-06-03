;; Add command line support
;; Usage: emacs -diff file1 file2
(defun command-line-diff (switch)
  (let ((file1 (pop command-line-args-left))
	(file2 (pop command-line-args-left)))
    (ediff file1 file2)))

(add-to-list 'command-switch-alist '("-diff" . command-line-diff))


;; (setq ediff-split-window-function (if (> (frame-width) 100)
;; 				      'split-window-horizontally
;; 				    'split-window-vertically))

(setq ediff-split-window-function 'split-window-horizontally)

