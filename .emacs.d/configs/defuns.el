
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


(defun set-frame-alist()
  (interactive)
  (when (display-graphic-p)
    (progn
      (defconst perf-num 0.618 "The perfect number")
      (defconst rperf-num (- 1 perf-num) "1 - perfect number")
      (add-to-list 'default-frame-alist (cons 'height
					      (round (/ (* (display-pixel-height) perf-num)
							(frame-char-height)))))
      (add-to-list 'default-frame-alist (cons 'width (round (/ (* (display-pixel-width) perf-num)
							       (frame-char-width)))))

      (add-to-list 'default-frame-alist (cons 'left (round (* (/ rperf-num 2) (display-pixel-width)))))
      (add-to-list 'default-frame-alist (cons 'top (round (* (/ rperf-num 2) (display-pixel-height)))))
      (add-to-list 'default-frame-alist '(tool-bar-lines . 0)))))


;; toggle window split
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(defun set-frame-window-appearance (&optional frame)
  (when (display-graphic-p)
    (menu-bar-mode 1)
    (defconst perf-num 0.618 "The perfect number")
    (defconst rperf-num (- 1 perf-num) "1 - perfect number")
    (let ((f (if frame frame
	       (selected-frame))))
      (progn
;	(set-face-foreground 'linum "grey55" f)	
	(set-frame-position f
			    (round (* (/ rperf-num 2) (display-pixel-width)))
			    (round (* (/ rperf-num 2) (display-pixel-height))))
	(set-frame-size f
			(round (/ (* (display-pixel-width) perf-num)
				  (frame-char-width)))
			(round (/ (* (display-pixel-height) perf-num)
				  (frame-char-height))))))))

(defun set-frame-terminal-appearance (&optional frame)
  (unless (display-graphic-p)
    (menu-bar-mode 0)
    (let ((f (if frame frame
	       (selected-frame))))
      (progn
	(set-face-foreground 'linum "yellow" f)))))

(defun set-frame-terminal (&optional frame)
  (unless (display-graphic-p)
    (print "here2")        
    (message "here2")    
    (set-frame-terminal-appearance frame)))
    
(defun set-frame-window (&optional frame)
  (when (display-graphic-p)
    (print "here1")    
    (message "here1")
    (set-frame-window-appearance frame)
    (raise-frame)))    
			  
(defun packages-init()
  "All package initialization should go here"
  (require 'smart-tab)
  (global-smart-tab-mode))

