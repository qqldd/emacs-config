
;;some convenience setup
;;(setq default-frame-alist 
;;'((height . 30) (width . 80) (menu-bar-lines . 20) (tool-bar-lines . 0)))

(set-frame-position (selected-frame)
		    (- (/ (display-pixel-width) 2) 320)
		    (- (/ (display-pixel-height) 2) 320))

(setq inhibit-startup-message t)
(setq column-number-mode t)
(setq mouse-yank-at-point t)
(setq scroll-margin 3
      scroll-conservatively 10000)
(show-paren-mode t)
(setq show-paren-style 'parentheses)
(mouse-avoidance-mode 'animate)
(setq frame-title-format "emacs@%b")
(auto-image-file-mode)
(global-font-lock-mode t)
(ido-mode t)
;;switch C-j and RET
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-j") 'newline)
;; For mac
(setq ns-right-command-modifier (quote meta))

(ansi-color-for-comint-mode-on) ;set shell colorful
(customize-set-variable 'scroll-bar-mode 'right)
(fset 'yes-or-no-p 'y-or-n-p) ;change 'yes or no' to 'y or n'
(setq x-select-enable-clipboard t) ;copy to clipboard
(display-time)
;;(tool-bar-mode -1) ;do not display tool bar
   
(setq dired-recursive-copies 'top)
(setq dired-recursive-deletes 'top)


;;backup settings
(setq
    backup-by-copying t ; 自动备份
    backup-directory-alist
    '(("." . "~/.emacs.d/backups")) ; 自动备份在目录"~/.emacs.d/backups"下
    delete-old-versions t ; 自动删除旧的备份文件
    kept-new-versions 6 ; 保留最近的6个备份文件
    kept-old-versions 2 ; 保留最早的2个备份文件
    version-control t) ; 多次备份

;;org-mode ditaa path
(setq org-ditaa-jar-path "~/.emacs.d/plugins/ditaa.jar")

;;For global varible I do not know how to config, add to hook
(add-hook 'find-file-hook (lambda () (linum-mode 1)))

 (mapcar
  (function (lambda (setting)
 	     (setq auto-mode-alist
 		   (cons setting auto-mode-alist))))
  '(("\\.xml$".  sgml-mode)
    ("\\\.bash" . sh-mode)
    ("\\.rdf$".  sgml-mode)
    ("\\.session" . emacs-lisp-mode)
    ("\\.l$" . c-mode)
    ("\\.cu$" . c++-mode)
    ("\\.cl$" . c++-mode)    
    ("\\.css$" . css-mode)
    ("\\.cfm$" . html-mode)
    ("gnus" . emacs-lisp-mode)
    ("\\.idl$" . idl-mode)))
 
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

(add-hook 'emacs-startup-hook 'toggle-window-split)
