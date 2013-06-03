(load "configs/defuns")

;;(set-frame-position (selected-frame)
;;        (- (/ (display-pixel-width) 2) 320)
;;        (- (/ (display-pixel-height) 2) 320))

(setq inhibit-startup-message t)
(setq column-number-mode t)
(setq mouse-yank-at-point t)
(setq scroll-margin 3
      scroll-conservatively 10000)

(delete-selection-mode 1)
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

(fset 'yes-or-no-p 'y-or-n-p) ;change 'yes or no' to 'y or n'
(setq x-select-enable-clipboard t) ;copy to clipboard
(display-time)
   
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

(global-linum-mode 1)

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
 
(add-hook 'emacs-startup-hook 'toggle-window-split)

(require 'weechat)

(xterm-mouse-mode t)
(global-set-key [mouse-4] '(lambda ()
			     (interactive)
			     (scroll-down 1)))
(global-set-key [mouse-5] '(lambda ()
			     (interactive)
			     (scroll-up 1)))

(require 'wgrep-ack)
