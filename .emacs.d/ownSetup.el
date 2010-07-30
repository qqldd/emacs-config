(provide 'ownSetup)

;;some convenience setup
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

(setq dired-recursive-copies 'top)
(setq dired-recursive-deletes 'top)

(setq default-frame-alist 
'((height . 30) (width . 80) (menu-bar-lines . 20) (tool-bar-lines . 0))) 

;;backup settings
(setq
    backup-by-copying t ; 自动备份
    backup-directory-alist
    '(("." . "~/.emacs.d/backups")) ; 自动备份在目录"~/.emacs.d/backups"下
    delete-old-versions t ; 自动删除旧的备份文件
    kept-new-versions 6 ; 保留最近的6个备份文件
    kept-old-versions 2 ; 保留最早的2个备份文件
    version-control t) ; 多次备份