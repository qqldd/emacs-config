(load "configs/defuns")

(add-hook 'after-make-frame-functions 'set-frame-terminal t)
(add-hook 'after-make-frame-functions 'set-frame-window t)

(if (display-graphic-p)
    (set-frame-window)
  (set-frame-terminal))

;(add-hook 'server-visit-hook 'set-frame-window)
;(add-hook 'server-visit-hook 'set-frame-terminal)

(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
;(setq 'scroll-bar-mode 'right)
;;(tool-bar-mode -1) ;do not display tool bar
