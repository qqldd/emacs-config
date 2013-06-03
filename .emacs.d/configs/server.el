(load "configs/defuns")

;(add-hook 'server-visit-hook (lambda () (print (display-pixel-width))))


;;(defun end-server-edit ()
;;   (shell-command "osascript -e \"tell application \\\"System Events\\\" to keystroke tab using command down\""));;(add-hook 'server-done-hook 'end-server-edit)



;(load "server")
;(unless (server-running-p) (server-start))
;; (setq server-use-tcp t
;;       server-port    52699)
;; (defun server-start-and-copy ()
;;   (server-start)
;;   (copy-file "~/.emacs.d/server/server" "/arch:.emacs.d/server/server" t))
;; (add-hook 'emacs-startup-hook 'server-start-and-copy)
