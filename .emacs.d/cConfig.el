(provide 'cConfig)

;;c and cpp configurations
;;(require 'cc-mode)
(defun my-c-mode-common-hook()
  (setq tab-width 4 indent-tabs-mode nil)
  (c-set-style "K&R")
  (require 'xcscope)
  (local-set-key (kbd "RET") 'newline-and-indent)
  (define-key c-mode-base-map (kbd "M-n") 'semantic-ia-complete-symbol-menu)

)

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook())
