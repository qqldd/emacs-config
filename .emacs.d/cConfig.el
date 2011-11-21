(provide 'cConfig)

;;c and cpp configurations
(require 'cc-mode)
(defun my-c-mode-common-hook()
  (setq tab-width 4 indent-tabs-mode nil)
  (c-set-style "K&R")
  (setq c-basic-offset 4)
  (require 'xcscope)
  (local-set-key (kbd "RET") 'newline-and-indent)
  (define-key c-mode-base-map (kbd "M-n") 'semantic-ia-complete-symbol-menu)
  (define-key c-mode-base-map (kbd "<f12>") 'semantic-ia-fast-jump)
  (define-key c-mode-base-map (kbd "M-o") 'eassist-switch-h-cpp)
  (define-key c-mode-base-map (kbd "M-m") 'eassist-list-methods)
)

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook())
(add-hook 'c++-mode-hook 'my-c-mode-common-hook())