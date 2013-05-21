;;c and cpp configurations
(require 'cc-mode)
(require 'highlight-symbol)
(require 'xcscope)

(defun my-c-mode-common-hook()
  (c-set-style "K&R")
  (setq tab-width 4 indent-tabs-mode nil)
  (setq c-basic-offset 4)
  (local-set-key (kbd "RET") 'newline-and-indent)
  (define-key c-mode-base-map (kbd "M-n") 'semantic-ia-complete-symbol)
  (define-key c-mode-base-map (kbd "C-c ?") 'semantic-ia-complete-symbol-menu)
  (define-key c-mode-base-map (kbd "C-c j") 'semantic-ia-fast-jump)
  (define-key c-mode-base-map (kbd "M-o") 'eassist-switch-h-cpp)
  (define-key c-mode-base-map (kbd "M-m") 'eassist-list-methods)

  ;;hightlight-symbol
  (highlight-symbol-mode t)
  (local-set-key (kbd "C-c h h") 'highlight-symbol-at-point)
  (local-set-key (kbd "C-C h n") 'highlight-symbol-next)
  (local-set-key (kbd "C-C h p") 'highlight-symbol-prev)
  (local-set-key (kbd "C-C h R") 'highlight-symbol-remove-all)
  (local-set-key (kbd "C-C h r") 'highlight-symbol-query-replace)
  
)

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook())
(add-hook 'c++-mode-hook 'my-c-mode-common-hook())
