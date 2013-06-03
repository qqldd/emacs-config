(setq debug-on-error t)

(add-to-list 'load-path "~/.emacs.d/plugins")
(add-to-list 'load-path "~/.emacs.d")

(setq custom-file "~/.emacs.d/configs/custom.el")
(load custom-file)

(load "load-configs")

