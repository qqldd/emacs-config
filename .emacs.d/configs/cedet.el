;;cedet configurations
;;CEDET
(load-file "~/.emacs.d/plugins/cedet-1.1/common/cedet.el")
(require 'cedet)
(require 'semantic-ia)

;; Enable EDE (Project Management) features
(global-ede-mode t)
  
;; Enable SRecode (Template management) minor-mode.
(global-srecode-minor-mode 1)

;;semantic
;; (semantic-load-enable-minimum-features)
(semantic-load-enable-code-helpers)
;; (semantic-load-enable-guady-code-helpers)
;; (semantic-load-enable-excessive-code-helpers)
;; (semantic-load-enable-semantic-debugging-helpers)


