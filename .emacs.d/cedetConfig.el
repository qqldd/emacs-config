(provide 'cedetConfig)

;;cedet configurations
;;CEDET
(add-to-list 'load-path "~/.emacs.d/plugins/cedet/common")
(require 'cedet)
(require 'semantic-ia)

;; Enable EDE (Project Management) features
(global-ede-mode 1)
  
;; Enable SRecode (Template management) minor-mode.
(global-srecode-minor-mode 1)

;;semantic
;; (semantic-load-enable-minimum-features)
(semantic-load-enable-code-helpers)
;; (semantic-load-enable-guady-code-helpers)
;; (semantic-load-enable-excessive-code-helpers)
(semantic-load-enable-semantic-debugging-helpers)


