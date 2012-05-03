
(add-to-list 'load-path "~/.emacs.d/plugins")
(add-to-list 'load-path "~/.emacs.d")

;;some convenience setup
(require 'ownSetup)


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
    ("\\.css$" . css-mode)
    ("\\.cfm$" . html-mode)
    ("gnus" . emacs-lisp-mode)
    ("\\.idl$" . idl-mode)))

(require 'cConfig)
(require 'cedetConfig)
(require 'yasnippetConfig)
(require 'goConfig)
(require 'auctexConfig)
(require 'highlightSymbolConfig)
