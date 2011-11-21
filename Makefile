install: $(HOME)/.emacs $(HOME)/.emacs.d

$(HOME)/.emacs: .emacs
	ln -s $(shell pwd)/$< $@

$(HOME)/.emacs.d: .emacs.d
	ln -s $(shell pwd)/$< $@
