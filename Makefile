install: $(HOME)/.emacs.d

$(HOME)/.emacs.d: .emacs.d
	ln -s $(shell pwd)/$< $@
