SHELL = /bin/bash
DOTFILES_DIR := $(shell echo $(HOME)/dotfiles)

link:
	ln -sf $(DOTFILES_DIR)/zshrc $(HOME)/.zshrc
	ln -sf $(DOTFILES_DIR)/latexmkrc $(HOME)/.latexmkrc
	ln -sf $(DOTFILES_DIR)/vimrc $(HOME)/.vimrc
	ln -sf $(DOTFILES_DIR)/gitconfig $(HOME)/.gitconfig
	ln -sf $(DOTFILES_DIR)/emacs/init.el $(HOME)/.emacs.d/init.el

unlink:
	unlink $(HOME)/.zshrc
	unlink $(HOME)/.latexmkrc
	unlink $(HOME)/.vimrc
	unlink $(HOME)/.gitconfig
	unlink $(HOME)/.emacs.d/init.el
