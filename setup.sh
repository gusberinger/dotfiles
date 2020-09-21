# git
ln -sv ~/dotfiles/gitconfig ~/.gitconfig

# emacs
ln -sv ~/dotfiles/emacs/init.el ~/.emacs.d/

# zsh
git clone https://github.com/tarjoilija/zgen.git "${HOME}/.zgen"
ln -sv ~/dotfiles/zshrc ~/.zshrc
if [[ "$OSTYPE" == "linux-gnu" ]]; then
    ln -sv ~/dotfiles/zprofile ~/.zprofile

# vimrc
curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
ln -sv ~/dotfiles/vimrc ~/.vimrc

# latex
ln -sv ~/dotfiles/latexmkrc ~/.latexmkrc
