# load zgen
source "${HOME}/.zgen/zgen.zsh"

# if the init scipt doesn't exist
if ! zgen saved; then
    echo "Creating a zgen save"

    zgen oh-my-zsh

    # plugins
    zgen oh-my-zsh plugins/git
    zgen oh-my-zsh plugins/sudo
    zgen oh-my-zsh plugins/command-not-found
    zgen load zsh-users/zsh-syntax-highlighting
    zgen load /path/to/super-secret-private-plugin

    # bulk load
    zgen loadall <<EOPLUGINS
        zsh-users/zsh-history-substring-search
        /path/to/local/plugin
EOPLUGINS
    # ^ can't indent this EOPLUGINS

    # completions
    zgen load zsh-users/zsh-completions src

    # save all to init script
    zgen save
fi

PROMPT=' %3~ %F{red}λ '

## libffi
export LDFLAGS="-L/usr/local/opt/libffi/lib"
export CPPFLAGS="-I/usr/local/opt/libffi/include"

## R compiler flags
export LDFLAGS="-L/usr/local/opt/openblas/lib"
export CPPFLAGS="-I/usr/local/opt/openblas/include"

# case insensitive tab completion
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

# show and hide desktop icons
alias hd="defaults write com.apple.finder CreateDesktop -bool FALSE && killall Finder"
alias sd="defaults write com.apple.finder CreateDesktop -bool TRUE && killall Finder"

# fix path errors for Emacs
export PATH=/Applications/Emacs.app/Contents/MacOS/bin:$PATH
export PATH=/Applications/Emacs.app/Contents/MacOS:$PATH
alias ec="emacsclient --c"


# chunkwm
alias swap="chunkc tiling::window --swap"

# git
alias gittrack="while true; do clear; git lg; sleep 10; done"

# navigate up in nested directory
function up {
    if [[ "$#" < 1 ]] ; then
	cd ..
    else
	CDSTR=""
	for i in {1..$1} ; do
	    CDSTR="../$CDSTR"
	done
	cd $CDSTR
    fi
}
