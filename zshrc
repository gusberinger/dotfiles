source ~/.zplug/init.zsh
export ZPLUG_HOME=/usr/local/opt/zplug
source $ZPLUG_HOME/init.zsh
zplug "zsh-users/zsh-syntax-highlighting"
zplug "zsh-users/zsh-history-substring-search"
zplug "bhilburn/powerlevel9k", use:powerlevel9k.zsh-theme
zplug "mafredri/zsh-async", from:"github", use:"async.zsh"
zplug load

# Syntax
if zplug check "zsh-users/zsh-syntax-highlighting"; then
	#ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=10'
	ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern cursor line)
	ZSH_HIGHLIGHT_PATTERNS=('rm -rf *' 'fg=white,bold,bg=red')

	typeset -A ZSH_HIGHLIGHT_STYLES
	ZSH_HIGHLIGHT_STYLES[cursor]='bg=yellow'
	ZSH_HIGHLIGHT_STYLES[globbing]='none'
	ZSH_HIGHLIGHT_STYLES[path]='fg=white'
	ZSH_HIGHLIGHT_STYLES[path_pathseparator]='fg=grey'
	ZSH_HIGHLIGHT_STYLES[alias]='fg=cyan'
	ZSH_HIGHLIGHT_STYLES[builtin]='fg=cyan'
	ZSH_HIGHLIGHT_STYLES[function]='fg=cyan'
	ZSH_HIGHLIGHT_STYLES[command]='fg=green'
	ZSH_HIGHLIGHT_STYLES[precommand]='fg=green'
	ZSH_HIGHLIGHT_STYLES[hashed-command]='fg=green'
	ZSH_HIGHLIGHT_STYLES[commandseparator]='fg=yellow'
	ZSH_HIGHLIGHT_STYLES[redirection]='fg=magenta'
	ZSH_HIGHLIGHT_STYLES[bracket-level-1]='fg=cyan,bold'
	ZSH_HIGHLIGHT_STYLES[bracket-level-2]='fg=green,bold'
	ZSH_HIGHLIGHT_STYLES[bracket-level-3]='fg=magenta,bold'
	ZSH_HIGHLIGHT_STYLES[bracket-level-4]='fg=yellow,bold'
fi


# show and hide desktop icons
alias hd="defaults write com.apple.finder CreateDesktop -bool FALSE && killall Finder"
alias sd="defaults write com.apple.finder CreateDesktop -bool TRUE && killall Finder"

# chunkwm
alias swap="chunkc tiling::window --swap"

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

# ssh works different in Kitty
alias ssh="kitty +kitten ssh"

# Kitty Config
autoload -Uz compinit
compinit
kitty + complete setup zsh | source /dev/stdin



