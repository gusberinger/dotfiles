# You can put files here to add functionality separated per file, which
# will be ignored by git.
# Files on the custom/ directory will be automatically loaded by the init
# script, in alphabetical order.

# For example: add yourself some shortcuts to projects you often work on.
#
# brainstormr=~/Projects/development/planetargon/brainstormr
# cd $brainstormr
#
clear;archey
DISABLE_UPDATE_PROMPT=true
alias vim='mvim -v'
alias q='exit'
alias show_files='defaults write com.apple.finder AppleShowAllFiles YES'
alias hide_files='defaults write com.apple.finder AppleShowAllFiles NO'

# setup thefuck
eval $(thefuck --alias)

# virtualenv
export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/Desktop
source /usr/local/bin/virtualenvwrapper.sh
