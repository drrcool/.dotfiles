
export ZSH=$HOME/.oh-my-zsh
ZSH_THEME="agnoster"
source $ZSH/oh-my-zsh.sh

#ColorLS
source $(dirname $(gem which colorls))/tab_complete.sh

eval "$(/opt/homebrew/bin/brew shellenv)"
eval $(thefuck --alias)
eval "$(zoxide init zsh)"
source $HOME/.config/broot/launcher/bash/br
