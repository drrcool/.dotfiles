
source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
export GCM_CREDENTIAL_STORE=gpg
alias volUp='pactl set-sink-volume @DEFAULT_SINK@ +2000'
alias volDown='pactl set-sink-volume @DEAULT_SINK@ -2000'
alias volMute='pactl set-sink-mute @DEFAULT_SINK@ toggle'
export LD_LIBRARY_PATH=~/repos/tree-sitter/
alias doomemacs='LD_LIBRARY_PATH=~/repos/tree-sitter /usr/local/bin/emacs --init-dir=~/.doomemacs.d'
