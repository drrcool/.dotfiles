# mac os shortcuts
alias code="open -a Visual\ Studio\ Code"

# Command line utilities
alias ll='exa -lbhH --color-scale --git -icons --group-directories-first'
alias grep='rg'
alias du="dust"
alias df="duf"
alias less="bat -p"
alias ps='procs'

#set up tmux integration with iterm2
export ITERM_TMUX_INTEGRATION=true

alias goqoe='exec $(echo "cd ~/qoedash; newt exec yarn run start:dev")'
alias gosw='exec $(echo "cd ~/sessionwiz; newt exec npm run start:dev")'
alias nvc="cd ~/.config/nvim/lua/user"
alias top="htop"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH" 
export PATH="$HOME/.local/bin:$PATH"
export PATN="$HOME/.fig/bin:$PATH"
alias reset_bluetooth="sudo pkill bluetoothd"

alias pylon='ssh root@rcool.pylon.prod.container.dataeng.netflix.net'
alias ls='exa  --group --icons --group-directories-first --binary'
alias ec='emacsclient -c'
alias killemacs='emacsclient -e "(kill-emacs)"'

alias vpn='/opt/pulsesecure/bin/pulseUI'

