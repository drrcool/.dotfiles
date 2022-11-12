# mac os shortcuts
alias code="open -a Visual\ Studio\ Code"

# Command line utilities
alias ls="exa --icons -a --group-directories-first"
alias ll='exa -lbhH --color-scale --git -icons --group-directories-first'
alias grep='rg'
alias du="dust"
alias df="duf"
alias less="bat -p"
alias cd='z'
alias ps='procs'


alias goqoe='exec $(echo "cd ~/qoedash; newt exec yarn run start:dev")'
alias gosw='exec $(echo "cd ~/sessionwiz; newt exec npm run start:dev")'
alias nvc="cd ~/.config/nvim/lua/user"
alias top="htop"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH" 
export PATH="$HOME/.local/bin:$PATH"
export PATN="$HOME/.fig/bin:$PATH"
alias reset_bluetooth="sudo pkill bluetoothd"
