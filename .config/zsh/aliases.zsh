# mac os shortcuts
alias code="open -a Visual\ Studio\ Code"

# Command line utilities
alias ls='exa --icons --group-directories-first'
alias ll='exa -l --icons --group-directories-first'
alias grep='rg'
alias du="dust"
alias df="duf"
alias less="bat -p"
alias cd='z'
alias ps='procs'


alias serve='yarn run start:dev'
alias nvc='cd ~/.config/nvim/lua/user'
alias yarn="newt exec yarn"
alias npm="newt exec npm"

export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH" 
alias reset_bluetooth="sudo pkill bluetoothd"
