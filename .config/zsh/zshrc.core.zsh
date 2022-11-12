
 source $HOME/.config/zsh/appsetup.zsh
 source $HOME/.config/zsh/functions.zsh
 source $HOME/.config/zsh/nvm.zsh
 source $HOME/.config/zsh/starship.zsh
 source $HOME/.config/zsh/aliases.zsh

 export ZSH=$HOME/.oh-my-zsh/
 plugins=(git zsh-autosuggestions sudo zsh-256color  fast-syntax-highlighting  zsh-vi-mode) 
 source $ZSH/oh-my-zsh.sh 

 eval "$(starship init zsh)"
 eval $(thefuck --alias)

 # Fig pre block. Keep at the top of this file.d

 export NVM_DIR="$HOME/.nvm"
 [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
 [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

