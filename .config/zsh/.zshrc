
[[ -f ~/.config/zsh/appsetup.zsh ]] && source ~/.config/zsh/appsetup.zsh
[[ -f ~/.config/zsh/aliases.zsh ]] && source ~/.config/zsh/aliases.zsh
[[ -f ~/.config/zsh/functions.zsh ]] && source ~/.config/zsh/functions.zsh
[[ -f ~/.config/zsh/starship.zsh ]] && source ~/.config/zsh/starship.zsh
[[ -f ~/.config/zsh/nvm.zsh ]] && source ~/.config/zsh/nvm.zsh

source ~/.zprofile
eval "$(starship init zsh)"

