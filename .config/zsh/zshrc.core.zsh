source $HOME/.config/zsh/aliases.zsh
if [[ `uname` == "Darwin" ]]; then
  source $HOME/.config/zsh/mac.zsh
else
  source $HOME/.config/zsh/linux.zsh
fi
source $HOME/.config/zsh/appsetup.zsh
source $HOME/.config/zsh/nvm.zsh
source $HOME/.config/zsh/starship.zsh
source $HOME/.config/zsh/zsh_completion_options.zsh
