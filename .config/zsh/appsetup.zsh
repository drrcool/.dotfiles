

source '/Users/rcool2/.cargo/env'

#ColorLS
eval "$(/opt/homebrew/bin/brew shellenv)"
eval $(thefuck --alias)
eval "$(zoxide init zsh)"
ssh-add -q --apple-use-keychain $HOME/.ssh/id_ed25519
ssh-add -q --apple-use-keychain $HOME/.ssh/id_github
ssh-add -q --apple-use-keychain $HOME/.ssh/id_rsa
