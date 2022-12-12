if status is-interactive
    # Commands to run in interactive sessions can go here
end

# ssh-add -q --apple-use-keychain /Users/rcool/.ssh/rcool@netflix.com-stash__corp__netflix__com
# ssh-add -q --apple-use-keychain /Users/rcool/.ssh/drrcool-GitHub
# ssh-add -q --apple-use-keychain /Users/rcool/.ssh/id_ed25519
ssh-add -q --apple-use-keychain $HOME/.ssh/id_ed25519
ssh-add -q --apple-use-keychain $HOME/.ssh/id_github
ssh-add -q --apple-use-keychain $HOME/.ssh/id_rsa

export NVIMLOADED=$HOME/.dotfiles/.config/nvimloaded

starship init fish | source
nvm use v16
