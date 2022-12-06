if status is-interactive
    # Commands to run in interactive sessions can go here
end

ssh-add -q --apple-use-keychain /Users/rcool/.ssh/rcool@netflix.com-stash__corp__netflix__com
ssh-add -q --apple-use-keychain /Users/rcool/.ssh/drrcool-GitHub
ssh-add -q --apple-use-keychain /Users/rcool/.ssh/id_ed25519

export NVIMLOADED=$HOME/.dotfiles/.config/nvimloaded
  alias nvl='XDG_DATA_HOME=$NVIMLOADED/share XDG_CACHE_HOME=$NVIMLOADED XDG_CONFIG_HOME=$NVIMLOADED nvim'

starship init fish | source
