if status is-interactive
     bass /Library/Ruby/Gems/2.6.0/gems/colorls-1.4.6/lib/tab_complete.sh     
     zoxide init fish | source
     thefuck --alias | source
     bass $(dirname $(gem which colorls))/tab_complete.sh
     eval $(/opt/homebrew/bin/brew shellenv)
end


ssh-add -q --apple-use-keychain /Users/rcool/.ssh/rcool@netflix.com-stash__corp__netflix__com
ssh-add -q --apple-use-keychain /Users/rcool/.ssh/id_ed25519
ssh-add -q --apple-use-keychain /Users/rcool/.ssh/drrcool-GitHub


set -U FZF_CTRL_R_OPTS "--reverse --preview 'echo {}' --preview-window down:3:hidden:wrap --bind '?:toggle-preview'"
set -U FZF_DEFAULT_COMMAND "fd -H -E '.git'"
set -U FZF_DEFAULT_OPTS "--color=spinner:#F8BD96,hl:#F28FAD --color=fg:#D9E0EE,header:#F28FAD,info:#DDB6F2,pointer:#F8BD96 --color=marker:#F8BD96,fg+:#F2CDCD,prompt:#DDB6F2,hl+:#F28FAD"
set -U FZF_TMUX_OPTS "-p"

fish_add_path $HOME/rcool/.cargo/bin
fish_add_path /opt/homebrew/bin
fish_add_path /opt/homebrew/sbin
fish_add_path $HOME/.config/bin # custom scripts
fish_add_path ./node_modules/.bin
fish_add_path $HOME/Documents/mydotfiles/bin/
fish_add_path /opt/homebrew/opt/nvm/versions/node/v17.9.1
fish_add_path $HOME/.yarn/bin/
fish_add_path $HOME/.config/yarn/global/node_modules/.bin


# fish colors
set -U fish_color_autosuggestion black
set -U fish_color_command normal
set -U fish_color_error red
set -U fish_color_param cyan
set -U fish_color_redirections yellow
set -U fish_color_terminators white
set -U fish_color_valid_path green

    # Commands to run in interactive sessions can go here

