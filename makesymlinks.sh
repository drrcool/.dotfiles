 #!/bin/bash
 CONFIG=$HOME/.config
 olddir=".old.dotfiles"
 mkdir -p $HOME/$olddir
 mkdir -p $CONFIG/$olddir

 home_files=".zshrc"
 config_dirs="alacritty bat fish iterm2 kitty lazygit lvim nvim tabby tmux"
 for file in $home_files; do
	echo "Moving any existing $file to $HOME/$olddir"
	mv $HOME/$file $HOME/$olddir/
	ln -s $PWD/$file $HOME/$file
done

for dir in $config_dirs; do
	mv $CONFIG/$dir $CONFIG/$olddir
	ln -s $PWD/.config/$dir $HOME/.config/$dir
done

