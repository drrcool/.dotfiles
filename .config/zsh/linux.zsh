
source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
export GCM_CREDENTIAL_STORE=gpg
alias volUp='pactl set-sink-volume @DEFAULT_SINK@ +5000'
alias volDown='pactl set-sink-volume @DEAULT_SINK@ -5000'
alias volMute='pactl set-sink-mute @DEFAULT_SINK@ toggle'
alias qtileLog='tail -n 100 ~/.local/share/qtile/qtile.log'


alias mirrordisplays='xrandr --output DP-2 --same-as HDMI-0'
alias mountpylon='sshfs -o reconnect pylon:/root ~/pylon'
alias unmountpylon='umount ~/pylon'
