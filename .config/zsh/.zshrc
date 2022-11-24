# Fig pre block. Keep at the top of this file.
[[ -f "$HOME/.fig/shell/zshrc.pre.zsh" ]] && builtin source "$HOME/.fig/shell/zshrc.pre.zsh"
export PATH=$HOME/.fig/bin:$HOME/.local/bin:$PATH
source $HOME/.config/zsh/zshrc.core.zsh
source /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zs
ZSH_THEME='code-smell'
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=7'
export BAT_THEME="1337"
"
# Fig post block. Keep at the bottom of this file.
[[ -f "$HOME/.fig/shell/zshrc.post.zsh" ]] && builtin source "$HOME/.fig/shell/zshrc.post.zsh"
