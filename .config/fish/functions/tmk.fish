function tmk --wraps='tmux kill-server' --description 'alias tmk tmux kill-server'
  tmux kill-server $argv; 
end
