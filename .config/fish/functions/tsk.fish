function tsk --wraps='tmux kill-server' --description 'alias tsk tmux kill-server'
  tmux kill-server $argv; 
end
