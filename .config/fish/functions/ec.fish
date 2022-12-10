function ec --wraps='emacsclient -c &' --wraps='emacsclient -c ' --description 'alias ec=emacsclient -c '
  emacsclient -c  $argv; 
end
