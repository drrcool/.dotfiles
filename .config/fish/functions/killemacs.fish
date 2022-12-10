function killemacs --wraps='emacsclient -e "(kill-emacs)"' --description 'alias killemacs=emacsclient -e "(kill-emacs)"'
  emacsclient -e "(kill-emacs)" $argv; 
end
