function ls --wraps='exa -l' --wraps='exa -la' --wraps=ll --wraps='l --sort_dir' --wraps='colorls -gpA' --wraps='colorls --gs' --wraps=lsd --description 'alias ls=lsd'
  lsd $argv; 
end
