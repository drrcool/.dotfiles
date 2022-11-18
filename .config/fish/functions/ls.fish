function ls --wraps='exa -l' --wraps='exa -la' --wraps=ll --wraps='l --sort_dir' --wraps='colorls -gpA' --wraps='colorls --gs' --wraps=lsd --wraps='exa --icons -a --group-directories-first --color always' --wraps='colorls --gs spec -t' --wraps='colorls --gs -t' --wraps='colorls --gs --sd -a' --description 'alias ls=colorls --gs --sd -a'
  colorls --gs --sd -a $argv; 
end
