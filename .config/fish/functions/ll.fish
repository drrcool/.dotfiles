function ll --wraps=ls --wraps=colorls\ --\ -lA\n --wraps='colorls --sd -lA' --wraps='exa -lbhHa --color-scale --git --icons --group-directories-first' --wraps='colorls --sd -lAa' --description 'alias ll colorls --sd -lAa'
  colorls --sd -lAa $argv; 
end
