function ll --wraps=ls --wraps=colorls\ --\ -lA\n --wraps='colorls --sd -lA' --description 'alias ll colorls --sd -lA'
  colorls --sd -lA $argv; 
end
