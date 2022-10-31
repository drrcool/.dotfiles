function jest --wraps='newt exec npx jest --color --watch' --description 'alias jest=newt exec npx jest --color --watch'
  newt exec npx jest --color --watch $argv; 
end
