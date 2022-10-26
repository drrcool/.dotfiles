function serve --wraps='newt exec npm run start:dev' --description 'alias serve newt exec npm run start:dev'
  newt exec npm run start:dev $argv; 
end
