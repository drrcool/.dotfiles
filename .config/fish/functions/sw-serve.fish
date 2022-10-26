function sw-serve --wraps='cd sessionWiz; nvm use; npm run start:dev' --description 'alias sw-serve cd sessionWiz; nvm use; npm run start:dev'
  cd sessionWiz; nvm use; npm run start:dev $argv; 
end
