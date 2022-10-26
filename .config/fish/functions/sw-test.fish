function sw-test --wraps='cd ~/sessionWiz; nvm use; npm test:watch' --wraps='cd ~/sessionWiz; nvm use; npm run test:watch' --description 'alias sw-test cd ~/sessionWiz; nvm use; npm run test:watch'
  cd ~/sessionWiz; nvm use; npm run test:watch $argv; 
end
