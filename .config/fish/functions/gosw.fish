function gosw --wraps=cd\ \~/sessionwiz\ \&\&\ newt\ exec\ npm\ run\ start:dev\n\n --wraps='cd ~/sessionwiz && newt exec npm run start:dev' --description 'alias gosw cd ~/sessionwiz && newt exec npm run start:dev'
  cd ~/sessionwiz && newt exec npm run start:dev $argv; 
end
