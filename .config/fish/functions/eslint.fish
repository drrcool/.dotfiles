function eslint --wraps='npx eslint . --ext .ts,.tsx --color --fix --ignore-path .gitignore' --description 'alias eslint npx eslint . --ext .ts,.tsx --color --fix --ignore-path .gitignore'
  npx eslint . --ext .ts,.tsx --color --fix --ignore-path .gitignore $argv; 
end
