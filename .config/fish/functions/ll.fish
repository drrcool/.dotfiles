function ll --wraps='exa -lbhH --color-scale --git -icons --group-directories-first' --wraps='exa -lbhH --color-scale --git -iicons --group-directories-first' --wraps='exa -lbhH --color-scale --git --icons --group-directories-first' --description 'alias ll exa -lbhH --color-scale --git --icons --group-directories-first'
  exa -lbhH --color-scale --git --icons --group-directories-first $argv
        
end
