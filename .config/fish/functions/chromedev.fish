function chromedev
    if  set -q argv
        alias port=$argv
    else
        alias port=3000
    end
       "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome" --remote-debugging-port=9222
end
