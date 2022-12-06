function pylon --wraps='ssh root@rcool.pylon.prod.container.dataeng.netflix.net' --description 'alias pylon=ssh root@rcool.pylon.prod.container.dataeng.netflix.net'
  ssh root@rcool.pylon.prod.container.dataeng.netflix.net $argv; 
end
