function scicomp2 --wraps='ssh -A root@rcool.scicomp2.prod.container.dataeng.netflix.net' --description 'alias scicomp2 ssh -A root@rcool.scicomp2.prod.container.dataeng.netflix.net'
  ssh -A root@rcool.scicomp2.prod.container.dataeng.netflix.net $argv; 
end
