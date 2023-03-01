function reset_bluetooth --wraps='sudo pkill bluetoothd' --description 'alias reset_bluetooth sudo pkill bluetoothd'
  sudo pkill bluetoothd $argv
        
end
