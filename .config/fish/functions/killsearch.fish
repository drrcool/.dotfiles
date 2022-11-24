function killsearch
  set results "$(ps aux | grep $argv)"
  for i in $results
   kill -9 "$(echo $i | awk -F ' ' '{print $2}')"
  end

end

