function get_parent
  echo (ps -o \
         comm= (cat (set self %self
                     echo /proc/$self/status) \
                | grep PPid \
                | cut -d\t -f 2))
  set -e self
end
