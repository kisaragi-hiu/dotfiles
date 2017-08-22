function get_parent
  echo (ps -o \
         comm= (cat (set self %self
                     echo /proc/$self/status) \
                | grep PPid \
                | cut -d\t -f 2))
  set -e self
end

if test (loginctl show-session $XDG_SESSION_ID -p Type) = "Type=tty" # are we in a tty?
if which terminology >/dev/null 2>/dev/null # is terminology installed?
if test (get_parent) = "login" # is fish evoked by login, ie. directly in a tty?

terminology >/dev/null 2>/dev/null # start terminology when all above are true

end
end
end
