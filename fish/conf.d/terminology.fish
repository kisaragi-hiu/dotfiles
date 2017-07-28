test (loginctl show-session $XDG_SESSION_ID -p Type) = "Type=tty" # are we in a tty?
and which terminology >/dev/null 2>/dev/null # is terminology installed?
and terminology >/dev/null 2>/dev/null # start terminology
or true # handle non-zero exit code
