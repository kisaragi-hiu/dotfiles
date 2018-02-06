function launch
    nohup $argv >/dev/null ^/dev/null &
    disown (jobs --last --pid)
end
