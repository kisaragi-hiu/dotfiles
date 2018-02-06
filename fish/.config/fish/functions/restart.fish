function restart
    # pkill $1 && $1
    # requires: launch
    pkill $argv[1]
    launch $argv[1]
end

