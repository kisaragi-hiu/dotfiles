function waitfor
    # Block until $argv[1] to come into existance
    while true
        sleep 1
        if test -e $argv[1]
            break
        end
    end
end

