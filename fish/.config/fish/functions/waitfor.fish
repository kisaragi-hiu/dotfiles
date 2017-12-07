function waitfor
    # Wait for $argv[1] to come into existance, then run $argv[2..-1]
    while true
        sleep 1
        if test -e $argv[1]
            break
        end
    end
    and eval $argv[2..-1]
end

