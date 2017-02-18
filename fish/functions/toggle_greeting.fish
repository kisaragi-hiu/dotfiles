function toggle_greeting
    switch "$fish_disable_greeting"
        case 1 'y*' 'on'
            if test "$argv[1]" = "-v"
                echo \n"greeting enabled"
            end
            set -gx fish_disable_greeting 0
        case 0 'n*' 'off' ''
            if test "$argv[1]" = "-v"
                echo \n"greeting disabled"
            end
            set -gx fish_disable_greeting 1
        case '*'
            echo "fish_disable_greeting does not seem like a boolean value."
    end   
end
