function toggle_greeting
    if test $fish_disable_greeting -eq 1 2> /dev/null
        set -gx fish_disable_greeting 0
        echo 0
    else if test $fish_disable_greeting -eq 0 2> /dev/null
        set -gx fish_disable_greeting 1
        echo 1
    else if test -z $fish_disable_greeting # if its length is zero
        set -gx fish_disable_greeting 1
        echo 2
    else
        # fish_disable_greeting is something else
        # complain
        echo "fish_disable_greeting does not seem like a boolean value."
        exit 1
    end
end
