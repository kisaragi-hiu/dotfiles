function toggle_greeting
    if [ $fish_disable_greeting -eq 1 -o\
         $fish_disable_greeting -eq "true" -o\
         $fish_disable_greeting -eq "yes" ] 2> /dev/null
        set -x fish_disable_greeting 0
    else if [ $fish_disable_greeting -eq 0 -o\
              $fish_disable_greeting -eq "false" -o\
              $fish_disable_greeting -eq "no" ] 2> /dev/null
        set -x fish_disable_greeting 1
    else if [ -z $fish_disable_greeting ] # if its length is zero
        export fish_disable_greeting=1
    else
        # fish_disable_greeting is something else
        # complain
        echo "fish_disable_greeting does not seem like a boolean value."
        echo "Consider setting it to 1 or 0, which is what this script does."
    end
end
