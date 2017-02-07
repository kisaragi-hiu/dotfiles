function fish_greeting
    if test $fish_disable_greeting -eq 1 -o\
            $fish_disable_greeting -eq "true" -o\
            $fish_disable_greeting -eq "yes" 2> /dev/null
    # do nothing
    else
        Sudocabulary
        echo # print extra line
    end
end
