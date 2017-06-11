function fish_greeting
    if test $fish_disable_greeting -eq 1 2> /dev/null
    # do nothing
    else
        Sudocabulary ~/git/text-files/vocab-{word,meaning}-30m.txt
        echo # print extra line
    end
end
