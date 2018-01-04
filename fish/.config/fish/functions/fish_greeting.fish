function fish_greeting
    # fish_disable_greeting is 1 or Sudocabulary can't be run
    # -> don't do anything
    if test $fish_disable_greeting -eq 1 2> /dev/null
        return
    else if not type -q Sudocabulary
        return
    else
        Sudocabulary ~/git/text-files/vocab-{word,meaning}-30m.txt
        echo # print extra line
    end
end
