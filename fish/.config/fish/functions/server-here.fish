function server-here
    if test -n $argv
        python -m http.server $argv
    else
        python -m http.server 4000
    end
end

