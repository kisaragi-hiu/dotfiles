function server-here
    # server-here port [path]
    if test -n "$argv[1]"
        if test -n "$argv[2]"
            pushd "$argv[2]"
        end
        python -m http.server $argv[1]
        if test -n "$argv[2]"
            popd
        end
    else
        python -m http.server 4000
    end
end

