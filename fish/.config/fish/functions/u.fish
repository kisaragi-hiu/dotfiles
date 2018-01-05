function u
    # cd to git root
    # ported from http://blog.sushi.money/entry/20100211/1265879271
    cd ./(git rev-parse --show-cdup)
    if test -n "$argv[1]"
        cd $argv[1]
    end
end

