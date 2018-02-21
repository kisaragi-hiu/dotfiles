function u
    # TODO overload cd to have : stand for gitroot instead
    # so `cd :` or `cd :/repo/rel/path` can be used
    # `cd ./:` should still go to the folder ":"

    # cd to git root
    # ported from http://blog.sushi.money/entry/20100211/1265879271
    cd ./(git rev-parse --show-cdup) # go to git repo root
    if test -n "$argv[1]" # $1 is relative to git repo
        cd $argv[1]
    end
end

