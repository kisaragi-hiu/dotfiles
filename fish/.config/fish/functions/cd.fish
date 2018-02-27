function cd
    # Overloaded cd.
    # When in a git repo, a starting ":" means the repository root.

    # if we're in a repo
    if git rev-parse 2>/dev/null # git rev-parse is really cheap
        # if first byte is a colon
        if test (echo "$argv[1]" | head --bytes 1) = ":"
            # go to repo root first
            builtin cd /(git rev-parse --show-toplevel 2>/dev/null) # failsafe slash
            # now go to the rest of the path
            builtin cd ./(echo "$argv[1]" | tail --bytes +2) # trim the colon
            return # job done, get out
        end # not a colon path, no special treatment needed
    end
    builtin cd $argv
end

