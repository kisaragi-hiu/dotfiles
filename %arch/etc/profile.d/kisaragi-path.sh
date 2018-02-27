append_to_path () {
    if [ -d "$HOME" ] && [ -d "$1" ]; then
        PATH="$1:$PATH"
    fi
}

append_to_path "$HOME/git/scripts"
append_to_path "$HOME/git/Sudocabulary"
append_to_path "$HOME/bin"
append_to_path "$HOME/.local/share/npm_global/bin"

if [ -f /usr/bin/racket ]; then
    RACKET_VERSION=$(/usr/bin/racket --version | sed 's/^.*v\(.*\)./\1/')
    append_to_path "$HOME/.racket/"$RACKET_VERSION"/bin"
fi

if [ -d /usr/bin/ruby ]; then
    RUBY_VERSION=$(/usr/bin/ruby --version | cut -d' ' -f 2 | sed 's/[a-zA-Z].*//')
    append_to_path "$HOME/.gem/ruby/"$RUBY_VERSION"/bin"
fi

unset append_to_path
