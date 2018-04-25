function __init_zplug {
    [[ -n $1 ]] && git clone https://github.com/zplug/zplug.git ~/.zplug
    source ~/.zplug/init.zsh
}

function __ask {
    while true; do
        printf "%s [y/n] " "$1"
        read yn
        case $yn in
            y*) return 0 ;;
            n*) return 1 ;;
            *)
                echo "Not y/n, assuming no"
                return 1
                ;;
        esac
    done
}

function __ask_zplug {
    __ask "zplug not found. Install it?"
}

function __has_command {
    command -v $@ &>/dev/null
}

function __source_if_present {
    [[ -f "$1" ]] && source "$1"
}
