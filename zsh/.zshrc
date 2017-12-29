if ! [[ -f $HOME/.zplug/init.zsh ]]; then
    git clone https://github.com/zplug/zplug.git ~/.zplug
fi
source ~/.zplug/init.zsh

has_command () {
    type $1 &>/dev/null
}

# map, filter, fold
zplug "Tarrasch/zsh-functional"

if has_command jq &&
    (has_command fzf || has_command peco); then
    # use C-s to complete emoji
    zplug "b4b4r07/emoji-cli"
fi

# disable these on Android
if [[ -z $ANDROID_ROOT ]]; then
    # send notification for long running commands
    zplug "marzocchi/zsh-notify"
fi

# an async prompt
zplug "eendroroy/alien"

zplug "zsh-users/zsh-autosuggestions"
zplug "hlissner/zsh-autopair", defer:2
zplug "zdharma/fast-syntax-highlighting"
zplug load

# cd to git repo root
# http://blog.sushi.money/entry/20100211/1265879271
u () {
    cd ./$(git rev-parse --show-cdup)
    if [ $# = 1 ]; then
        cd $1
    fi
}

bindkey -v
export KEYTIMEOUT=1
