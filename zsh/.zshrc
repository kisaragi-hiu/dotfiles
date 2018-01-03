if [[ ! -f ~/.zplug/init.zsh ]]; then
    git clone https://github.com/zplug/zplug.git ~/.zplug
fi
source ~/.zplug/init.zsh

has_command () {
    type $1 &>/dev/null
}

# libraries
## map, filter, fold
zplug "Tarrasch/zsh-functional"
zplug "mafredri/zsh-async", defer:0

if has_command jq && has_command fzf; then
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
zplug "lib/completion", from:oh-my-zsh
zplug "hlissner/zsh-autopair", defer:2
zplug "zdharma/fast-syntax-highlighting"
zplug load

# ~/.zsh/functions/
for i in $(find ~/.zsh/functions/ -name '*.zsh'); do
    source "$i"
done

# bindkey -v
# export KEYTIMEOUT=1
