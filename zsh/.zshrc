#!/bin/zsh
source ~/.zsh/my-functions.zsh

# Initialize zplug
if [[ -f ~/.zplug/init.zsh ]]; then
    source ~/.zplug/init.zsh
elif __ask "zplug not found. Install it?"; then
    __install_zsh
    source ~/.zplug/init.zsh
else
    zplug () {return;}
fi

# Libraries
zplug "Tarrasch/zsh-functional" # map, filter, fold
zplug "mafredri/zsh-async", defer:0

if __has_command jq fzf; then
    zplug "b4b4r07/emoji-cli" # C-s emoji completion
fi

# Theme
# zplug "dracula/zsh", as:theme
# zplug "carloscuesta/materialshell", use:materialshell, from:github, as:theme
zplug "tylerreckart/hyperzsh", as:theme

# Shell behavior
zplug "zsh-users/zsh-autosuggestions"
zplug "lib/completion", from:oh-my-zsh
zplug "hlissner/zsh-autopair", defer:2
zplug "zdharma/fast-syntax-highlighting"
# zplug "zsh-users/zsh-history-substring-search"
# bindkey '^[[A' history-substring-search-up
# bindkey '^[[B' history-substring-search-down
# bindkey -M vicmd 'k' history-substring-search-up
# bindkey -M vicmd 'j' history-substring-search-down

# send notification for long running commands
zplug "marzocchi/zsh-notify", if:"[[ -z $ANDROID_ROOT ]]"

# Apps
zplug "kisaragi-hiu/randomwallpaper", as:command, use:"randomwallpaper"
zplug "mrowa44/emojify", as:command, use:"emojify"

zplug load

__source_if_present "/usr/share/doc/pkgfile/command-not-found.zsh"

zmodload zsh/complist
setopt menucomplete
zstyle ':completion:*' menu select=0 search

if ! zplug check; then
    __ask "Install all packages specified in .zshrc?" && zplug install
fi

# ~/.zsh/functions/
for i (~/.zsh/functions/*.zsh) source $i

test -f /usr/share/doc/pkgfile/command-not-found.zsh && source /usr/share/doc/pkgfile/command-not-found.zsh
