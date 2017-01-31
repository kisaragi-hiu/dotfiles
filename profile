#!/bin/bash

# enable it where you want instead, namely video players
export vblank_mode=0

# qt5ct platform theme
#if [[ "$XDG_CURRENT_DESKTOP" == "KDE" || "$XDG_CURRENT_DESKTOP" == "GNOME" ]]; then
#  export QT_QPA_PLATFORMTHEME="qt5ct"
#fi

# do (set -U fish_user_paths ...) in fish for PATH
#export PATH=/home/moonlightf/git/scripts:/home/moonlightf/bin:$PATH
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx

# use vim instead of vi by default
export VISUAL=vim
export EDITOR=vim
