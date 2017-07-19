#!/bin/bash

# use qt5ct outside plasma
[ "$XDG_CURRENT_DESKTOP" != plasma ] && export QT_QPA_PLATFORMTHEME=qt5ct

# Workaround electron apps' global menu not functioning
# by disabling election's global menu integration
export ELECTRON_FORCE_WINDOW_MENU_BAR=1

# setup GOPATH
export GOPATH=/home/flyin1501/.gopath

# for storing ssh keys with kdewallet
export SSH_ASKPASS="/usr/bin/ksshaskpass"

# exporting PATH here ensures that the desktop also sees them
export PATH=/home/flyin1501/git/scripts:/home/flyin1501/git/Sudocabulary:/home/flyin1501/bin:/flyin1501/.local/share/npm-global:$PATH

# `npm config set prefix ~/.local/share/npm-global` to set up global node prefix

# map area to screen ratio
xsetwacom --set "Wacom Intuos PT S 2 Pen stylus" Area 0 0 15200 8550

# running multiple xmodmap -e commands seems to be more reliable
# for some reason xmodmap ~/.Xmodmap needs to be run twice to achieve this
# remap CapsLock(keycode 66) to U25CA(◊) via unused keysym XF86Launch0
xmodmap -e 'clear lock'
xmodmap -e 'keycode 66 = XF86Launch0'
xmodmap -e 'keysym XF86Launch0 = U25CA'

# make sure fcitx is used
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx

# filmic Blender can be loaded this way
export OCIO=/home/flyin1501/.dotfiles/filmic-blender/config.ocio

# turn off wine debugging
export WINEDEBUG=-all

# vim
export VISUAL=vim
export EDITOR=vim

# dir variables
export D=/run/media/flyin1501/Data
export C=/run/media/flyin1501/Windows
export G=/home/flyin1501/ドキュメント
export M=/run/media/flyin1501/Data/mega
export P=/run/media/flyin1501/Data/mega/Projects
# grab XDG_* from user-dirs.dirs
for i in $(sed 's/#.*//g' ~/.config/user-dirs.dirs | grep XDG | sed 's/XDG_//g' | sed 's/_.*//'); do
  export XDG_"$i"_DIR=$(xdg-user-dir $i)/
done

# vsync off by default
export vblank_mode=0
