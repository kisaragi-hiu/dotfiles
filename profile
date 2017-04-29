#!/bin/bash

echo $(date): priority test from ~/.profile >> $HOME/.priority_test

# Workaround electron apps' global menu not functioning
# by disabling election's global menu integration
export ELECTRON_FORCE_WINDOW_MENU_BAR=1

# setup GOPATH
export GOPATH=/home/flyin1501/.gopath

# for storing ssh keys with kdewallet
export SSH_ASKPASS="/usr/bin/ksshaskpass"

# exporting PATH here ensures the desktop also sees them
export PATH=/home/flyin1501/git/scripts:/home/flyin1501/git/Sudocabulary:/home/flyin1501/bin:/home/flyin1501/.gem/ruby/2.4.0/bin:$PATH

# map area to screen ratio
xsetwacom --set "Wacom Intuos PT S 2 Pen stylus" Area 0 0 15200 8550

# run this earlier so tilda stops complaining about F24
xmodmap ~/.Xmodmap

# make sure fcitx is used
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx

# filmic Blender can be loaded this way
export OCIO=/home/flyin1501/.dotfiles/filmic-blender/config.ocio

# vim
export VISUAL=vim
export EDITOR=vim

# dir variables
export D=/run/media/flyin1501/Data
export C=/run/media/flyin1501/Windows
export G='/run/media/flyin1501/Data/Google ドライブ'
export M=/run/media/flyin1501/Data/Mega
export P=/run/media/flyin1501/Data/Mega/Projects

# vsync off by default
export vblank_mode=0
