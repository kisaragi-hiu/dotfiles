#!/bin/bash

## while commands do run, variables don't get stored
## source .profile.d
#if test -d $HOME/.profile.d; then
#  for profile in $HOME/.profile.d/*; do
#    source "$profile" &
#  done
#fi

# My config.fish uses this variable
export enable_powerline=0

# setup GOPATH
export GOPATH=/home/moonlightf/.gopath

# for storing ssh keys with kdewallet
export SSH_ASKPASS="/usr/bin/ksshaskpass"

# exporting PATH here ensures the desktop also sees them
export PATH=/home/moonlightf/git/scripts:/home/moonlightf/git/Sudocabulary:/home/moonlightf/bin:/home/moonlightf/.gem/ruby/2.4.0/bin:$PATH

# Map area to screen ratio
xsetwacom --set "Wacom Intuos PT S 2 Pen stylus" Area 0 0 15200 8550

# Make sure fcitx is used
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx

# Filmic Blender can be loaded this way
export OCIO=/home/moonlightf/.dotfiles/filmic-blender/config.ocio

# vim
export VISUAL=vim
export EDITOR=vim

# VSync off by default
export vblank_mode=0
