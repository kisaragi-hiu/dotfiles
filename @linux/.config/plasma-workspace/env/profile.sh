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
export PATH=/home/flyin1501/git/scripts:/home/flyin1501/git/Sudocabulary:/home/flyin1501/bin:/flyin1501/.local/share/npm-global/bin:/home/flyin1501/.gem/ruby/2.4.0/bin:$PATH

# `npm config set prefix ~/.local/share/npm-global` to set up global node prefix

# map area to screen ratio
xsetwacom --set "Wacom Intuos PT S 2 Pen stylus" Area 0 0 15200 8550

# keyboard config
# TODO: .Xmodmap.d/ (or just port to xkb)
xmodmap ~/.Xmodmap

# make sure fcitx is used
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx

# filmic Blender can be loaded this way
export OCIO=/home/flyin1501/.dotfiles/_filmic-blender/config.ocio

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
export DIARY=/home/flyin1501/git/diary
export XDG_OSU_DIR=/run/media/flyin1501/Data/games/osu

# for XDG_* directory quick access
export XDG_DESKTOP_DIR=/home/flyin1501/デスクトップ/
export XDG_DOWNLOAD_DIR=/home/flyin1501/ダウンロード/
export XDG_TEMPLATES_DIR=/home/flyin1501/テンプレート/
export XDG_PUBLICSHARE_DIR=/home/flyin1501/公開/
export XDG_DOCUMENTS_DIR=/home/flyin1501/ドキュメント/
export XDG_MUSIC_DIR=/home/flyin1501/音楽/
export XDG_PICTURES_DIR=/home/flyin1501/画像/
export XDG_VIDEOS_DIR=/home/flyin1501/ビデオ/

# vsync off by default
export vblank_mode=0