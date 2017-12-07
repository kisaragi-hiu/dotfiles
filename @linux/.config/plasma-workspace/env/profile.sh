#!/bin/bash
# shellcheck disable=SC1090

# use qt5ct outside plasma
if [ "$XDG_CURRENT_DESKTOP" != plasma ] || [ "$XDG_CURRENT_DESKTOP" != KDE ]; then
  export QT_QPA_PLATFORMTHEME=qt5ct
fi

# Workaround electron apps' global menu not functioning
# by disabling election's global menu integration
export ELECTRON_FORCE_WINDOW_MENU_BAR=1

# setup GOPATH
export GOPATH="$HOME"/.gopath

# for storing ssh keys with kdewallet
export SSH_ASKPASS="/usr/bin/ksshaskpass"

# exporting PATH here ensures that the desktop also sees them
export PATH=\
"$HOME"/git/scripts:\
"$HOME"/git/Sudocabulary:\
"$HOME"/bin:\
"$HOME"/.racket/6.11/bin:\
"$HOME"/.local/share/npm-global/bin:\
"$HOME"/.gem/ruby/2.4.0/bin:\
$PATH

export fish_alias="$HOME/.config/fish/conf.d/alias.fish"
export fish_alias_linux="$HOME/.config/fish/conf.d/alias-linux.fish"

# `npm config set prefix ~/.local/share/npm-global` to set up global node prefix

# map area to screen ratio
xsetwacom --set "Wacom Intuos PT S 2 Pen stylus" Area 0 0 15200 8550

# keyboard config
# TODO: .Xmodmap.d/ (or just port to xkb)
xmodmap ~/.Xmodmap

# This should be run a bit later. Putting it into autostart-scripts/.
# xcape -e 'Alt_R=Escape;Super_L=Alt_L|F1' -t 250

# make sure fcitx is used
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx

# filmic Blender can be loaded this way
export OCIO="$HOME"/.dotfiles/_filmic-blender/config.ocio

# turn off wine debugging
export WINEDEBUG=-all

# vim
export VISUAL=nvim
export EDITOR=nvim

# dir variables
export D=/run/media/flyin1501/Data
export C=/run/media/flyin1501/Windows
export G="$HOME"/ドキュメント
export M=/run/media/flyin1501/Data/mega
export P=/run/media/flyin1501/Data/mega/Projects
source "$HOME"/.bookmarks

# for XDG_* directory quick access
export XDG_DESKTOP_DIR="$HOME"/デスクトップ/
export XDG_DOWNLOAD_DIR="$HOME"/ダウンロード/
export XDG_TEMPLATES_DIR="$HOME"/テンプレート/
export XDG_PUBLICSHARE_DIR="$HOME"/公開/
export XDG_DOCUMENTS_DIR="$HOME"/ドキュメント/
export XDG_MUSIC_DIR="$HOME"/音楽/
export XDG_PICTURES_DIR="$HOME"/画像/
export XDG_VIDEOS_DIR="$HOME"/ビデオ/

# vsync off by default
export vblank_mode=0

source "$HOME"/.private_env
