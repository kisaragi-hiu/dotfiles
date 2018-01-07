#!/bin/bash
# shellcheck disable=SC1090

# == directory shortcuts ==
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

export DOTFILES_DIR="$HOME"/.dotfiles/

# == UI ==
# use qt5ct (within plasma as well)
export QT_QPA_PLATFORMTHEME=qt5ct

# export ELECTRON_FORCE_WINDOW_MENU_BAR=1 # electron global menu is fixed in 1.6.15

# map tablet area to screen ratio
xsetwacom --set "Wacom Intuos PT S 2 Pen stylus" Area 0 0 15200 8550

# keyboard config with xmodmap
xmodmap ~/.Xmodmap

# vsync off by default
export vblank_mode=0

# make sure fcitx is used
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx

# == PATH ==
export GOPATH="$HOME"/.gopath

# exporting PATH here ensures that the desktop also sees them
export PATH=\
"$HOME"/git/scripts:\
"$HOME"/git/Sudocabulary:\
"$HOME"/bin:\
"$HOME"/.racket/6.11/bin:\
"$HOME"/.local/share/npm-global/bin:\ # `npm config set prefix ~/.local/share/npm-global` to set up global node prefix
"$HOME"/.gem/ruby/2.4.0/bin:\
$PATH

# == app behavior controls ==
export SSH_ASKPASS="/usr/bin/ksshaskpass" # for storing ssh keys with kdewallet
export OCIO="$DOTFILES_DIR"/_filmic-blender/config.ocio # filmic blender
export WINEDEBUG=-all # turn off wine debugging

# editor
export VISUAL=nvim
export EDITOR=nvim

# what was this here for??
# export fish_alias="$HOME/.config/fish/conf.d/alias.fish"
# export fish_alias_linux="$HOME/.config/fish/conf.d/alias-linux.fish"

source "$HOME"/.private_env
