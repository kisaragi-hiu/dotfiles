if [ -f "$HOME/.Xmodmap.d/capslock-to-altgr.xmodmap" ] && [ -f "$HOME/.Xmodmap" ]; then
    xmodmap <(cat "$HOME/.Xmodmap.d/capslock-to-altgr.xmodmap" "$HOME/.Xmodmap")
fi
