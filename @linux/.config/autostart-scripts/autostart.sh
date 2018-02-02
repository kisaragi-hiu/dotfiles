#!/bin/bash
# Put all autostart commands in one file
# so I don't need to rely on links

# Vocabulary notification
# loop-command 30m 0 pop-random-notification\
#   /home/flyin1501/git/text-files/vocab-word-30m.txt\
#   /home/flyin1501/git/text-files/vocab-meaning-30m.txt &

loop-command 10m 0 grive -p /home/flyin1501/ドキュメント/ &
ssh-agent &
ssh-add </dev/null
numlockx &

sleep 10 && xcape -e 'Alt_R=Escape;Super_L=Alt_L|F1' -t 250 &
# map tablet area to screen ratio
xsetwacom --set "Wacom Intuos PT S 2 Pen stylus" Area 0 0 15200 8550
# keyboard config with xmodmap
xmodmap <(cat ~/.Xmodmap.d/capslock-to-altgr.xmodmap ~/.Xmodmap)
