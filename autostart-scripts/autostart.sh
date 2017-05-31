#!/bin/bash
# Put all autostart commands in one file
# so I don't need to rely on links

##-------REMEMBER TO PUT THEM IN BACKGROUND-------##
echo $(date): does $G work? >> $HOME/.priority_test &

# Vocabulary notification
loop-command 30m 0 pop-random-notification\
  /home/flyin1501/git/notification-srs/vocab-files/vocab-word-30m.txt\
  /home/flyin1501/git/notification-srs/vocab-files/vocab-meaning-30m.txt &

loop-command 10m 0 grive -p /run/media/flyin1501/Data/Google\ ドライブ/ & # Grive
ssh-agent &
numlockx &
screenstalk -t 10m &

# the sleep isn't put in the background, have to find another place for them probably
sleep 4; tilda &

# Megasync would put a window right at the upper-left corner if started before the tray
sleep 2m; megasync &
