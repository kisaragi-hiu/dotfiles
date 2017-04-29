#!/bin/bash
# Put all autostart commands in one file
# so I don't need to rely on links

##-------REMEMBER TO PUT THEM IN BACKGROUND-------##
echo $(date): priority test from ~/.config/autostart-scripts/autostart.sh >> $HOME/.priority_test &

# Vocabulary notification
loop-command 30m 0 pop-random-notification\
  /home/flyin1501/git/notification-srs/vocab-files/vocab-word-30m.txt\
  /home/flyin1501/git/notification-srs/vocab-files/vocab-meaning-30m.txt &

# Grive
loop-command 10m 0 grive -p $G/ &

ssh-agent &

numlockx &
screenstalk -t 10 &
sleep 4; tilda &
