#!/bin/bash
# Put all autostart commands in one file
# so I don't need to rely on links

# Vocabulary notification
loop-command 30m 0 pop-random-notification\
  /home/flyin1501/git/text-files/vocab-word-30m.txt\
  /home/flyin1501/git/text-files/vocab-meaning-30m.txt &

loop-command 10m 0 grive -p /home/flyin1501/ドキュメント/ &
ssh-agent &
numlockx &
