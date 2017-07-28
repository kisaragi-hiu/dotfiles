#powerline-daemon -q
#set fish_function_path $fish_function_path "/usr/lib/python3.6/site-packages/powerline/bindings/fish"
#powerline-setup
if test (loginctl show-session $XDG_SESSION_ID -p Type) = "Type=tty"
  terminology
end
