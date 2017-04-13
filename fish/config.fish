if [ "$enable_powerline" == "1" ] >/dev/null ^/dev/null
  powerline-daemon -q
  set fish_function_path $fish_function_path "/usr/lib/python3.6/site-packages/powerline/bindings/fish"
  powerline-setup
end
