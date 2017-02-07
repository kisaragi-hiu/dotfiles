function patch-chrome-passwdstore
  for i in ~/.local/share/applications/chrome-*
    sed -i 's/google-chrome-unstable/google-chrome-unstable --password-store=gnome/g' $i
  end
end
