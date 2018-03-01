# +arch

System configuration on my Arch desktop.

This should be stowed to / with `--target=/`. To avoid file permission issues, it is also probably a good idea to clone another copy of this repository as root.

Note that wherever this repo sits in, it needs to be world-readable. For instance, `pacman -Ss pkg` reads `/etc/pacman.conf` as the current user, which means that the source file `/path/to/dotfiles/%arch/etc/pacman.conf` needs to be readable as well. Currently I have /root set to `rwxr-xr-x`.
