#! /usr/bin/env sh

echo "Setting up symbolic links..."

rm $HOME/.gitconfig
rm $HOME/.zshrc
rm $HOME/.xsession
rm $HOME/.Xmodmap
rm $HOME/.Xdefaults
rm $HOME/.ssh/config

ln -s $HOME/nixos-config/dotfiles/ssh_config $HOME/.ssh/config
ln -s $HOME/nixos-config/dotfiles/zshrc $HOME/.zshrc
ln -s $HOME/nixos-config/dotfiles/gitconfig $HOME/.gitconfig
ln -s $HOME/nixos-config/dotfiles/xsession $HOME/.xsession
ln -s $HOME/nixos-config/dotfiles/Xmodmap $HOME/.Xmodmap
ln -s $HOME/nixos-config/dotfiles/Xdefaults $HOME/.Xdefaults

rm -rf $HOME/.xmonad
mkdir -p $HOME/.xmonad
ln -s $HOME/nixos-config/dotfiles/xmonad.hs $HOME/.xmonad/xmonad.hs
ln -s $HOME/nixos-config/dotfiles/xmobar.hs $HOME/.xmonad/xmobar.hs

rm -rf $XDG_CONFIG_HOME/termite
mkdir -p $XDG_CONFIG_HOME/termite
ln -s $HOME/nixos-config/termite/config $XDG_CONFIG_HOME/termite

rm -rf $XDG_CONFIG_HOME/gtk-3.0
mkdir -p $XDG_CONFIG_HOME/gtk-3.0
ln -s $HOME/nixos-config/gtk/gtk.css $XDG_CONFIG_HOME/gtk-3.0/gtk.css
ln -s $HOME/nixos-config/gtk/settings.ini $XDG_CONFIG_HOME/gtk-3.0/settings.ini

rm -rf $XDG_CONFIG_HOME/rofi
mkdir -p $XDG_CONFIG_HOME/rofi
ln -s $HOME/nixos-config/rofi/config $XDG_CONFIG_HOME/rofi/config

if [ ! -f $XDG_CONFIG_HOME/compton.conf ]; then
	ln -s $HOME/nixos-config/compton.conf $XDG_CONFIG_HOME/compton.conf
fi

echo "Attempting to symlink dotfiles"
ln $HOME/dotfiles $HOME/nixos/dotfiles

echo "Done!"


