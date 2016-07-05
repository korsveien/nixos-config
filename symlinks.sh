#! /usr/bin/env sh

echo "Setting up symbolic links..."
#ln -s $HOME/nixos-config/configuration.nix /etc/nixos/configuration.nix
#ln -s /etc/nixos/hardware-configuration.nix $HOME/nixos-config/hardware-configuration.nix

rm $HOME/.gitconfig
rm $HOME/.zshrc
rm $HOME/.xsession
rm $HOME/.Xmodmap
rm $HOME/.Xdefaults
rm $HOME/.fehbg
rm $HOME/.xmonad/xmonad.hs
rm $HOME/.xmobarrc
rm $HOME/.ssh/config

ln -s $HOME/nixos-config/dotfiles/ssh_config $HOME/.ssh/config
ln -s $HOME/nixos-config/dotfiles/zshrc $HOME/.zshrc
ln -s $HOME/nixos-config/dotfiles/gitconfig $HOME/.gitconfig
ln -s $HOME/nixos-config/dotfiles/xsession $HOME/.xsession
ln -s $HOME/nixos-config/dotfiles/Xmodmap $HOME/.Xmodmap
ln -s $HOME/nixos-config/dotfiles/Xdefaults $HOME/.Xdefaults
ln -s $HOME/nixos-config/dotfiles/fehbg $HOME/.fehbg
ln -s $HOME/nixos-config/dotfiles/xmobarrc $HOME/.xmobarrc

rm -rf $HOME/.xmonad
mkdir -p $HOME/.xmonad
ln -s $HOME/nixos-config/dotfiles/xmonad.hs $HOME/.xmonad/xmonad.hs

rm -rf $XDG_CONFIG_HOME/termite
mkdir -p $XDG_CONFIG_HOME/termite
ln -s $HOME/nixos-config/termite/config $XDG_CONFIG_HOME/termite

rm -rf $XDG_CONFIG_HOME/gtk-3.0
mkdir -p $XDG_CONFIG_HOME/gtk-3.0
ln -s $HOME/nixos-config/gtk/gtk.css $XDG_CONFIG_HOME/gtk-3.0


