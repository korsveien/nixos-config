#! /usr/bin/env sh

echo "Setting up symbolic links..."

rm $HOME/.gitconfig
rm $HOME/.zshrc
rm $HOME/.xsession
rm $HOME/.Xmodmap
rm $HOME/.Xdefaults
rm $HOME/.xmonad/xmonad.hs
rm $HOME/.xmobarrc
rm $HOME/.ssh/config
rm $XDG_CONFIG_HOME/gtk-3.0/gtk.css

ln -s $HOME/nixos-config/dotfiles/ssh_config $HOME/.ssh/config
ln -s $HOME/nixos-config/dotfiles/zshrc $HOME/.zshrc
ln -s $HOME/nixos-config/dotfiles/gitconfig $HOME/.gitconfig
ln -s $HOME/nixos-config/dotfiles/xsession $HOME/.xsession
ln -s $HOME/nixos-config/dotfiles/Xmodmap $HOME/.Xmodmap
ln -s $HOME/nixos-config/dotfiles/Xdefaults $HOME/.Xdefaults
ln -s $HOME/nixos-config/dotfiles/xmobarrc $HOME/.xmobarrc
ln -s $HOME/nixos-config/gtk/gtk.css $XDG_CONFIG_HOME/gtk-3.0/gtk.css

rm -rf $HOME/.xmonad
mkdir -p $HOME/.xmonad
ln -s $HOME/nixos-config/dotfiles/xmonad.hs $HOME/.xmonad/xmonad.hs

rm -rf $XDG_CONFIG_HOME/termite
mkdir -p $XDG_CONFIG_HOME/termite
ln -s $HOME/nixos-config/termite/config $XDG_CONFIG_HOME/termite
