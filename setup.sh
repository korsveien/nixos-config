#! /usr/bin/env sh

echo "Setting up symbolic links..."
#ln -s $HOME/nixos-config/configuration.nix /etc/nixos/configuration.nix
#ln -s /etc/nixos/hardware-configuration.nix $HOME/nixos-config/hardware-configuration.nix

rm $HOME/.gitconfig
rm $HOME/.bashrc
rm $HOME/.xsession
rm $HOME/.Xmodmap
rm $HOME/.Xdefaults

ln -s $HOME/nixos-config/dotfiles/bashrc $HOME/.bashrc
ln -s $HOME/nixos-config/dotfiles/gitconfig $HOME/.gitconfig
ln -s $HOME/nixos-config/dotfiles/xsession $HOME/.xsession
ln -s $HOME/nixos-config/dotfiles/Xmodmap $HOME/.Xmodmap
ln -s $HOME/nixos-config/dotfiles/Xdefaults $HOME/.Xdefaults

rm -rf $XDG_CONFIG_HOME/termite
mkdir -p $XDG_CONFIG_HOME/termite
ln -s $HOME/nixos-config/termite/config $XDG_CONFIG_HOME/termite

rm -rf $XDG_CONFIG_HOME/gtk-3.0
mkdir -p $XDG_CONFIG_HOME/gtk-3.0
ln -s $HOME/nixos-config/gtk/gtk.css $XDG_CONFIG_HOME/gtk-3.0

echo "Cloning neovim-config into $XDG_CONFIG_HOME/nvim..."
git clone git@github.com:pederpus/neovim-config.git $XDG_CONFIG_HOME/nvim

