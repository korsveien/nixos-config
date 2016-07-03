#! /usr/bin/env sh

echo "Set config location..."
mkdir -p $HOME/.config
export XDG_CONFIG_HOME=/home/pederpus/.config

echo "Setting up symbolic links..."
ln -s /home/pederpus/nixos-config/configuration.nix /etc/nixos/configuration.nix
ln -s /etc/nixos/hardware-configuration.nix /home/pederpus/nixos-config/hardware-configuration.nix

rm /home/pederpus/.gitconfig
rm /home/pederpus/.profile
rm /home/pederpus/.xsession
rm /home/pederpus/.Xmodmap
rm /home/pederpus/.Xresources

ln -s /home/pederpus/nixos-config/dotfiles/profile /home/pederpus/.profile
ln -s /home/pederpus/nixos-config/dotfiles/gitconfig /home/pederpus/.gitconfig
ln -s /home/pederpus/nixos-config/dotfiles/xsession /home/pederpus/.xsession
ln -s /home/pederpus/nixos-config/dotfiles/Xmodmap /home/pederpus/.Xmodmap
ln -s /home/pederpus/nixos-config/dotfiles/Xresources /home/pederpus/.Xresources

echo "Cloning neovim-config into $XDG_CONFIG_HOME/nvim..."
git clone git@github.com:pederpus/neovim-config.git $XDG_CONFIG_HOME/nvim

