#! /usr/bin/env sh

ln -s /home/pederpus/nixos-config/configuration.nix /etc/nixos/configuration.nix
ln -s /etc/nixos/hardware-configuration.nix /home/pederpus/nixos-config/hardware-configuration.nix

ln -s /home/pederpus/nixos-config/dotfiles/profile /home/pederpus/.profile
ln -s /home/pederpus/nixos-config/dotfiles/gitconfig /home/pederpus/.gitconfig
ln -s /home/pederpus/nixos-config/dotfiles/xinitrc /home/pederpus/.xinitrc
ln -s /home/pederpus/nixos-config/dotfiles/Xmodmap /home/pederpus/.Xmodmap

git clone git@github.com:pederpus/neovim-config.git $XDG_CONFIG_HOME/nvim

