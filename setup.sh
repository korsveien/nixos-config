#! /usr/bin/env sh

ln -s $HOME/nixos-config/configuration.nix /etc/nixos/configuration.nix
ln -s /etc/nixos/hardware-configuration.nix $HOME/nixos-config/hardware-configuration.nix

ln -s $HOME/nixos-config/dotfiles/gitconfig $HOME/.gitconfig
ln -s $HOME/nixos-config/dotfiles/profile $HOME/.profile
