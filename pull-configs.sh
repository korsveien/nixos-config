#! /usr/bin/env sh

if [ -f $XDF_CONFIG_HOME/nvim ]; then
rm -rf $XDG_CONFIG_HOME/nvim
fi
git clone git@github.com:pederpus/neovim-config.git $XDG_CONFIG_HOME/nvim
