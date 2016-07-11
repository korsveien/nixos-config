# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./desktop/gtk-theme.nix
    ];

  nixpkgs.config.allowUnfree = true;

  # Run garbage collector each night
  nix.gc.automatic = true;
  nix.gc.dates = "03:00";


# List packages installed in system profile. To search by name, run:
# $ nix-env -qaP | grep wget
  environment = {
	  systemPackages = with pkgs; [
		  bash
		  chromium
		  coreutils
		  dropbox
		  dropbox-cli
		  feh
		  oracle-instantclient
		  firefox
		  ghc
		  git
		  haskellPackages.xmobar
		  hexchat
		  htop
		  httpie
		  i3lock
		  irssi
		  idea.idea-community 
		  neovim
		  nmap
		  nodejs
		  rofi
		  screenfetch
		  silver-searcher
		  termite
          telnet
		  tree
		  lxappearance
		  unzip
		  wget
		  which
		  sublime3
		  xclip
          maven
		  autojump
		  ];

	  variables = {
		  EDITOR="nvim";
		  BROWSER="chromium-browser";
		  XDG_CONFIG_HOME="/home/pederpus/.config";
	  };
  };

	boot = {
		initrd = {
			checkJournalingFS = false;
		};
		loader.grub = {
			enable = true;
			version = 2;
			device = "/dev/sda";
		};
	};

	networking = {
		hostName = "nixos-vbox";
		extraHosts = "127.0.0.1 localdb";
        wireless.enable = true;
	};
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Select internationalisation properties.
   i18n = {
     consoleKeyMap = "us";
     defaultLocale = "en_US.UTF-8";
   };


  # Set your time zone.
  time.timeZone = "Europe/Oslo";


  # List services that you want to enable:

	fonts = {
		enableFontDir = true;
		enableGhostscriptFonts = true;
		fonts = with pkgs; [
			hack-font
			inconsolata
		];
	};

	services = {
		openssh.enable = true;

		xserver = {

			enable = true;
			layout = "us";
			xkbOptions = "eurosign:e, caps:none";
            xkbVariant = "altgr-intl";


			displayManager = {
				slim.enable = true;
				slim.defaultUser = "pederpus";
				slim.autoLogin = true;
				sessionCommands = ''
					sh /home/pederpus/nixos-config/symlinks.sh
					xrdb -merge $HOME/.Xdefaults
					xmodmap $HOME/.Xmodmap
				'';
			};

			desktopManager = {
				xterm.enable = false;
                default = "none";
			};

			windowManager = {
				xmonad.enable = true;
				xmonad.enableContribAndExtras = true;
				default = "xmonad";
			};
		};
	};

	virtualisation.docker.enable = true;


	# Enable ssh-add. On by default.
	programs = {
		ssh = {
			startAgent = true;
			agentTimeout = null; #keep keys in memory forever.
		};
		zsh.enable = true;
	};


   # Set zsh as default shell system-wide
   users.defaultUserShell = "/run/current-system/sw/bin/zsh";

  # Define a user account. Don't forget to set a password with ‘passwd’.
   users.extraUsers.pederpus = {
     description = "Peder Korsveien";
     extraGroups = ["wheel" "networkmanager" "docker"];
     isNormalUser = true;
     uid = 1000;
   };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.03";

}
