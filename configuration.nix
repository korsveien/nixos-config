# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  nixpkgs.config = {
	  allowUnfree = true;
  };


  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
   environment.systemPackages = with pkgs; [
	   arc-gtk-theme
	   chromium
	   coreutils
	   dropbox
	   dropbox-cli
	   feh
	   firefox
	   ghc
	   git
	   haskellPackages.xmobar
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
	   tree
	   unzip
	   vim 
	   wget
	   which
	   xclip
       autojump
   ];

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

   networking.hostName = "nixos-vbox"; # Define your hostname.
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
		xkbOptions = "eurosign:e";


		displayManager = {
			slim.enable = true;
			slim.defaultUser = "pederpus";
			slim.autoLogin = true;
			sessionCommands = ''

				sh /home/pederpus/nixos-config/symlinks.sh &

				xrdb -merge $HOME/.Xdefaults

				sh /home/pederpus/.fehbg &

				rofi &

				xsetroot -cursor_name left_ptr &

				ssh-add &

				xmodmap $HOME/.Xmodmap


			'';
		};

		desktopManager.xterm.enable = false;
		desktopManager.default = "none";

		windowManager = {
			xmonad.enable = true;
			xmonad.enableContribAndExtras = true;
			default = "xmonad";
		};
		};
	};

	environment.sessionVariables = {
		EDITOR="nvim";
		BROWSER="firefox";
		XDG_CONFIG_HOME="/home/pederpus/.config";
	};

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
     extraGroups = ["wheel"];
     isNormalUser = true;
     uid = 1000;
	 openssh.authorizedKeys.keyFiles = [
		"/home/pederpus/.ssh/id_rsa.pub"
	 ];
   };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.03";

}
