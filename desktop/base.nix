# Module with configuration options used on desktop systems.
# Note that "desktop" in this case refers to something that is not a server,
# including laptops.
{ pkgs, lib, config, ... }:

with lib;

let
  cfg = config.desktop;

  comptonStart =
    (pkgs.writeScriptBin "compton-start" ''
      ${pkgs.compton}/bin/compton -b --config $HOME/.compton.conf
    '');
  comptonToggle =
    (pkgs.writeScriptBin "compton-toggle" ''
      killall compton || ${comptonStart}/bin/compton-start
    '');

  lockScreen =
    (pkgs.writeScriptBin "lock-screen" ''
      revert() {
        ${pkgs.xlibs.xset}/bin/xset -dpms
      }
      trap revert SIGHUP SIGINT SIGTERM
      ${pkgs.xlibs.xset}/bin/xset +dpms dpms 600
      tmpdir=/run/user/$UID/lock-screen
      [ -d $tmpdir ] || mkdir $tmpdir
      ${pkgs.scrot}/bin/scrot $tmpdir/screen.png
      # ${pkgs.i3lock}/bin/i3lock -n -i $tmpdir/screen.png &
      # temp_pid=$!
      ${pkgs.imagemagick}/bin/convert $tmpdir/screen.png -scale 10% -scale 1000% $tmpdir/screen.png
      # ${pkgs.imagemagick}/bin/convert -blur 0x2 $tmpdir/screen.png $tmpdir/screen.png
		  ${pkgs.imagemagick}/bin/convert -gravity center -composite -matte \
        $tmpdir/screen.png ${lockIcon} $tmpdir/screen.png
      ${pkgs.i3lock}/bin/i3lock -n -i $tmpdir/screen.png &
      i3pid=$!
      # kill $temp_pid
      wait $i3pid
      rm $tmpdir/screen.png
      revert
    '');
  lockIcon = ./lockicon.png;

  todoT = pkgs.fetchhg {
    url = "https://bitbucket.org/sjl/t/";
  };

  customSt = pkgs.st.override {
    conf = readFile ./st-config.h;
  };

  # Runs a terminal in the given directory.
  viewDir =
    (pkgs.writeScriptBin "view-dir" ''
      ${customSt}/bin/st ${pkgs.bash}/bin/bash -c "cd $1 && ls && ${pkgs.bash}/bin/bash"
    '');
in
{
  imports = [
    ./xmonad.nix
    ./cmds.nix
    ./gtktheme.nix
    ./defaultapps.nix
    ./steam-controller.nix
  ];

  options = {
    desktop = {
      enable = mkEnableOption "desktop";

      sleepLock.enable = mkEnableOption "sleepLock";
    };
  };


  config = mkIf cfg.enable {
    desktop.mimeapps = {
      extraDesktopFiles = [
        (pkgs.makeDesktopItem {
          name = "st-view-dir";
          exec = "${viewDir}/bin/view-dir %f";
          mimeType = "inode/directory";
          desktopName = "View Directory";
          genericName = "NixOS default";
        })
      ];
      defaults = {
        "inode/directory" = "st-view-dir.desktop";
        "application/pdf" = "evince.desktop";
      };
    };

    environment.systemPackages = with pkgs; [
      lxappearance
    ];

    environment.shellAliases = {
      g = "git";
    };

    services = {
      xserver = {
        enable = true;
        layout = "us";
        xkbOptions = "eurosign:e, caps:none";


        displayManager = {
          slim.enable = true;
          slim.defaultUser = "pederpus";
          slim.autoLogin = true;
          sessionCommands = ''
              sh /home/pederpus/nixos-config/symlinks.sh
              xrdb -merge $HOME/.Xdefaults
              rofi
              xsetroot -cursor_name left_ptr
              xmodmap $HOME/.Xmodmap
              feh --bg-scale /home/pederpus/nixos-config/background.png
              ssh-add
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

    hardware.opengl = {
      enable = true;
      driSupport = true;
      driSupport32Bit = true;
    };

    hardware.pulseaudio = {
      enable = true;
      support32Bit = true;
    };

    # Font Settings
    fonts = {
      enableFontDir = true;
      enableGhostscriptFonts = true;
      fonts = with pkgs; [
        hack-font
        corefonts  # Micrsoft free fonts
        inconsolata  # monospaced
        ipafont
        ubuntu_font_family  # Ubuntu fonts
        source-code-pro
        opensans-ttf
      ];
    };
}
