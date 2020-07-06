{ pkgs, ... }:
let
  manageX = true;
  channels = import ~/dotfiles/channels.nix;
  unstable = import <unstable> { };
in rec {
  imports =
    [ ./caches.nix ./home-modules ./home-modules/caches.nix ./xcjp-cache.nix ];

  caches.cachix = [{
    name = "iohk";
    sha256 = "0ds8j8g3rp9jam7kb0040smrjhnrrcgc0xjpnhmy6iq9fkm6zja4";
  }];

  home = {
    packages = with pkgs; [
      anki
      ag
      blender
      gnome3.nautilus
      killall
      kakoune-unwrapped
      mpv
      neofetch
      okular
      unstable.cached-nix-shell
      haskellPackages.cabal-bounds
      pandoc
      pavucontrol
      python37Packages.ueberzug # for image previews
      slack
      s-tui
      spotify
      steam
      sxiv
      tealdeer
      transmission-gtk
      tmux
      unzip
      weechat
      xclip
    ];

    sessionVariables = {
      EDITOR = "nvim";
      PAGER = "less";
      TERMCMD = "termite";
    };

    file = {
      stackConfig = {
        target = ".stack/config.yaml";
        text = ''
          templates:
            params:
              author-name: Jonas Carpay
              author-email: jonascarpay@gmail.com
              github-username: jonascarpay
        '';
      };
      ghciConf = {
        target = ".ghci";
        text = ''
          :set prompt "\ESC[1;34m%s\n\ESC[0;34mλ> \ESC[m"
          :set prompt-cont "\ESC[0;34m | \ESC[m"
          :set +m
          :def remain \_args -> pure $ unlines [":reload", ":main"]
        '';
      };
      fancy-uninstall = {
        executable = true;
        target = "fancy-uninstall.sh";
        text = ''
          #!usr/bin/env bash
          nix-env -q | fzf | xargs -I{} nix-env -e {}
        '';
      };
      script = {
        executable = true;
        target = "hello.sh";
        text = ''
          #!usr/bin/env bash
          echo "hello"
        '';
      };
      stack-clean-all = {
        executable = true;
        target = "stack-clean-all.sh";
        text = ''
          #!usr/bin/env bash
          find -type f -name ".stack-work" -exec rm -rf {} \;
        '';
      };
      ".tmux.conf".text = with pkgs.tmuxPlugins; ''
        # run-shell ${sensible}/share/tmux-plugins/sensible/sensible.tmux
        run-shell ${pain-control}/share/tmux-plugins/pain-control/pain_control.tmux

        set  -g base-index      0
        setw -g pane-base-index 0

        set -g status-keys vi
        set -g mode-keys   vi
        setw -g clock-mode-style  24

        set -g mouse on
        bind v split-window -h
        bind V split-window -v
      '';
    };

    keyboard.options = [ "ctrl:nocaps" ];
  };

  gtk = {
    enable = manageX;
    theme = {
      package = pkgs.numix-gtk-theme;
      name = "Numix";
    };
    iconTheme = {
      package = pkgs.numix-icon-theme;
      name = "Numix";
    };
    gtk3.extraCss = ''
      VteTerminal, vte-terminal {
        padding: 9px;
      }
    ''; # Termite padding goes here
  };

  qt = { enable = manageX; };

  programs = {
    fzf.enable = true;
    broot.enable = true;

    termite = {
      enable = true;
      backgroundColor = "rgba(0,0,0,0.90)";
      # cursorBlink = "on";
      # cursorShape = "ibeam";
    };

    home-manager = {
      enable = true;
      path = "https://github.com/rycee/home-manager/archive/master.tar.gz";
      # path = https://github.com/rycee/home-manager/archive/release-20.03.tar.gz;
    };

    autorandr = {
      enable = true;
      hooks = {
        predetect = {
          setup-hidpi = ''
            xrandr --newmode "2560x1440_30.00" 146.25 2560 2680 2944 3328 1440 1443 1448 1468 -HSync +VSync
            xrandr --addmode HDMI-1-2 "2560x1440_30.00"
          '';
        };
        postswitch = {
          restart-xmonad = "xmonad --restart";
          # restart-xmonad = "sleep 5; xmonad --restart";
        };
      };
    };

    git = {
      enable = true;
      userName = "Jonas Carpay";
      userEmail = "jonascarpay@gmail.com";
      ignores = [ "*~" "*.swp" "*.swo" "tags" "TAGS" ];
      extraConfig.commit.verbose = true;
    };
  };

  services = {
    flameshot.enable = manageX;
    unclutter.enable = manageX;
    dunst.enable = manageX;
    # network-manager-applet.enable = manageX;
    # xscreensaver.enable = true;

    # syncthing.enable = true;
    # syncthing.tray = true;

    picom = {
      enable = manageX;
      package = pkgs.callPackage ~/Dev/picom { };
      shadow = true;
      experimentalBackends = true;
      fade = true;
      noDockShadow = false;
      fadeDelta = 4;
      extraOptions = ''
        blur = true;
        blur-method = "dual_kawase";
        blur-background-fixed = true;
        blur-strength = 12;
      '';
      opacityRule = [
        "70:class_i = 'termite' && !focused"
        "70:class_i = 'polybar'"
        "100:class_i = 'Blender'"
        "100:class_i = 'gl'"
        "80:!focused"
      ];
    };

    redshift = let
      delft = {
        latitude = "52.0115769";
        longitude = "4.3570677";
      };
      tokyo = {
        latitude = "35.6762";
        longitude = "139.6503";
      };
    in {
      enable = true;
      tray = true;
      inherit (tokyo) latitude longitude;
    };

  };

  systemd.user.services.mpris = {
    Unit = {
      Description = "Bluetooth media controls";
      After = [ "pulseaudio.service" "bluetooth.service" ];
    };
    Service.ExecStart = "${pkgs.bluez}/bin/mpris-proxy";
  };

  xsession = {
    enable = manageX;
    initExtra = ''
      ${pkgs.albert}/bin/albert &
    '';
  };
}

# vim: fdm=indent:foldcolumn=3:foldlevel=1
