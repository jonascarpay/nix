{ pkgs, ... }:
let
  manageX = true;
  channels = import ~/dotfiles/channels.nix;
  unstable = import <unstable> { };
in
{
  imports = [
    ./home-modules/agda.nix
    ./home-modules/albert
    ./home-modules/blender.nix
    ./home-modules/caches.nix
    ./home-modules/emacs
    ./home-modules/firefox.nix
    ./home-modules/fish.nix
    ./home-modules/picom.nix
    ./home-modules/polybar.nix
    ./home-modules/ranger
    ./home-modules/rofi.nix
    ./home-modules/scripts
    # ./home-modules/slipbox.nix
    ./home-modules/vim
    ./home-modules/wal.nix
    ./home-modules/xmonad
    ./xcjp-cache.nix
    ./secrets.nix
  ];

  caches = {
    # cachix = [{
    #   name = "iohk";
    #   sha256 = "0ds8j8g3rp9jam7kb0040smrjhnrrcgc0xjpnhmy6iq9fkm6zja4";
    # }];
    extraCaches = {
      iohk-hydra = {
        url = "https://hydra.iohk.io";
        key = "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=";
      };
    };
  };

  manual = {
    manpages.enable = true;
    html.enable = true;
    json.enable = true;
  };

  fonts.fontconfig.enable = true;

  home = {
    packages = with pkgs; [
      ag
      anki
      entr
      gnome3.nautilus
      haskellPackages.cabal-bounds # TODO move to formatter
      killall
      celluloid
      neofetch
      okular
      pandoc
      pavucontrol
      pidgin
      python37Packages.ueberzug # for image previews
      s-tui
      signal-desktop
      skype
      slack
      spotify
      steam
      sxiv
      tealdeer
      tmux
      transmission-gtk
      unstable.cached-nix-shell
      # unstable.lutris
      unzip
      weechat
      xclip # Doesn't work?
      unstable.youtube-dl
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
      ".tmux.conf".text = with pkgs.tmuxPlugins; ''
        # run-shell ${sensible}/share/tmux-plugins/sensible/sensible.tmux
        run-shell ${pain-control}/share/tmux-plugins/pain-control/pain_control.tmux

        set  -g base-index      0
        setw -g pane-base-index 0
        setw -g window-size smallest

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
      # backgroundColor = "rgba(0,0,0,0.95)";
      # cursorBlink = "on";
      # cursorShape = "ibeam";
    };

    home-manager = {
      enable = true;
      # setting path seems dangerous since it creates a circular dependency
      # path = "https://github.com/rycee/home-manager/archive/master.tar.gz";
      # path = https://github.com/rycee/home-manager/archive/release-20.09.tar.gz;
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
      ignores = [ "*~" "*.swp" "*.swo" "tags" "TAGS" "result" "result-*" ];
      extraConfig.commit.verbose = true;
    };
  };

  services = {
    flameshot.enable = manageX;
    unclutter.enable = manageX;
    syncthing = {
      enable = true;
      tray = true;
    };
    caffeine.enable = true;
    dunst.enable = manageX;
    # network-manager-applet.enable = manageX;
    # xscreensaver.enable = true;

    redshift =
      let
        delft = {
          latitude = "52.0115769";
          longitude = "4.3570677";
        };
        tokyo = {
          latitude = "35.6762";
          longitude = "139.6503";
        };
      in
      {
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
  systemd.user.services.orgsync = {
    Unit.Description = "Org github sync";
    Service = {
      Type = "oneshot";
      ExecStart =
        let
          script = pkgs.writeShellScript "org-sync" ''
            cd ~/Org
            ${pkgs.gitAndTools.git-sync}/bin/git-sync
          '';
        in
        "${script}";
    };
  };
  systemd.user.timers.orgsync = {
    Unit.Description = "Org sync timer";
    Timer.OnCalendar = "*:0/15";
    Install.WantedBy = [ "timers.target" ];
  };

  xsession = {
    enable = manageX;
    initExtra = "${pkgs.albert}/bin/albert &";
  };
}

# vim: fdm=indent:foldcolumn=3:foldlevel=1
