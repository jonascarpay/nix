{ pkgs, inputs, ... }:
let

  colors = (import ../home/everforest.nix).default;


  # TODO this does not properly handle directories with spaces in their names
  fuzzel-directory = { config, ... }:
    let
      history = "${config.xdg.dataHome}/frecently/directory-history";
      frecently = "${inputs.frecently.defaultPackage.${pkgs.system}}/bin/frecently";
      script = pkgs.writeShellScriptBin "fuzzel-directory" ''
        ${pkgs.python3}/bin/python3 ${./fuzzel-directory.py} ${frecently} ${history} /home/jmc/Dev
      '';
    in
    {
      home.packages = [ script ];
      programs.fish.shellInit = ''
        function __frecently-directory-hook --on-variable PWD --description 'add current directory to directory history'
          ${frecently} bump ${history} "$PWD"
        end
      '';
    };



  neovide = {
    programs.neovide = {
      enable = true;
      settings = {
        font = {
          normal = "SauceCodePro Nerd Font";
        };
      };
    };

    programs.neovim.extraLuaConfig = let padding = "10"; in ''
      vim.g.neovide_padding_top = ${padding}
      vim.g.neovide_padding_bottom = ${padding}
      vim.g.neovide_padding_right = ${padding}
      vim.g.neovide_padding_left = ${padding}
      vim.g.neovide_cursor_trail_size = 0.0
      vim.g.neovide_cursor_animation_length = 0.04
      vim.g.neovide_cursor_animate_command_line = false
      vim.g.neovide_cursor_animate_in_insert_mode = false
    '';

  };

  alacritty = {
    programs.alacritty = {
      enable = true;
      settings = {
        font = {
          normal = { family = "DM Mono Nerd Font"; style = "Regular"; };
          bold.style = "Medium";
          italic.style = "Italic";
          bold_italic.style = "Medium Italic";
        };
        window = {
          dynamic_padding = true;
          padding.x = 4;
          padding.y = 4;
        };
        colors =
          rec {
            primary = {
              background = colors.bg0;
              foreground = colors.fg;
            };
            normal = bright;
            bright = {
              black = colors.bg3;
              red = colors.red;
              green = colors.green;
              yellow = colors.yellow;
              blue = colors.blue;
              magenta = colors.purple;
              cyan = colors.aqua;
              white = colors.fg;
            };
          };
      };
    };
  };

in
{

  imports = [
    inputs.niri-flake.nixosModules.niri
  ];
  programs.niri.enable = true;
  # programs.niri.package = inputs.niri-flake.packages.${pkgs.system}.niri-unstable;
  niri-flake.cache.enable = false;

  # Without this, Firefox (and other apps) can't open file open/save/upload
  # dialogs under niri: the only installed portal backend is
  # xdg-desktop-portal-gnome (pulled in by GDM), which declares UseIn=gnome and
  # is therefore never used under XDG_CURRENT_DESKTOP=niri. Route the FileChooser
  # interface to the gtk backend, and keep gnome for screencast/screenshot.
  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
    config.niri = {
      default = [ "gnome" "gtk" ];
      "org.freedesktop.impl.portal.FileChooser" = [ "gtk" ];
    };
  };

  services.displayManager.autoLogin = {
    enable = false; # TODO sadly doesn't seem to work with Wayland atm?
    user = "jmc";
  };
  services.xserver.displayManager.gdm = {
    enable = true;
    autoLogin.delay = 5;
  };

  home-manager.users.jmc = { config, ... }: {
    imports = [
      neovide
      alacritty
      fuzzel-directory
      ./niri.nix
    ];

    programs.fuzzel = {
      enable = true;
      settings = {
        colors = {
          background = colors.bg3 + "FF";
          text = colors.yellow + "FF";
          prompt = colors.grey2 + "FF";
          input = colors.fg + "FF";
          match = colors.purple + "FF";
        };
        main.use-bold = true;
      };
    };

    home.packages = [
      pkgs.wl-clipboard
    ];

    home.sessionVariables.NIXOS_OZONE_WL = 1;
    services.mako = {
      enable = true;
      settings.border-radius = 4;
      settings.default-timeout = 10 * 1000;
    };


  };
}
