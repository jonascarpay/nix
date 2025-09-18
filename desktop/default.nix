{ pkgs, inputs, ... }:
let

  colors = (import ../home/everforest.nix).default;
  focused-dir =
    let
      pwdx = "${pkgs.procps}/bin/pwdx";
      ps = "${pkgs.procps}/bin/ps";
    in
    pkgs.writeShellScript "getFocusedDir" ''
      set -e
      WINDOW_PID=$(niri msg focused-window | awk '/PID:/ {print $2}')
      SHELL_PID=$(${ps} --ppid $WINDOW_PID -o pid=)
      DIR=$(${pwdx} $SHELL_PID | cut -d' ' -f 2)
      echo "$DIR"
    '';


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

  alacritty-fuzzel = pkgs.writeShellScript "alacritty-fuzzel" ''
    set -e
    DIR=$(fuzzel-directory)
    alacritty --working-directory "$DIR"
  '';

  alacritty-focused = pkgs.writeShellScript "alacritty-focused" ''
    DIR=$(${focused-dir})
    if [ -d "$DIR" ]; then
      alacritty --working-directory "$DIR"
    else
      alacritty
    fi
  '';

  neovide-fuzzel = pkgs.writeShellScript "neovide-fuzzel" ''
    set -e
    DIR=$(fuzzel-directory)
    cd $DIR
    direnv exec . neovide .
  '';

  neovide-focused = pkgs.writeShellScript "neovide-focused" ''
    DIR=$(${focused-dir})
    if [ -d "$DIR" ]; then
      cd $DIR
      direnv exec . neovide .
    else
      ${neovide-fuzzel}
    fi
  '';


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
  programs.niri.package = inputs.niri-flake.packages.${pkgs.system}.niri-unstable;
  niri-flake.cache.enable = false;

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

    home.packages = [ pkgs.wl-clipboard ];

    home.sessionVariables.NIXOS_OZONE_WL = 1;
    services.mako = {
      enable = true;
      settings.border-radius = 4;
      settings.default-timeout = 10 * 1000;
    };

    programs.niri.settings = {
      # Default config: https://github.com/YaLTeR/niri/blob/main/resources/default-config.kdl
      # Niri flake docs: https://github.com/sodiboo/niri-flake/blob/main/docs.md
      binds =
        let
          actions = config.lib.niri.actions;
          directional = { up, down, left, right }: {
            "Mod+${left}".action = actions.focus-column-left;
            "Mod+${down}".action = actions.focus-window-or-workspace-down;
            "Mod+${up}".action = actions.focus-window-or-workspace-up;
            "Mod+${right}".action = actions.focus-column-right;
            "Mod+Shift+${left}".action = actions.move-column-left;
            "Mod+Shift+${down}".action = actions.move-window-down-or-to-workspace-down;
            "Mod+Shift+${up}".action = actions.move-window-up-or-to-workspace-up;
            "Mod+Shift+${right}".action = actions.move-column-right;
            "Mod+Ctrl+${down}".action = actions.move-workspace-down;
            "Mod+Ctrl+${up}".action = actions.move-workspace-up;
          };
        in
        directional { left = "H"; down = "J"; up = "K"; right = "L"; } //
        directional { left = "Left"; down = "Down"; up = "Up"; right = "Right"; } //
        {
          "Mod+F".action.spawn = "firefox";
          "Mod+Shift+Slash".action = actions.show-hotkey-overlay;
          "Mod+Return".action.spawn = "${alacritty-focused}";
          "Mod+Shift+Return".action.spawn = "${alacritty-fuzzel}";
          "Mod+Ctrl+Return".action.spawn = "alacritty";

          "Mod+N".action.spawn = "${neovide-focused}";
          "Mod+Shift+N".action.spawn = "${neovide-fuzzel}";
          "Mod+Ctrl+N".action.spawn = "neovide";

          "Mod+O".action.spawn = "fuzzel";

          "Mod+Q".action = actions.close-window;

          "Mod+Comma".action = actions.focus-column-first;
          "Mod+Period".action = actions.focus-column-last;
          "Mod+Shift+Comma".action = actions.move-column-to-first;
          "Mod+Shift+Period".action = actions.move-column-to-last;

          "Mod+WheelScrollDown" = { cooldown-ms = 150; action = actions.focus-workspace-down; };
          "Mod+WheelScrollUp" = { cooldown-ms = 150; action = actions.focus-workspace-up; };
          "Mod+Shift+WheelScrollDown" = { cooldown-ms = 150; action = actions.focus-column-right; };
          "Mod+Shift+WheelScrollUp" = { cooldown-ms = 150; action = actions.focus-column-left; };

          "Mod+Ctrl+WheelScrollDown" = { cooldown-ms = 150; action = actions.move-column-to-workspace-down; };
          "Mod+Ctrl+WheelScrollUp" = { cooldown-ms = 150; action = actions.move-column-to-workspace-up; };
          "Mod+Ctrl+Shift+WheelScrollDown" = { cooldown-ms = 150; action = actions.move-column-right; };
          "Mod+Ctrl+Shift+WheelScrollUp" = { cooldown-ms = 150; action = actions.move-column-left; };

          "Mod+WheelScrollLeft" = { cooldown-ms = 150; action = actions.focus-column-right; };
          "Mod+WheelScrollRight" = { cooldown-ms = 150; action = actions.focus-column-left; };
          "Mod+Ctrl+WheelScrollLeft" = { cooldown-ms = 150; action = actions.move-column-right; };
          "Mod+Ctrl+WheelScrollRight" = { cooldown-ms = 150; action = actions.move-column-left; };

          # Mod+T { spawn "bash" "-c" "notify-send hello && exec alacritty"; }
          XF86AudioRaiseVolume = { action.spawn = [ "wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "0.1+" ]; allow-when-locked = true; };
          XF86AudioLowerVolume = { action.spawn = [ "wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "0.1-" ]; allow-when-locked = true; };
          XF86AudioMute = { action.spawn = [ "wpctl" "set-mute" "@DEFAULT_AUDIO_SINK@" "toggle" ]; allow-when-locked = true; };
          XF86AudioMicMute = { action.spawn = [ "wpctl" "set-mute" "@DEFAULT_AUDIO_SOURCE@" "toggle" ]; allow-when-locked = true; };

          "Mod+1".action.focus-workspace = 1;
          "Mod+2".action.focus-workspace = 2;
          "Mod+3".action.focus-workspace = 3;
          "Mod+4".action.focus-workspace = 4;
          "Mod+5".action.focus-workspace = 5;
          "Mod+6".action.focus-workspace = 6;
          "Mod+7".action.focus-workspace = 7;
          "Mod+8".action.focus-workspace = 8;
          "Mod+9".action.focus-workspace = 9;
          "Mod+Shift+1".action.move-column-to-workspace = 1;
          "Mod+Shift+2".action.move-column-to-workspace = 2;
          "Mod+Shift+3".action.move-column-to-workspace = 3;
          "Mod+Shift+4".action.move-column-to-workspace = 4;
          "Mod+Shift+5".action.move-column-to-workspace = 5;
          "Mod+Shift+6".action.move-column-to-workspace = 6;
          "Mod+Shift+7".action.move-column-to-workspace = 7;
          "Mod+Shift+8".action.move-column-to-workspace = 8;
          "Mod+Shift+9".action.move-column-to-workspace = 9;

          "Mod+Tab".action = actions.focus-workspace-previous;

          "Mod+d".action = actions.toggle-overview;

          "Mod+BracketLeft".action = actions.consume-or-expel-window-left;
          "Mod+BracketRight".action = actions.consume-or-expel-window-right;

          "Mod+Space".action = actions.maximize-column;
          "Mod+Shift+Space".action = actions.fullscreen-window;
          "Mod+Shift+F".action = actions.expand-column-to-available-width;
          # "Mod+C".action = actions.center-column;
          "Mod+C".action = actions.center-visible-columns;
          "Mod+Shift+C".action = actions.center-column;

          "Mod+Minus".action.set-column-width = "-10%";
          "Mod+Equal".action.set-column-width = "+10%";

          "Mod+Shift+Minus".action.set-window-height = "-10%";
          "Mod+Shift+Equal".action.set-window-height = "+10%";

          "Mod+E".action.set-column-width = "33.3%";
          "Mod+R".action.set-column-width = "50%";
          "Mod+T".action.set-column-width = "66.7%";

          "Mod+V".action = actions.toggle-window-floating;
          "Mod+Shift+V".action = actions.switch-focus-between-floating-and-tiling;
          "Mod+W".action = actions.toggle-column-tabbed-display;

          "Mod+Apostrophe".action = actions.screenshot;
          # "Mod+Ctrl+Apostrophe".action = actions.screenshot-screen;
          # "Mod+Shift+Apostrophe".action = actions.screenshot-window;

          "Mod+Escape" = { action = actions.toggle-keyboard-shortcuts-inhibit; allow-inhibiting = false; };
          "Mod+Shift+E".action = actions.quit;
          "Mod+Shift+P".action = actions.power-off-monitors;
        };

      input.workspace-auto-back-and-forth = true;
      prefer-no-csd = true;
      layout = {
        always-center-single-column = true;
        default-column-width.proportion = 0.5;
        focus-ring.width = 2;
        shadow.enable = true;

        default-column-display = "tabbed";
        tab-indicator = {
          hide-when-single-tab = true;
          width = 6.0;
        };
      };

      window-rules = [{
        clip-to-geometry = true;
        geometry-corner-radius = let radius = 8.0; in {
          top-right = radius;
          top-left = radius;
          bottom-right = radius;
          bottom-left = radius;
        };
      }];

      cursor.hide-after-inactive-ms = 10000;
    };
  };
}
