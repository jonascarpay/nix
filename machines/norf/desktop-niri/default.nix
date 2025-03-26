# https://github.com/sodiboo/niri-flake/blob/main/docs.md

{ pkgs, unstable, inputs, ... }:
let
  obsidian = { pkgs, config, ... }: {
    home.packages = [
      pkgs.obsidian
    ];
    services.git-sync = {
      enable = true;
      repositories.obsidian = {
        path = "${config.home.homeDirectory}/Obsidian";
        uri = "git+ssh://git@github.com:jonascarpay/Obsidian.git";
      };
    };
  };

  alacritty-focused =
    let
      pwdx = "${pkgs.procps}/bin/pwdx";
      ps = "${pkgs.procps}/bin/ps";
    in
    pkgs.writeShellScript "getFocusedPwd" ''
      WINDOW_PID=$(niri msg focused-window | awk '/PID:/ {print $2}')
      SHELL_PID=$(${ps} --ppid $WINDOW_PID -o pid=)
      PWD=$(${pwdx} $SHELL_PID | cut -d' ' -f 2)
      if [ -d "$PWD" ]; then
        alacritty --working-directory "$PWD"
      else
        alacritty
      fi
    '';

  history-root = "/home/jmc/.local/share/frecently";
  frecently-pkg = inputs.frecently.defaultPackage.${pkgs.system};
  frecently = "${frecently-pkg}/bin/frecently";
  # TODO this does not properly handle directories with spaces in their names
  fuzzel-directory =
    let
      history = "${history-root}/directory-history";
      script =
        pkgs.writeShellScriptBin "fuzzel-directory" ''
          set -e
          for dir in $(${frecently} view ${history}); do
            if [ ! -d "$dir" ]; then
              echo "Removing $dir"
              ${frecently} delete ${history} "$dir"
            fi
          done
          DIR=$(find $HOME/Dev $HOME/Documents -maxdepth 1 -type d | ${frecently} view ${history} -a | sed "s#$HOME#~#" | fuzzel -d -p " ")
          DIR_REAL=$(realpath "''${DIR/#\~/$HOME}")
          if [ -d $DIR_REAL ]; then
            ${frecently} bump ${history} "$DIR_REAL"
            alacritty --working-directory "$DIR_REAL"
          fi
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


in
{

  imports = [
    inputs.niri-flake.nixosModules.niri
    ../../../nixos/fonts.nix
    ../../../desktop/jp.nix
  ];

  programs.niri.enable = true;
  programs.firefox.enable = true;

  niri-flake.cache.enable = false;

  # This entire thing might not be necessary
  services.xserver.displayManager = {
    gdm.enable = true;
    autoLogin = {
      enable = true;
      user = "jmc";
    };
  };

  home-manager.users.jmc = {

    imports = [
      ./wallpaper.nix
      obsidian
      fuzzel-directory
    ];

    home.packages = [
      pkgs.wl-clipboard
      pkgs.spotify
      pkgs.tdesktop
      pkgs.okular
      unstable.signal-desktop
    ];

    programs.fuzzel = {
      enable = true;
    };

    programs.waybar = {
      enable = true;
      systemd.enable = true;
      settings.mainBar = {
        height = 30;
        layer = "top";
        modules-left = [ "niri/workspaces" "niri/window" ];
        modules-center = [ ];
        modules-right = [ "memory" "cpu" "clock" "tray" ];
        spacing = 4;
        "niri/workspaces" = {
          format = "{value}";
        };
        "niri/window" = {
          icon = true;
          separate-outputs = true;
        };
        clock = {
          format = "{:%a %b %d %H:%M}";
          timezones = [ "Asia/Tokyo" "Europe/Amsterdam" ];
          tooltip-format = "{tz_list}";
        };
        cpu = {
          interval = 1;
          format = "{icon0}{icon1}{icon2}{icon3}{icon4}{icon5}{icon6}{icon7}{icon8}{icon9}{icon10}{icon11}";
          format-icons = [ "▁" "▂" "▃" "▄" "▅" "▆" "▇" "█" ];
        };
      };
    };

    services.wlsunset = {
      enable = true;
      latitude = "35.6762";
      longitude = "139.6503";
      systemdTarget = "niri.service";
      temperature.night = 4500;
    };

    programs.alacritty = {
      enable = true;
      settings = {
        font = {
          normal = { family = "DM Mono Nerd Font"; style = "Regular"; };
          bold.style = "Medium";
          italic.style = "Italic";
          bold_italic.style = "Medium Italic";
          size = 13;
        };
        window = {
          dynamic_padding = true;
          padding.x = 4;
          padding.y = 4;
        };
        colors =
          let
            everforest = import ../../../home/everforest.nix;
            col = everforest.dark.foreground // everforest.dark.background.medium;
          in
          rec {
            primary = {
              background = col.bg0;
              foreground = col.fg;
            };
            normal = bright;
            bright = {
              black = col.bg3;
              red = col.red;
              green = col.green;
              yellow = col.yellow;
              blue = col.blue;
              magenta = col.purple;
              cyan = col.aqua;
              white = col.fg;
            };
          };
      };
    };

    services.mako.enable = true;
    home.sessionVariables.NIXOS_OZONE_WL = 1;
    programs.niri.config = ''
      // https://github.com/YaLTeR/niri/wiki/Configuration:-Overview

      // https://github.com/YaLTeR/niri/wiki/Configuration:-Input
      input {
        workspace-auto-back-and-forth
      }

      // niri msg outputs
      // https://github.com/YaLTeR/niri/wiki/Configuration:-Outputs
      /-output "HDMI-A-2" {
        mode "3840x2160@60.000"
        scale 2
        transform "normal"
        position x=1280 y=0
      }

      // Settings that influence how windows are positioned and sized.
      // Find more information on the wiki:
      // https://github.com/YaLTeR/niri/wiki/Configuration:-Layout
      layout {
        always-center-single-column
        // Set gaps around windows in logical pixels.
        gaps 32

        // You can customize the widths that "switch-preset-column-width" (Mod+R) toggles between.
        preset-column-widths {
          // Proportion sets the width as a fraction of the output width, taking gaps into account.
          // For example, you can perfectly fit four windows sized "proportion 0.25" on an output.
          // The default preset widths are 1/3, 1/2 and 2/3 of the output.
          proportion 0.33333
          proportion 0.5
          proportion 0.66667

          // Fixed sets the width in logical pixels exactly.
          // fixed 1920
        }

        // You can also customize the heights that "switch-preset-window-height" (Mod+Shift+R) toggles between.
        // preset-window-heights { }

        // You can change the default width of the new windows.
        default-column-width { proportion 0.5; }
        // If you leave the brackets empty, the windows themselves will decide their initial width.
        // default-column-width {}

        // By default focus ring and border are rendered as a solid background rectangle
        // behind windows. That is, they will show up through semitransparent windows.
        // This is because windows using client-side decorations can have an arbitrary shape.
        //
        // If you don't like that, you should uncomment `prefer-no-csd` below.
        // Niri will draw focus ring and border *around* windows that agree to omit their
        // client-side decorations.
        //
        // Alternatively, you can override it with a window rule called
        // `draw-border-with-background`.

        focus-ring {
          width 2
          active-color "#7fc8ff"
          inactive-color "#505050"
        }

        border {
          off
        }

        // You can enable drop shadows for windows.
        shadow {
          on
        }

        struts {
          left 32
          right 32
          top 16
          bottom 32
        }
      }

      prefer-no-csd

      // You can change the path where screenshots are saved.
      // A ~ at the front will be expanded to the home directory.
      // The path is formatted with strftime(3) to give you the screenshot date and time.
      screenshot-path "~/Pictures/Screenshots/Screenshot from %Y-%m-%d %H-%M-%S.png"

      // You can also set this to null to disable saving screenshots to disk.
      // screenshot-path null

      // Animation settings.
      // The wiki explains how to configure individual animations:
      // https://github.com/YaLTeR/niri/wiki/Configuration:-Animations
      animations {
        // Uncomment to turn off all animations.
        // off

        // Slow down all animations by this factor. Values below 1 speed them up instead.
        // slowdown 3.0
      }

      // Window rules let you adjust behavior for individual windows.
      // Find more information on the wiki:
      // https://github.com/YaLTeR/niri/wiki/Configuration:-Window-Rules

      // Work around WezTerm's initial configure bug
      // by setting an empty default-column-width.
      window-rule {
        // This regular expression is intentionally made as specific as possible,
        // since this is the default config, and we want no false positives.
        // You can get away with just app-id="wezterm" if you want.
        match app-id=r#"^org\.wezfurlong\.wezterm$"#
        default-column-width {}
      }

      // Open the Firefox picture-in-picture player as floating by default.
      window-rule {
        // This app-id regular expression will work for both:
        // - host Firefox (app-id is "firefox")
        // - Flatpak Firefox (app-id is "org.mozilla.firefox")
        match app-id=r#"firefox$"# title="^Picture-in-Picture$"
        open-floating true
      }

      // Example: block out two password managers from screen capture.
      // (This example rule is commented out with a "/-" in front.)
      /-window-rule {
        match app-id=r#"^org\.keepassxc\.KeePassXC$"#
        match app-id=r#"^org\.gnome\.World\.Secrets$"#

        block-out-from "screen-capture"

        // Use this instead if you want them visible on third-party screenshot tools.
        // block-out-from "screencast"
      }

      // Example: enable rounded corners for all windows.
      // (This example rule is commented out with a "/-" in front.)
      window-rule {
        geometry-corner-radius 4
        clip-to-geometry true
      }

      binds {
        // Keys consist of modifiers separated by + signs, followed by an XKB key name
        // in the end. To find an XKB name for a particular key, you may use a program
        // like wev.
        //
        // "Mod" is a special modifier equal to Super when running on a TTY, and to Alt
        // when running as a winit window.
        //
        // Most actions that you can bind here can also be invoked programmatically with
        // `niri msg action do-something`.

        // Mod-Shift-/, which is usually the same as Mod-?,
        // shows a list of important hotkeys.
        Mod+Shift+Slash { show-hotkey-overlay; }

        // Suggested binds for running programs: terminal, app launcher, screen locker.
        Mod+F { spawn "firefox"; }
        Mod+Return { spawn "${alacritty-focused}"; }
        Mod+Shift+Return { spawn "alacritty"; }
        Mod+O { spawn "fuzzel"; }
        Mod+D { spawn "fuzzel-directory"; }

        // You can also use a shell. Do this if you need pipes, multiple commands, etc.
        // Note: the entire command goes as a single argument in the end.
        // Mod+T { spawn "bash" "-c" "notify-send hello && exec alacritty"; }

        // Example volume keys mappings for PipeWire & WirePlumber.
        // The allow-when-locked=true property makes them work even when the session is locked.
        XF86AudioRaiseVolume allow-when-locked=true { spawn "wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "0.1+"; }
        XF86AudioLowerVolume allow-when-locked=true { spawn "wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "0.1-"; }
        XF86AudioMute    allow-when-locked=true { spawn "wpctl" "set-mute" "@DEFAULT_AUDIO_SINK@" "toggle"; }
        XF86AudioMicMute   allow-when-locked=true { spawn "wpctl" "set-mute" "@DEFAULT_AUDIO_SOURCE@" "toggle"; }

        Mod+Q { close-window; }

        Mod+H   { focus-column-left; }
        Mod+J   { focus-window-or-workspace-down; }
        Mod+K   { focus-window-or-workspace-up; }
        Mod+L   { focus-column-right; }

        Mod+Shift+H   { move-column-left; }
        Mod+Shift+J   { move-window-down-or-to-workspace-down; }
        Mod+Shift+K   { move-window-up-or-to-workspace-up; }
        Mod+Shift+L   { move-column-right; }

        Mod+Ctrl+J { move-workspace-down; }
        Mod+Ctrl+K { move-workspace-up; }

        // TODO find better bindings
        Mod+Comma  { focus-column-first; }
        Mod+Period { focus-column-last; }
        Mod+Shift+Comma  { move-column-to-first; }
        Mod+Shift+Period { move-column-to-last; }

        // You can bind mouse wheel scroll ticks using the following syntax.
        // These binds will change direction based on the natural-scroll setting.
        //
        // To avoid scrolling through workspaces really fast, you can use
        // the cooldown-ms property. The bind will be rate-limited to this value.
        // You can set a cooldown on any bind, but it's most useful for the wheel.
        Mod+WheelScrollDown    cooldown-ms=150 { focus-workspace-down; }
        Mod+WheelScrollUp    cooldown-ms=150 { focus-workspace-up; }
        Mod+Ctrl+WheelScrollDown cooldown-ms=150 { move-column-to-workspace-down; }
        Mod+Ctrl+WheelScrollUp   cooldown-ms=150 { move-column-to-workspace-up; }

        Mod+WheelScrollRight    { focus-column-right; }
        Mod+WheelScrollLeft     { focus-column-left; }
        Mod+Ctrl+WheelScrollRight { move-column-right; }
        Mod+Ctrl+WheelScrollLeft  { move-column-left; }

        // Usually scrolling up and down with Shift in applications results in
        // horizontal scrolling; these binds replicate that.
        Mod+Shift+WheelScrollDown    { focus-column-right; }
        Mod+Shift+WheelScrollUp    { focus-column-left; }
        Mod+Ctrl+Shift+WheelScrollDown { move-column-right; }
        Mod+Ctrl+Shift+WheelScrollUp   { move-column-left; }

        // Similarly, you can bind touchpad scroll "ticks".
        // Touchpad scrolling is continuous, so for these binds it is split into
        // discrete intervals.
        // These binds are also affected by touchpad's natural-scroll, so these
        // example binds are "inverted", since we have natural-scroll enabled for
        // touchpads by default.
        // Mod+TouchpadScrollDown { spawn "wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "0.02+"; }
        // Mod+TouchpadScrollUp   { spawn "wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "0.02-"; }

        // You can refer to workspaces by index. However, keep in mind that
        // niri is a dynamic workspace system, so these commands are kind of
        // "best effort". Trying to refer to a workspace index bigger than
        // the current workspace count will instead refer to the bottommost
        // (empty) workspace.
        //
        // For example, with 2 workspaces + 1 empty, indices 3, 4, 5 and so on
        // will all refer to the 3rd workspace.
        Mod+1 { focus-workspace 1; }
        Mod+2 { focus-workspace 2; }
        Mod+3 { focus-workspace 3; }
        Mod+4 { focus-workspace 4; }
        Mod+5 { focus-workspace 5; }
        Mod+6 { focus-workspace 6; }
        Mod+7 { focus-workspace 7; }
        Mod+8 { focus-workspace 8; }
        Mod+9 { focus-workspace 9; }
        Mod+Shift+1 { move-column-to-workspace 1; }
        Mod+Shift+2 { move-column-to-workspace 2; }
        Mod+Shift+3 { move-column-to-workspace 3; }
        Mod+Shift+4 { move-column-to-workspace 4; }
        Mod+Shift+5 { move-column-to-workspace 5; }
        Mod+Shift+6 { move-column-to-workspace 6; }
        Mod+Shift+7 { move-column-to-workspace 7; }
        Mod+Shift+8 { move-column-to-workspace 8; }
        Mod+Shift+9 { move-column-to-workspace 9; }

        // Alternatively, there are commands to move just a single window:
        // Mod+Ctrl+1 { move-window-to-workspace 1; }

        // Switches focus between the current and the previous workspace.
        // Mod+Tab { focus-workspace-previous; }

        // The following binds move the focused window in and out of a column.
        // If the window is alone, they will consume it into the nearby column to the side.
        // If the window is already in a column, they will expel it out.
        Mod+BracketLeft  { consume-or-expel-window-left; }
        Mod+BracketRight { consume-or-expel-window-right; }

        // Mod+R { switch-preset-column-width; }
        // Mod+Shift+R { switch-preset-window-height; }
        // Mod+Ctrl+R { reset-window-height; }
        Mod+Space { maximize-column; }
        Mod+Shift+Space { fullscreen-window; }

        // Expand the focused column to space not taken up by other fully visible columns.
        // Makes the column "fill the rest of the space".
        Mod+Ctrl+F { expand-column-to-available-width; }

        Mod+C { center-column; }

        // Finer width adjustments.
        // This command can also:
        // * set width in pixels: "1000"
        // * adjust width in pixels: "-5" or "+5"
        // * set width as a percentage of screen width: "25%"
        // * adjust width as a percentage of screen width: "-10%" or "+10%"
        // Pixel sizes use logical, or scaled, pixels. I.e. on an output with scale 2.0,
        // set-column-width "100" will make the column occupy 200 physical screen pixels.
        Mod+Minus { set-column-width "-10%"; }
        Mod+Equal { set-column-width "+10%"; }
        Mod+E { set-column-width "33.3%"; }
        Mod+R { set-column-width "50%"; }
        Mod+T { set-column-width "66.7%"; }

        // Finer height adjustments when in column with other windows.
        Mod+Shift+Minus { set-window-height "-10%"; }
        Mod+Shift+Equal { set-window-height "+10%"; }

        // Move the focused window between the floating and the tiling layout.
        Mod+V     { toggle-window-floating; }
        Mod+Shift+V { switch-focus-between-floating-and-tiling; }

        // Toggle tabbed column display mode.
        // Windows in this column will appear as vertical tabs,
        // rather than stacked on top of each other.
        Mod+W { toggle-column-tabbed-display; }

        Mod+Apostrophe { screenshot; }
        Mod+Ctrl+Apostrophe { screenshot-screen; }
        Mod+Shift+Apostrophe { screenshot-window; }

        // Applications such as remote-desktop clients and software KVM switches may
        // request that niri stops processing the keyboard shortcuts defined here
        // so they may, for example, forward the key presses as-is to a remote machine.
        // It's a good idea to bind an escape hatch to toggle the inhibitor,
        // so a buggy application can't hold your session hostage.
        //
        // The allow-inhibiting=false property can be applied to other binds as well,
        // which ensures niri always processes them, even when an inhibitor is active.
        Mod+Escape allow-inhibiting=false { toggle-keyboard-shortcuts-inhibit; }

        // The quit action will show a confirmation dialog to avoid accidental exits.
        Mod+Shift+E { quit; }
        Ctrl+Alt+Delete { quit; }

        // Powers off the monitors. To turn them back on, do any input like
        // moving the mouse or pressing any other key.
        Mod+Shift+P { power-off-monitors; }
      }
    '';
  };
}
