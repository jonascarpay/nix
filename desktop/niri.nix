{ pkgs, config, inputs, ... }:
let
  freqle = "${inputs.freqle.packages.${pkgs.system}.default}/bin/freqle";

  fzcmd-launch = pkgs.writeShellScript "command-launcher" ''
    HISTORY="$HOME/.local/share/freqle/command-history"
    DIR=$(${focused-dir})
    CMD=$(${freqle} view "$HISTORY" | fuzzel --dmenu --prompt "run: ")
    [ -n "$CMD" ] || exit 0
    ${freqle} bump "$HISTORY" -- "$CMD"

    SCRIPT='
      eval $argv[1]
      set _ex $status
      echo
      echo "[ exited $_ex ]"
    '
    export DIRENV_LOG_FORMAT=""
    if [ -d "$DIR" ]; then
      exec alacritty --working-directory "$DIR" --hold -e ${pkgs.direnv}/bin/direnv exec "$DIR" fish -c "$SCRIPT" "$CMD"
    else
      exec alacritty --hold -e fish -c "$SCRIPT" "$CMD"
    fi
  '';

  focused-dir = pkgs.lib.getExe (pkgs.writeShellApplication {
    name = "getFocusedDir";
    runtimeInputs = [ pkgs.procps pkgs.coreutils pkgs.gawk ];
    text = ''
      WINDOW_PID=$(niri msg focused-window | awk '/PID:/ {print $2}')
      SHELL_PID=$(ps --ppid "$WINDOW_PID" -o pid= | head -n1 | tr -d ' ')
      # foreground process group of the controlling terminal (the "active" program)
      FG_PGID=$(ps -o tpgid= -p "$SHELL_PID" | tr -d ' ')
      if [ "''${FG_PGID:--1}" -gt 0 ]; then TARGET=$FG_PGID; else TARGET=$SHELL_PID; fi
      readlink /proc/"$TARGET"/cwd
    '';
  });

  alacritty-fuzzel = pkgs.writeShellScript "alacritty-fuzzel" ''
    set -e
    DIR=$(fuzzel-directory)
    if jj --repository "$DIR" root >/dev/null 2>&1; then
      exec ${jjui-split} "$DIR"
    else
      exec alacritty --working-directory "$DIR"
    fi
  '';

  alacritty-focused = pkgs.writeShellScript "alacritty-focused" ''
    DIR=$(${focused-dir})
    if [ -d "$DIR" ]; then
      alacritty --working-directory "$DIR"
    else
      alacritty
    fi
  '';

  # Given a directory, open a terminal at 2/3 width on the left and a terminal
  # running jjui at 1/3 width on the right.
  jjui-split = pkgs.lib.getExe (pkgs.writeShellApplication {
    name = "jjui-split";
    runtimeInputs = [ pkgs.jq pkgs.alacritty pkgs.coreutils ];
    text = ''
      DIR="''${1:-$PWD}"

      focused_id() { niri msg --json focused-window | jq -r '.id // empty'; }

      # Spawn, then wait until a different window gains focus (the new one).
      wait_for_new() {
        local before="$1" cur
        for _ in $(seq 1 100); do
          cur=$(focused_id)
          if [ -n "$cur" ] && [ "$cur" != "$before" ]; then return 0; fi
          sleep 0.05
        done
      }

      before=$(focused_id)
      alacritty --working-directory "$DIR" &
      wait_for_new "$before"
      niri msg action set-column-width "66.7%"
      left=$(focused_id)

      alacritty --working-directory "$DIR" -e jjui &
      wait_for_new "$left"
      niri msg action set-column-width "33.3%"

      # Return focus to the main (left) terminal.
      niri msg action focus-column-left
    '';
  });

in

{

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

        "Mod+O".action.spawn = "fuzzel";
        "Mod+Shift+O".action.spawn = "${fzcmd-launch}";

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
        "Mod+Shift+E".action.set-column-width = "66.7%";
        "Mod+R".action.set-column-width = "50%";
        "Mod+T".action.set-column-width = "66.7%";
        "Mod+Shift+T".action.set-column-width = "33.3%";

        "Mod+V".action = actions.toggle-window-floating;
        "Mod+Shift+V".action = actions.switch-focus-between-floating-and-tiling;
        "Mod+W".action = actions.toggle-column-tabbed-display;

        # "Mod+Apostrophe".action = actions.screenshot;
        # "Mod+Ctrl+Apostrophe".action = actions.screenshot-screen;
        # "Mod+Shift+Apostrophe".action = actions.screenshot-window;
        "Mod+Apostrophe".action.screenshot = [ ]; # https://github.com/sodiboo/niri-flake/issues/1380#issuecomment-3420863847
        "Mod+Ctrl+Apostrophe".action.screenshot-screen = [ ];
        "Mod+Shift+Apostrophe".action.screenshot-window = [ ];

        "Mod+Escape" = { action = actions.toggle-keyboard-shortcuts-inhibit; allow-inhibiting = false; };
        # "Mod+Shift+E".action = actions.quit;
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

    layer-rules = [{
      matches = [{ namespace = "^awww-daemon$"; }];
      place-within-backdrop = true;
    }];
    layout.background-color = "transparent";
    overview.workspace-shadow.enable = false;

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
}
