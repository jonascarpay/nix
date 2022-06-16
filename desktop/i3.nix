{ pkgs, config, lib, ... }:
let
  concatAttrs = builtins.foldl' (a: b: a // b) { };
  concatMapAttrs = f: as: concatAttrs (builtins.map f as);
  m = config.xsession.windowManager.i3.config.modifier;
  escape = builtins.replaceStrings [ "\"" ] [ "\\\\\"" ];
  exec = str: "exec --no-startup-id \"${escape str}\"";
  exec' = str: "exec \"${escape str}\"";
  withMod = lib.mapAttrs' (key: value: { name = "${m}+${key}"; value = value; });
in
{
  xsession.windowManager.i3 = {
    enable = true;
    package = pkgs.i3-gaps;
    config = {
      terminal = "st";
      modifier = "Mod4";
      workspaceAutoBackAndForth = true;
      defaultWorkspace = "workspace number 1";

      gaps.inner = 20;
      gaps.outer = 0;

      startup = [{ command = "systemctl --user restart polybar"; always = true; notification = false; }];

      modes =
        let
          mkMode = keys: { Escape = "mode default"; Return = "mode default"; q = "mode default"; "[--release] ${m}" = "mode default"; } // keys // withMod keys;
          execExit = str: "mode default, ${exec str}";
        in
        {
          notes = mkMode {
            b = execExit "note-bookmarks";
            t = execExit "note-todos open -c floating";
            n = execExit "note-open -today -c floating";
            o = execExit "note-open -c floating";
          };
        };

      floating.criteria = [
        { class = "Pavucontrol"; }
        { class = "Qalculate-gtk"; }
        { class = "floating"; }
      ];

      bars = [ ];

      window.commands = [
        {
          criteria = { class = "floating"; };
          command = "resize set 1320 px 840 px, move position center";
        }
      ];
      keybindings =
        let
          generalKeys = withMod {
            "Escape" = "workspace back_and_forth";
            "grave" = "workspace back_and_forth";
            "Tab" = "workspace next";
            "shift+Tab" = "workspace prev";
            "Ctrl+Return" = exec "st fish";
            "Return" = exec "st -d \"`${pkgs.xcwd}/bin/xcwd`\" fish";
            "shift+Return" = exec "st -c floating -d \"`${pkgs.xcwd}/bin/xcwd`\" fish";
            "f" = exec' "firefox";
            "o" = exec "dmenu-run";
            "d" = exec "dmenu-directory";
            "r" = exec "dmenu-command -c floating";
            "shift+d" = exec "dmenu-directory -c floating";
            "n" = "mode notes";
            "s" = "split toggle";
            "shift+s" = "layout toggle split";
            "Ctrl+s" = "layout tabbed";
            "Ctrl+t" = "floating toggle";
            "shift+t" = "sticky toggle";
            "t" = "focus mode_toggle";
            "q" = "kill";
            "space" = "fullscreen toggle";
            "a" = "focus parent";
            "Shift+a" = "focus child";
            "shift+minus" = "move scratchpad";
            "minus" = "scratchpad show";
          };

          scratchpadKeys = concatMapAttrs perScratchpad scratchpads;
          perScratchpad = key:
            let
              mark = "\"scratchpad_${key}\"";
            in
            {
              "${m}+${key}" = "[con_mark = ${mark}] scratchpad show";
              "${m}+Shift+${key}" = "mark ${mark}, move scratchpad, [con_mark = ${mark}] scratchpad show";
            };
          scratchpads = [ "y" "u" "i" "o" "p" ];

          workspaceKeys = concatMapAttrs perWorkspace workspaces;
          perWorkspace = { key, ws }: {
            "${m}+${key}" = "workspace number ${ws}";
            "${m}+Shift+${key}" = "move container to workspace number ${ws}";
            "${m}+Ctrl+${key}" = "move container to workspace number ${ws}, workspace number ${ws}";
          };
          workspaces = [
            { key = "1"; ws = "1"; }
            { key = "2"; ws = "2"; }
            { key = "3"; ws = "3"; }
            { key = "4"; ws = "4"; }
            { key = "5"; ws = "5"; }
            { key = "6"; ws = "6"; }
            { key = "7"; ws = "7"; }
            { key = "8"; ws = "8"; }
            { key = "9"; ws = "9"; }
            { key = "0"; ws = "10"; }
          ];

          directionalKeys = concatMapAttrs perDirection directions;
          perDirection = { key, dir }: withMod {
            "${key}" = "focus ${dir}";
            "Shift+${key}" = "move ${dir} 240 px";
            "Ctrl+${key}" = "resize grow ${dir} 240 px or 10 ppt";
            "Ctrl+shift+${key}" = "resize shrink ${dir} 240 px or 10 ppt";
          };
          directions = [
            { key = "h"; dir = "left"; }
            { key = "j"; dir = "down"; }
            { key = "k"; dir = "up"; }
            { key = "l"; dir = "right"; }
            { key = "Left"; dir = "left"; }
            { key = "Down"; dir = "down"; }
            { key = "Up"; dir = "up"; }
            { key = "Right"; dir = "right"; }
          ];

        in
        workspaceKeys // directionalKeys // scratchpadKeys // generalKeys;
      focus.followMouse = false;
    };
  };
}
