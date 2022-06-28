{ pkgs, config, lib, ... }:
let
  sensors = "${pkgs.lm_sensors}/bin/sensors";
  grep = "${pkgs.gnugrep}/bin/grep";
  awk = "${pkgs.gawk}/bin/awk";
  sed = "${pkgs.gnused}/bin/sed";
  colors = {
    foreground = "#d8dee9";
    background = "#232831"; # darker than normal nord, slightly more muted
    snow0 = "#d8dee9";
    red = "#BF616A";
  };
in
{
  # TODO this is a hack to get a proper PATH.
  # This is necessary to get scripts like note-todos to properly spawn dmenu/st.
  # it's probably more robust to just have i3 start polybar instead, so it's always using the same PATH
  # Most is still written in a style that doesn't assume a PATH, which could then just be ported
  systemd.user.services.polybar.Service.Environment = lib.mkForce "PATH=/home/jmc/bin:/run/wrappers/bin:/home/jmc/.nix-profile/bin:/etc/profiles/per-user/jmc/bin:/nix/var/nix/profiles/default/bin:/run/current-system/sw/bin";
  services.polybar = {
    enable = config.xsession.enable;
    package = pkgs.polybar.override {
      alsaSupport = true;
      i3GapsSupport = true;
      pulseSupport = true;
    };
    script = "polybar $(${pkgs.nettools}/bin/hostname) &";
    settings = {
      "bar/xc-jonas" = {
        "inherit" = "bar/hidpi";
        modules-right = "todos vpn wireless wired fs memory temp fan cpu battery backlight-t480 pulseaudio date-jpn date";
      };

      "bar/anpan" = {
        "inherit" = "bar/hidpi";
        modules-right = "todos zfs onigiri vpn wireless wired fs memory temp fan cpu pulseaudio date-nl date";
      };

      "bar/paninix" = {
        "inherit" = "bar/common";
        modules-right = "vpn wireless wired fs memory temp fan cpu battery pulseaudio date-jpn date";
        height = "18";
        tray-maxsize = "15";
      };

      "bar/hidpi" = {
        "inherit" = "bar/common";
        line-size = "6";
        dpi = "140";
        height = "30";
        tray-maxsize = "26";
      };

      "bar/common" = {
        inherit (colors) foreground background;
        width = "100%";
        locale = "ja_JP.UTF-8";
        font-0 = "SauceCodePro Nerd Font:style=Regular:size=8;2";
        font-1 = "SauceCodePro Nerd Font:style=Bold:size=8;2";
        font-2 = "IPAPGothic:style=Bold:size=8;3";
        tray-position = "right";
        module-margin = "1";
        modules-left = "i3 xwindow";
        line-color = colors.snow0;
        line-size = "3";
      };

      "module/date" = {
        type = "internal/date";
        date = "%y/%m/%d %a";
        time = "%H:%M";
        label = " %date%  %time% ";
        label-font = "2";
      };

      "module/date-jpn" = {
        type = "custom/script";
        exec = ''TZ=Asia/Tokyo ${pkgs.coreutils}/bin/date +" %H:%M"'';
        interval = "30";
      };

      "module/date-nl" = {
        type = "custom/script";
        exec = ''TZ=Europe/Amsterdam ${pkgs.coreutils}/bin/date +" %H:%M"'';
        interval = "30";
      };

      "module/fan" = {
        type = "custom/script";
        exec = "${sensors} | ${grep} fan1 | ${awk} '{print $2; exit}'";
        label = " %output% RPM";
        interval = "1";
      };

      "module/backlight-t480" = {
        type = "internal/backlight";
        card = "intel_backlight";
        format = "<ramp>";
        ramp = [ " " " " " " " " " " " " " " " " " " " " " " " " " " " " ];
        enable-scroll = true;
      };

      "module/onigiri" =
        let
          singleping = pkgs.writeShellScript "singleping" ''
            #!/usr/bin/env bash

            # if output=$(ping -c 1 -W 1 192.168.1.6 | ${grep} -oP ".*time=\K\d+"); then
            if output=$(ping -W 1 -q -c 5 192.168.1.6 | ${grep} -oP " = \K\d"); then
                echo "歷  $output ms"
            else
                echo "轢 "
            fi
          '';
        in
        {
          type = "custom/script";
          exec = "${singleping}";
          interval = "30";
        };

      "module/vpn" = {
        type = "custom/script";
        exec = ''[[ $(/run/current-system/sw/bin/nmcli con show --active) =~ "tun" ]] && echo "%{F${colors.red}} %{F-}" || echo ""'';
        interval = "5";
      };

      "module/zfs" =
        let
          check = cmd: ''[[ ! $(${cmd}) =~ "no" ]] && echo "" || echo "%{F${colors.red}}%{F-}"'';
          zfs = "/run/current-system/sw/bin/zfs get mounted -t filesystem";
          ssh = cmd: "/run/current-system/sw/bin/ssh 192.168.1.6 '${cmd}'";
        in
        {
          type = "custom/script";
          exec = ''echo " $(${check zfs}) $(${check (ssh zfs)})"'';
          interval = builtins.toString 60;
        };

      "module/temp" = {
        type = "custom/script";
        exec = "${sensors} | ${grep} Package | ${awk} '{print $4; exit}' | ${sed} 's/^.\\([0-9]\\+\\)../\\1/' ";
        label = " %output%";
        interval = "1";
      };

      "module/todos" =
        let
          script = pkgs.writeShellScript "todos" ''
            set -e
            count=$(${pkgs.note-todos}/bin/note-todos count)
            if [[ $count -gt 0 ]]; then
              echo " $count"
            else
              echo ""
            fi
          '';
        in
        {
          type = "custom/script";
          exec = "${script}";
          interval = builtins.toString 60;
          click-left = "${pkgs.note-todos}/bin/note-todos open -c floating";
        };

      "module/pulseaudio" = {
        type = "internal/pulseaudio";
        use-ui-max = false;
        format-volume = "<ramp-volume>";
        label-muted = "婢  ";
        ramp-volume = [ "奄  " "奄 ▁" "奄 ▂" "奔 ▃" "奔 ▄" "奔 ▅" "墳 ▆" "墳 ▇" "墳 █" ];
        click-right = "${pkgs.pavucontrol}/bin/pavucontrol &";
      };

      "module/battery" = {
        type = "internal/battery";
        full-at = "95";
        battery = "BAT1";
        adapter = "AC";
        time-format = "%H:%M";
        format-full = "";
        format-charging = "<ramp-capacity> <label-charging>";
        label-charging = "%time%";
        format-discharging = "<ramp-capacity> <label-discharging>";
        label-discharging = "%time%";
        ramp-capacity = [ "" "" "" "" "" "" "" "" "" "" "" ];
        click-left = "st -c floating sudo powertop";
      };

      "module/fs" = {
        type = "internal/fs";
        mount-0 = "/";
        label-mounted = " %percentage_used%%";
      };

      "module/memory" = {
        type = "internal/memory";
        label = " %percentage_used:4%%";
      };

      "module/cpu" = {
        type = "internal/cpu";
        # format = "<label> <ramp-coreload> ";
        format = "<label><ramp-coreload>";
        # format = "<ramp-coreload> ";
        label = "  ";
        # click-left = "firefox";

        ramp-coreload-spacing = 0;
        ramp-coreload = [ "▁" "▂" "▃" "▄" "▅" "▆" "▇" "█" ];
      };

      "module/i3" = {
        type = "internal/i3";
        pin-workspaces = true;
        show-urgent = true;
        wrapping-scroll = false;
        # The T1 sets the regular font, used just to escape the leading space.
        # The docs say you can surround with quotes but this doesn't seem to work
        format = "%{T1} <label-state> <label-mode>%{T-}";

        label-focused = "%index%";
        label-focused-foreground = colors.background;
        label-focused-background = colors.foreground;
        label-focused-padding = 1;

        label-mode-foreground = colors.red;
        label-mode-padding = 1;

        label-unfocused = "%index%";
        label-unfocused-padding = 1;

        label-visible = "%index%";
        label-visible-padding = 1;

        label-urgent = "%index%";
        label-urgent-foreground = colors.background;
        label-urgent-background = colors.red;
        label-urgent-padding = 1;
      };

      "module/xwindow" = {
        type = "internal/xwindow";
        label-maxlen = 80;
        label = "- %{T2}%title%%{T-}";
      };

      "module/wired" = {
        type = "internal/network";
        interface = "enp0s31f6";
        label-connected = " %local_ip% %downspeed:9%  %upspeed:9%  ";
        label-disconnected = "";
      };

      "module/wireless" = {
        type = "internal/network";
        interface = "wlp3s0";
        label-connected = "直 %downspeed:9%  %upspeed:9%  ";
        label-disconnected = "睊";
      };
    };
  };
}
