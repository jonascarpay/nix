# Click events are broken until 3.5.0
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
lib.mkIf (config.xsession.enable) {

  programs.autorandr.hooks.postswitch.restart-polybar = ''
    systemctl restart --user polybar.service
  '';
  services.polybar = {
    enable = true;
    package = pkgs.polybar.override {
      alsaSupport = true;
      i3GapsSupport = true;
      pulseSupport = true;
    };
    script = ''
      polybar hidpi &
    '';
    config = {
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
        modules-right = "todos zfs onigiri vpn wireless wired fs memory temp fan cpu battery pulseaudio date-nl date";
        line-color = colors.snow0;
        line-size = "3";
      };

      "bar/hidpi" = {
        "inherit" = "bar/common";
        line-size = "6";
        dpi = "140";
        height = "30";
        tray-maxsize = "26";
      };

      "bar/lodpi" = {
        "inherit" = "bar/common";
        height = "18";
        tray-maxsize = "15";
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
        exec = ''TZ=Asia/Tokyo ${pkgs.coreutils}/bin/date +"JST: %H:%M"'';
        interval = "30";
      };

      "module/fan" = {
        type = "custom/script";
        exec = "${sensors} | ${grep} fan1 | ${awk} '{print $2; exit}'";
        label = " %output% RPM";
        interval = "1";
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

      "module/date-nl" = {
        type = "custom/script";
        exec = ''TZ=Europe/Amsterdam ${pkgs.coreutils}/bin/date +" %H:%M"'';
        interval = "30";
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
        };

      "module/pulseaudio" = {
        type = "internal/pulseaudio";
        use-ui-max = false;
        format-volume = "<ramp-volume> <label-volume>";
        format-muted = "ﱝ   0%";
        label-volume = "%percentage:3%%";
        ramp-volume-0 = "奄";
        ramp-volume-1 = "奔";
        ramp-volume-2 = "墳";
        click-right = "${pkgs.pavucontrol}/bin/pavucontrol";
      };

      "module/battery" = {
        type = "internal/battery";
        label-charging = "🗲 %percentage%";
        full-at = "95";
        battery = "BAT1";
        adapter = "AC";
        format-charging = "<ramp-capacity>";
        format-full = "<ramp-capacity>";
        format-discharging = "<ramp-capacity>";
        ramp-capacity-0 = "";
        ramp-capacity-1 = "";
        ramp-capacity-2 = "";
        ramp-capacity-3 = "";
        ramp-capacity-4 = "";
        ramp-capacity-5 = "";
        ramp-capacity-6 = "";
        ramp-capacity-7 = "";
        ramp-capacity-8 = "";
        ramp-capacity-9 = "";
        ramp-capacity-10 = "";
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
        ramp-coreload-0 = "▁";
        ramp-coreload-1 = "▂";
        ramp-coreload-2 = "▃";
        ramp-coreload-3 = "▄";
        ramp-coreload-4 = "▅";
        ramp-coreload-5 = "▆";
        ramp-coreload-6 = "▇";
        ramp-coreload-7 = "█";
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
        format = "- %{T2}<label>%{T-}";
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
