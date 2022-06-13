# Click events are broken until 3.5.0
{ pkgs, config, lib, ... }:
let
  sensors = "${pkgs.lm_sensors}/bin/sensors";
  grep = "${pkgs.gnugrep}/bin/grep";
  awk = "${pkgs.gawk}/bin/awk";
  sed = "${pkgs.gnused}/bin/sed";
in
lib.mkIf (config.xsession.enable) {

  programs.autorandr.hooks.postswitch.restart-polybar = ''
    systemctl restart --user polybar.service
  '';
  services.polybar = {
    enable = true;
    package = pkgs.polybar.override { alsaSupport = true; };
    script = ''
      polybar hidpi &
    '';
    config = {
      "bar/common" = {
        width = "100%";
        locale = "ja_JP.UTF-8";
        font-0 = "SauceCodePro Nerd Font:style=Regular:size=8;2";
        font-1 = "SauceCodePro Nerd Font:style=Bold:size=8;2";
        font-2 = "IPAPGothic:style=Bold:size=8;3";
        tray-position = "right";
        background = "#232831"; # darker than normal nord, slightly more muted
        foreground = "#d8dee9";
        module-margin = "1";
        modules-left = "xmonad";
        modules-right = "todos zfs onigiri vpn wireless wired fs memory temp fan cpu battery date-nl date";
        line-color = "#d8dee9";
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
        exec = ''[[ $(/run/current-system/sw/bin/nmcli con show --active) =~ "tun" ]] && echo "%{F#f00} %{F-}" || echo ""'';
        interval = "5";
      };

      "module/zfs" =
        let
          check = cmd: ''[[ ! $(${cmd}) =~ "no" ]] && echo "" || echo "%{F#f00}%{F-}"'';
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

      "module/xmonad" =
        let
          dbuswatch = pkgs.writeTextFile
            {
              name = "xmonad-log.sh";
              executable = true;
              # godverdomme 3 kkuur bezig geweest met uitvinden dat je -u nodig hebt voor sed echt chille zaterdag
              text = ''
                ${pkgs.dbus}/bin/dbus-monitor "path=/org/xmonad/Log,interface=org.xmonad.Log,member=Update" | \
                  ${sed} -nu 's/^   string "\([^:].*\)"$/\1/p'
              '';
            };
        in
        {
          type = "custom/script";
          exec = "${dbuswatch}";
          tail = "true";
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
