# Click events are broken until 3.5.0
{ pkgs, config, lib, ... }:
lib.mkIf (config.xsession.enable) {

  programs.autorandr.hooks.postswitch.restart-polybar = ''
    systemctl restart --user polybar.service
  '';
  services.polybar = {
    enable = true;
    package = pkgs.polybar.override { alsaSupport = true; };
    script = ''
      polybar t480-intel-edp1 &
      polybar t480-intel-hdmi2 &
      polybar t480-dgpu-dp-1-1 &
      polybar t480-dgpu-dp-1-2 &
      polybar t480-dgpu-edp-1-1 &
      polybar t480-dgpu-hdmi-1-2 &
    '';
    config = {
      "bar/common" = {
        width = "100%";
        locale = "ja_JP.UTF-8";
        font-0 = "SauceCodePro Nerd Font:style=Regular:size=8;2";
        font-1 = "SauceCodePro Nerd Font:style=Bold:size=8;2";
        font-2 = "IPAPGothic:style=Bold:size=8;3";
        tray-position = "right";
        background = "\${colors.background}";
        foreground = "\${colors.foreground}";
        module-margin = "1";
        modules-left = "xmonad";
        modules-right = "wireless wired fs memory cpu battery date-nl date";
        line-color = "\${colors.foreground}";
        line-size = "3";
      };

      "colors" = {
        background = "\${xrdb:color0:#222}";
        foreground = "\${xrdb:color7:#222}";
        foreground-alt = "\${xrdb:color7:#222}";
        primary = "\${xrdb:color1:#222}";
        secondary = "\${xrdb:color2:#222}";
        alert = "\${xrdb:color3:#222}";
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

      "bar/t480-intel-edp1" = {
        "inherit" = "bar/hidpi";
        monitor = "eDP1";
      };

      "bar/t480-intel-hdmi2" = {
        "inherit" = "bar/lodpi";
        monitor = "HDMI2";
      };

      "bar/t480-dgpu-dp-1-1" = {
        "inherit" = "bar/hidpi";
        monitor = "\${env:MONITOR:DP-1-1}";
      };

      "bar/t480-dgpu-dp-1-2" = {
        "inherit" = "bar/hidpi";
        monitor = "\${env:MONITOR:DP-1-2}";
      };

      "bar/t480-dgpu-edp-1-1" = {
        "inherit" = "bar/hidpi";
        monitor = "\${env:MONITOR:eDP-1-1}";
        tray-position = "none";
      };

      "bar/t480-dgpu-hdmi-1-2" = {
        "inherit" = "bar/hidpi";
        monitor = "\${env:MONITOR:HDMI-1-2}";
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

      "module/date-nl" = {
        type = "custom/script";
        exec = ''TZ=Europe/Amsterdam ${pkgs.coreutils}/bin/date +" %H:%M"'';
        interval = "30";
      };

      "module/pulseaudio" = { type = "internal/pulseaudio"; };

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
                  ${pkgs.gnused}/bin/sed -nu 's/^   string "\([^:].*\)"$/\1/p'
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
