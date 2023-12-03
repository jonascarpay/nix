{ pkgs, config, lib, ... }:
{

  age.secrets.notifications-token.file = ../secrets/notifications-token.age;

  # TODO this is a hack to get a proper PATH.
  # This is so I can access things that aren't in pkgs, primarily my notes scripts.
  # It's ugly, but an inherent issue of having polybar being a systemd service, I think.
  # It's probably more robust to have i3 run polybar instead.
  systemd.user.services.polybar.Service.Environment =
    # Takes all of these string, appends "/bin", and concatenates using ":"
    let
      path = lib.strings.makeBinPath [
        "/home/jmc"
        "/run/wrappers"
        "/home/jmc/.nix-profile"
        "/etc/profiles/per-user/jmc"
        "/nix/var/nix/profiles/default"
        "/run/current-system/sw"
        "${pkgs.bash}"
        "${pkgs.coreutils}"
      ];
    in
    lib.mkForce "PATH=${path}";

  services.polybar = {
    enable = config.xsession.enable;
    package = pkgs.polybar.override {
      alsaSupport = true;
      i3Support = true;
      pulseSupport = true;
    };
    script = ''
      polybar mybar &
    '';
    settings =
      let

        colors = {
          foreground = "#d8dee9";
          background = "#232831"; # darker than normal nord, slightly more muted
          snow0 = "#d8dee9";
          red = "#BF616A";
          green = "#A3BE8C";
          orange = "#D08770";
        };

      in
      {
        "bar/common" = {
          inherit (colors) foreground background;
          width = "100%";
          locale = "ja_JP.UTF-8";
          font-0 = "SauceCodePro Nerd Font:style=Regular:size=8;2";
          font-1 = "SauceCodePro Nerd Font:style=Bold:size=8;2";
          font-2 = "Noto Sans JP:style=Bold:size=8;3";
          tray-position = "right";
          module-margin = "1";
          modules-left = "i3 xwindow";
          line-color = colors.snow0;
          line-size = "3";
        };

        "bar/hidpi" = {
          line-size = "6";
          dpi = "140";
          height = "30";
          tray-maxsize = "26";
        };

        "module/date" = {
          type = "internal/date";
          date = "%y/%m/%d %a";
          time = "%H:%M";
          label = "%date% %time% ";
          label-font = "2";
        };

        "module/date-jp" = {
          type = "custom/script";
          exec = ''TZ=Asia/Tokyo ${pkgs.coreutils}/bin/date +"󱉊 %H:%M"'';
          interval = "30";
        };

        "module/date-nl" = {
          type = "custom/script";
          exec = ''TZ=Europe/Amsterdam ${pkgs.coreutils}/bin/date +"󱉊 %H:%M"'';
          interval = "30";
        };

        "module/onigiri" =
          let
            singleping = pkgs.writeShellScript "singleping" ''
              #!/usr/bin/env bash

              # if output=$(ping -c 1 -W 1 192.168.1.6 | grep -oP ".*time=\K\d+"); then
              if output=$(ping -W 1 -q -c 5 192.168.1.6 | grep -oP " = \K\d"); then
                  echo "󰒍 %{F${colors.green}}$output ms%{F-}"
              else
                  echo "%{F${colors.red}}󰒎 %{F-}"
              fi
            '';
          in
          {
            type = "custom/script";
            exec = "${singleping}";
            interval = "30";
          };

        "module/notifications" =
          let
            script = pkgs.writeShellScript "notifications" ''
              set -euo pipefail
              AUTH="jonascarpay:$(cat ${config.age.secrets.notifications-token.path})"
              notifications=$(${pkgs.curl}/bin/curl -sf --user "$AUTH" https://api.github.com/notifications | ${pkgs.jq}/bin/jq length)
              if [ "$notifications" -gt 0 ]; then
                echo " $notifications"
              else
                echo ""
              fi
            '';
          in
          {
            type = "custom/script";
            exec = "${script}";
            interval = "60";
          };

        "module/vpn" = {
          type = "custom/script";
          exec = ''[[ $(ifconfig) =~ "tun" ]] && echo "%{F${colors.green}} %{F-}" || echo ""'';
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

        "module/todos" =
          let
            script = pkgs.writeShellScript "todos" ''
              count=$(todos-csv)
              echo anything $count
              # count=$(todos-csv | ${pkgs.coreutils}/bin/wc -l)
              # if [[ $count -gt 0 ]]; then
              #   echo " $count"
              # else
              #   echo "asdf"
              # fi
            '';
          in
          {
            type = "custom/script";
            exec = "${script}";
            # exec = "todos-csv";
            # exec = "/etc/profiles/per-user/jmc/bin/todos-csv";
            interval = builtins.toString 60;
            click-left = "emacs --eval '(org-todo-list)'";
          };

        "module/pulseaudio" = {
          type = "internal/pulseaudio";
          use-ui-max = false;
          format-volume = "<ramp-volume>";
          label-muted = "󰖁  ";
          ramp-volume = [ "󰕿  " "󰕿 ▁" "󰕿 ▂" "󰖀 ▃" "󰖀 ▄" "󰖀 ▅" "󰕾 ▆" "󰕾 ▇" "󰕾 █" ];
          click-right = "${pkgs.pavucontrol}/bin/pavucontrol &";
        };

        "module/fs" = {
          type = "internal/fs";
          mount-0 = "/";
          label-mounted = " %percentage_used%%";
        };

        "module/memory" = {
          type = "internal/memory";
          label = "󰘚%percentage_used:3%%";
          label-warn = "%{F${colors.orange}}%percentage_used:3%%{F-}";
          warn-percentage = "50";
        };

        "module/cpu" = {
          type = "internal/cpu";
          format = "<label> <ramp-coreload>";
          label = " ";

          ramp-coreload-spacing = 0;
          ramp-coreload = [ "▁" "▂" "▃" "▄" "▅" "▆" "▇" "█" ];
        };

        "module/cpu-temp" = {
          type = "internal/temperature";
          label = " %temperature-c%";
          label-warn = "%{F${colors.orange}} %temperature-c%%{F-}";
        };

        "module/gpu-temp" = {
          type = "internal/temperature";
          label = " %temperature-c%";
          label-warn = "%{F${colors.orange}} %temperature-c%%{F-}";
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
          label-connected = "󰈀 %local_ip% %downspeed:9%  %upspeed:9%  ";
          label-disconnected = "󰈂 ";
        };

        "module/wireless" = {
          type = "internal/network";
          label-connected = "󰖩 %downspeed:9%  %upspeed:9%  ";
          label-disconnected = "󰖪";
        };
      };
  };
}
