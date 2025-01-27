let
  everforest = (import ../home/everforest.nix).default;
in
{
  services.dunst = {
    enable = true;
    settings = {
      colors.foreground = everforest.fg;
      global = {
        follow = "keyboard";
        geometry = "900-15+45"; # tweaked for bar size 30
        # shrink = true; # i want this but width calculation is broken? spotify notifications get line breaks for some reason
        padding = 8; # vertical
        horizontal_padding = 8;
        text_icon_padding = 16;
        markup = "full";
        font = "SauceCodePro Nerd Font 10";
        format = "<b>%s</b>\\n\\n%b";
        max_icon_size = 160;
        word_wrap = true;
      };
      urgency_low.background = everforest.bg0;
      urgency_normal.background = everforest.bg2;
      urgency_critical.background = everforest.bg_red;
    };
  };
}
