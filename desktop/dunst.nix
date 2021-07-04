{
  services.dunst = {
    enable = true;
    settings = {
      colors.foreground = "#d8dee9";
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
      urgency_low.background = "#232831";
      urgency_normal.background = "#232831";
      urgency_critical.background = "#4c566a";
    };
  };
}
