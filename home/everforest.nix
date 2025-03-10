# bg_dim:      Dimmed background
# bg0:         Default background
# bg1:         Cursor line background
# bg2:         Popup menu background
# bg3:         List characters, special keys
# bg4:         Window splits, whitespaces
# bg5:         Not currently used
# bg_visual:   Visual selection background
# bg_red:      Diff deleted line, error highlights
# bg_green:    Diff added line, hint highlights
# bg_blue:     Diff changed line, info highlights
# bg_yellow:   Warning highlights
# fg:          Default foreground
# red:         Error messages and critical keywords
# orange:      Labels, operators, storage classes
# yellow:      Types, special characters, warnings
# green:       Functions, strings, hint messages
# aqua:        Constants, macros
# blue:        Identifiers, info messages
# purple:      Numbers, booleans, preprocessor directives
# grey0:       Line numbers, UI elements
# grey1:       Comments, punctuation
# grey2:       Cursor line number
# statusline1: Statusline and menu highlights
# statusline2: Inactive statusline
# statusline3: Active statusline

rec {
  default = dark.foreground // dark.background.medium;
  dark = {
    foreground = {
      fg = "#D3C6AA";
      red = "#E67E80";
      orange = "#E69875";
      yellow = "#DBBC7F";
      green = "#A7C080";
      aqua = "#83C092";
      blue = "#7FBBB3";
      purple = "#D699B6";
      grey0 = "#7A8478";
      grey1 = "#859289";
      grey2 = "#9DA9A0";
      statusline1 = "#A7C080";
      statusline2 = "#D3C6AA";
      statusline3 = "#E67E80";
    };
    background = {
      hard = {
        bg_dim = "#1E2326";
        bg0 = "#272E33";
        bg1 = "#2E383C";
        bg2 = "#374145";
        bg3 = "#414B50";
        bg4 = "#495156";
        bg5 = "#4F5B58";
        bg_visual = "#4C3743";
        bg_red = "#493B40";
        bg_green = "#3C4841";
        bg_blue = "#384B55";
        bg_yellow = "#45443C";
      };
      medium = {
        bg_dim = "#232A2E";
        bg0 = "#2D353B";
        bg1 = "#343F44";
        bg2 = "#3D484D";
        bg3 = "#475258";
        bg4 = "#4F585E";
        bg5 = "#56635F";
        bg_visual = "#543A48";
        bg_red = "#514045";
        bg_green = "#425047";
        bg_blue = "#3A515D";
        bg_yellow = "#4D4C43";
      };
      soft = {
        bg_dim = "#293136";
        bg0 = "#333C43";
        bg1 = "#3A464C";
        bg2 = "#434F55";
        bg3 = "#4D5960";
        bg4 = "#555F66";
        bg5 = "#5D6B66";
        bg_visual = "#5C3F4F";
        bg_red = "#59464C";
        bg_green = "#48584E";
        bg_blue = "#3F5865";
        bg_yellow = "#55544A";
      };
    };
  };
  light = {
    foreground = {
      fg = "#5C6A72";
      red = "#F85552";
      orange = "#F57D26";
      yellow = "#DFA000";
      green = "#8DA101";
      aqua = "#35A77C";
      blue = "#3A94C5";
      purple = "#DF69BA";
      grey0 = "#A6B0A0";
      grey1 = "#939F91";
      grey2 = "#829181";
      statusline1 = "#93B259";
      statusline2 = "#708089";
      statusline3 = "#E66868";
    };
    background = {
      hard = {
        bg_dim = "#F2EFDF";
        bg0 = "#FFFBEF";
        bg1 = "#f8f5e4";
        bg2 = "#F2EFDF";
        bg3 = "#EDEADA";
        bg4 = "#E8E5D5";
        bg5 = "#BEC5B2";
        bg_visual = "#F0F2D4";
        bg_red = "#FFE7DE";
        bg_green = "#F3F5D9";
        bg_blue = "#ECF5ED";
        bg_yellow = "#FEF2D5";
      };
      medium = {
        bg_dim = "#EFEBD4";
        bg0 = "#FDF6E3";
        bg1 = "#F4F0D9";
        bg2 = "#EFEBD4";
        bg3 = "#E6E2CC";
        bg4 = "#E0DCC7";
        bg5 = "#BDC3AF";
        bg_visual = "#EAEDC8";
        bg_red = "#FBE3DA";
        bg_green = "#F0F1D2";
        bg_blue = "#E9F0E9";
        bg_yellow = "#FAEDCD";
      };
      soft = {
        bg_dim = "#E5DFC5";
        bg0 = "#F3EAD3";
        bg1 = "#EAE4CA";
        bg2 = "#E5DFC5";
        bg3 = "#DDD8BE";
        bg4 = "#D8D3BA";
        bg5 = "#B9C0AB";
        bg_visual = "#E1E4BD";
        bg_red = "#F4DBD0";
        bg_green = "#E5E6C5";
        bg_blue = "#E1E7DD";
        bg_yellow = "#F1E4C5";
      };
    };
  };
}
