{ pkgs, ... }:
{
  programs.niri.settings = {
    binds."Mod+Slash".action.spawn = "${pkgs.qalculate-gtk}/bin/qalculate-gtk";
    window-rules = [{
      matches = [{ app-id = "qalculate-gtk"; }];
      open-floating = true;
    }];
  };
}
