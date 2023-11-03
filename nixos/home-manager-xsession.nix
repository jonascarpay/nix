{ pkgs, ... }:
{
  home-manager.users.jmc.xsession = {
    enable = true;
    scriptPath = ".hm-xsession";
  };
  services.xserver.displayManager = {
    autoLogin = {
      enable = true;
      user = "jmc";
    };
    # https://discourse.nixos.org/t/opening-i3-from-home-manager-automatically/4849/8
    session = [{
      manage = "desktop";
      name = "home-manager";
      start = ''
        ${pkgs.runtimeShell} $HOME/.hm-xsession &
        waitPID=$!
      '';
    }];
    defaultSession = "home-manager";
  };
}
