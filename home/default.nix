{ pkgs, inputs, ... }:
let

  bat = {
    programs.bat.enable = true;
    programs.fish.shellAliases.cat = "bat";
  };

in
{
  imports = [
    ./direnv.nix
    ./fish.nix
    ./tmux.nix
    ./vim
    ./git.nix
    ./postgres.nix
    ./daily.nix
    inputs.agenix.homeManagerModules.age
    bat
  ];

  manual.manpages.enable = true;

  xdg.enable = true;

  home = {
    stateVersion = "23.05";
    packages = [
      pkgs.silver-searcher # TODO do I use this? yes for fzf fg
      pkgs.visidata
      pkgs.cloc
      pkgs.entr
      pkgs.jq
      pkgs.tldr
      pkgs.ripgrep
      pkgs.zip
    ];

    sessionVariables = {
      EDITOR = "nvim";
      PAGER = "less";
    };

  };

  programs.fzf.enable = true;

  programs.eza = {
    enable = true;
    git = true;
    icons = "auto";
  };

  programs.fd = {
    enable = true;
    ignores = [ ".git/*" ];
  };

  programs.ssh.enable = true;

  programs.yazi = {
    enable = true;
    enableBashIntegration = true;
    enableFishIntegration = true;
  };
}
