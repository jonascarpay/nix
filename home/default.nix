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

  programs = {
    man.generateCaches = true;
    fzf.enable = true;

    eza = {
      enable = true;
      git = true;
      icons = "auto";
    };

    git = {
      enable = true;
      userName = "Jonas Carpay";
      userEmail = "jonascarpay@gmail.com";
      ignores = [
        "result"
        "result-*"
        # The reason we put synchting here, and not in a, say, desktop
        # syncthing module is that onigiri needs these during syncing even though syncthing is
        # a system module there.
        ".stversions"
        "*.sync-confict-*"
        ".stignore"
        ".stfolder"
        ".lvimrc"
      ];
      extraConfig = {
        commit.verbose = true;
        pull.rebase = true;
        init.defaultBranch = "master";
      };
    };
    ssh = {
      enable = true;
    };
  };
}
