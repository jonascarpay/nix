{ pkgs, inputs, ... }:
let

  python = {
    home.packages = [
      pkgs.pyright
      (pkgs.python3.withPackages (p: [
        p.polars
      ]))
    ];
    programs.ruff = {
      enable = true;
      settings = {
        line-length = 120;
        per-file-ignores = {
          "__init__.py" = [ "F401" ];
        };
        lint = {
          select = [ "ALL" ];
          ignore = [
            "T" # https://docs.astral.sh/ruff/rules/#flake8-print-t20
            "S603" # https://docs.astral.sh/ruff/rules/subprocess-without-shell-equals-true/
            "S607" # https://docs.astral.sh/ruff/rules/start-process-with-partial-path/
            "INP001" # https://docs.astral.sh/ruff/rules/implicit-namespace-package/
            "D" # https://docs.astral.sh/ruff/rules/#pydocstyle-d
          ];
        };
      };
    };
  };

  rust = {
    home.packages = [
      pkgs.cargo
      pkgs.rustc
      pkgs.rust-analyzer
      pkgs.rustfmt
      pkgs.clippy
      pkgs.clang
    ];
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
    python
    rust
  ];

  programs.bat.enable = true;

  manual.manpages.enable = true;

  xdg.enable = true;

  programs.powerline-go = {
    enable = true;
    modules = [ "venv" "host" "ssh" "cwd" "perms" "git" "exit" "nix-shell" "jobs" ];
    settings = {
      hostname-only-if-ssh = true;
      cwd-max-dir-depth = 4;
    };
    pathAliases = {
      "\\~/Dev" = "Dev";
      "/nix/store" = "nst";
    };
  };

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
