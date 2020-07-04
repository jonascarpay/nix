# Modular neovim config manager.
{ pkgs, config, lib, ... }:
let
  cfg = config.programs.neovim.init;

  githubPkg = repo: args:
    pkgs.vimUtils.buildVimPluginFrom2Nix {
      pname = builtins.replaceStrings [ "/" ] [ "-" ] repo;
      version = "1";
      src =
        builtins.fetchGit ({ url = "https://github.com/${repo}.git"; } // args);
    };

  modulesFiltered = builtins.filter (v: v.enable or true)
    (lib.attrValues (cfg.modules githubPkg));
  modulePlugins =
    builtins.concatLists (map (m: m.plugins or [ ]) modulesFiltered);
  moduleConfig = builtins.map (m: m.config or "") modulesFiltered;

in with lib; {
  options = {
    programs.neovim.init = {
      enable = mkEnableOption "neovim-init";
      preConfig = mkOption { };
      plugins = mkOption { };
      modules = mkOption { };
    };
  };
  config = mkIf cfg.enable {
    programs.neovim = {
      plugins = cfg.plugins ++ modulePlugins;
      extraConfig = concatStringsSep "\n" ([ cfg.preConfig ] ++ moduleConfig);
    };
  };
}
