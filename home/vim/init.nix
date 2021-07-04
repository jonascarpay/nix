{ pkgs, config, lib, ... }:
with lib;
let
  ecfg = import ../../lib/editor-config.nix lib;
in
{
  options = {
    programs.neovim.init = {
      enable = mkEnableOption "Vim config generator";
      modules = ecfg.option;
    };
  };
  config = mkIf (config.programs.neovim.enable && config.programs.neovim.init.enable)
    (
      let
        assembled = ecfg.assemble config.programs.neovim.init.modules;
      in
      {
        programs.neovim = {
          extraConfig = assembled.config;
          plugins = assembled.plugins pkgs.vimPlugins;
        };
        home.packages = assembled.packages;
      }
    );
}
