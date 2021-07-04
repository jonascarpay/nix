{ pkgs, config, lib, ... }:
with lib;
let
  ecfg = import ../../lib/editor-config.nix lib;
in
{
  options = {
    programs.emacs.init = {
      enable = mkEnableOption "Emacs config generator";
      modules = ecfg.option;
    };
  };
  config = mkIf (config.programs.emacs.enable && config.programs.emacs.init.enable)
    (
      let
        assembled = ecfg.assemble config.programs.emacs.init.modules;
      in
      {
        home.file.emacsInit = {
          target = ".emacs.d/init.el";
          text = assembled.config;
        };
        programs.emacs.extraPackages = assembled.packages;
      }
    );
}
