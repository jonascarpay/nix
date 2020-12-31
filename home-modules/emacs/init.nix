{ pkgs, config, lib, ... }:
with lib;
let
  cfg = config.programs.emacs.init;
  moduleType = types.submodule
    ({ name, config, ... }: {
      options = {
        enable = mkOption {
          default = true;
          description = "Whether to enable ${name}";
          type = types.bool;
          example = false;
        };
        packages = mkOption {
          default = [ name ];
          type = with types; listOf (either str package);
        };
        precedence = mkOption {
          type = types.int;
          default = 0;
        };
        config = mkOption {
          type = types.lines;
          default = "";
        };
      };
    });

  unlines = concatStringsSep "\n";

  enabledModules = filter (mod: mod.enable) (builtins.attrValues cfg.modules);

  assembledText =
    let
      byPrecedence = lists.groupBy (mod: toString mod.precedence) enabledModules;
      asText = mapAttrsToList
        (prec: mods: {
          prec = toInt prec;
          text = unlines
            ([ ";; init.nix precedence ${prec} ;;" "" ] ++ map (mod: mod.config) mods);
        })
        byPrecedence;
      sorted = builtins.sort (a: b: a.prec > b.prec) asText;
    in
    concatStringsSep "\n" (map (p: p.text) sorted);

  assembledPackages = p:
    let
      pkgDecls = builtins.concatMap (mod: mod.packages) enabledModules;
      toPkg = with builtins; pkg:
        if isString pkg then
          p."${pkg}"
        else
          pkg;
    in
    map toPkg pkgDecls;

in
{
  options = {
    programs.emacs.init = {
      enable = mkEnableOption "Emacs config generator";
      modules = mkOption {
        type = types.attrsOf moduleType;
        default = { };
      };
    };
  };
  config = mkIf (config.programs.emacs.enable && cfg.enable) {
    home.file.emacsInit = {
      target = ".emacs.d/init.el";
      text = assembledText;
    };
    programs.emacs.extraPackages = assembledPackages;
  };
}
