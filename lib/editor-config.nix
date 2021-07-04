lib: with lib;
let

  moduleType = with types; submodule
    ({ name, config, ... }: {
      options = {
        enable = mkOption {
          default = true;
          description = "Whether to enable ${name}";
          type = bool;
          example = false;
        };
        plugins = mkOption {
          default = [ name ];
          type = listOf (either str package);
        };
        packages = mkOption {
          default = [ ];
          type = listOf package;
        };
        precedence = mkOption {
          type = int;
          default = 0;
        };
        config = mkOption {
          type = lines;
          default = "";
        };
      };
    });

  option = mkOption {
    type = types.attrsOf moduleType;
    default = { };
  };

  assemble = cfg:
    let
      sorted =
        builtins.sort
          (a: b: a.precedence > b.precedence)
          (filter
            (mod: mod.enable)
            (builtins.attrValues cfg));
    in
    {
      config = concatStringsSep "\n" (map (mod: mod.config) sorted);
      plugins = pkgs:
        map
          (pkg: if builtins.isString pkg then pkgs."${pkg}" else pkg)
          (builtins.concatMap (mod: mod.plugins) sorted);
      packages = builtins.concatMap (mod: mod.packages) sorted;
    };

in
{ inherit option assemble; }
