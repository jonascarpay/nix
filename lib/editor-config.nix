lib: with lib;
let

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
      packages = pkgs:
        map
          (pkg: if builtins.isString pkg then pkgs."${pkg}" else pkg)
          (builtins.concatMap (mod: mod.packages) sorted);
    };

in
{ inherit option assemble; }