{ config, lib, ... }:
with lib;
let

  cfg = config.caches;

  nixosCache = {
    url = "https://cache.nixos.org";
    key = "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=";
  };

  cachixCaches = let

    getCacheDynamic = arg:
      if isString arg then getCache { name = arg; } else getCache arg;

    getCache = args:
      let
        content = builtins.fetchurl ({
          url = "https://cachix.org/api/v1/cache/${args.name}";
        } // optionalAttrs (args ? sha256) { inherit (args) sha256; });
        json = builtins.fromJSON (builtins.readFile content);
        url = json.uri;
        keys = json.publicSigningKeys;
      in { inherit url keys; };

  in listToAttrs (map (arg: {
    name = arg.name or arg;
    value = getCacheDynamic arg;
  }) cfg.cachix);

  substituters = concatStringsSep " " (map (v: v.url) (attrValues cfg.caches));
  publicKeys = concatStringsSep " "
    (concatMap (v: v.keys or [ v.key ]) (attrValues cfg.caches));
  nixConfSource = ''
    substituters = ${substituters}
    trusted-public-keys = ${publicKeys}
  '';

in {
  options.caches = {
    caches = mkOption {
      description = ''
        Caches to write to .config/nix/nix.conf.
        If this value is set, the values of caches.extraCaches and caches.cachix will be ignored.
        The names are ignored.

        Example value:
          
          {
            nixos = {
              url = "https://cache.nixos.org";
              key = "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=";
            };
          }

      '';
      type = types.attrs;
      default = cachixCaches // cfg.extraCaches // { nixos = nixosCache; };
    };

    extraCaches = mkOption {
      description = ''
        Caches to append to .config/nix/nix.conf.
        Same as caches.caches, but composes with caches.cachix and leaves the
        default nixos cache intact.
      '';
      type = types.attrs;
      default = { };
    };

    cachix = mkOption {
      description = ''
        Cachix caches to append to .config/nix/nix.conf.
        Accepts two configuration formats; either as a string, or an attribute
        set with a specified sha (recommended).

        Example value:

        [
          "someCachix"
          "someOtherCachix"
          { name = "someCachixWithSha"; sha256 = "..."; }
        ]
      '';
      default = [ ];
    };

  };

  config.home.file.nixConf = {
    target = ".config/nix/nix.conf";
    text = nixConfSource;
  };
}
