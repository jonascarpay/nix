let mapAttrs = f: set: builtins.listToAttrs (
      map (attr: { name = attr; value = f set.${attr}; })
    (builtins.attrNames set));
    channels = {
      aardvark    = "13.10";
      baboon      = "14.04";
      caterpillar = "14.12";
      dingo       = "15.09";
      emu         = "16.03";
      flounder    = "16.09";
      gorilla     = "17.03";
      hummingbird = "17.09";
      impala      = "18.03";
      jellyfish   = "18.09";
      koi         = "19.03";
      loris       = "19.09";
    };
in mapAttrs (v:
     import (builtins.fetchTarball
       "https://nixos.org/channels/nixos-${v}/nixexprs.tar.xz") {})
   channels
