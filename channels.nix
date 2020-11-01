let
  mapAttrs = f: set:
    builtins.listToAttrs (map
      (attr: {
        name = attr;
        value = f set.${attr};
      })
      (builtins.attrNames set));

  channels = {
    aardvark = "13.10";
    baboon = "14.04";
    caterpillar = "14.12";
    dingo = "15.09";
    emu = "16.03";
    flounder = "16.09";
    gorilla = "17.03";
    hummingbird = "17.09";
    impala = "18.03";
    jellyfish = "18.09";
    koi = "19.03";
    loris = "19.09";
    markhor = "20.03";
    nightingale = "20.09";

    "13.10" = "13.10";
    "14.04" = "14.04";
    "14.12" = "14.12";
    "15.09" = "15.09";
    "16.03" = "16.03";
    "16.09" = "16.09";
    "17.03" = "17.03";
    "17.09" = "17.09";
    "18.03" = "18.03";
    "18.09" = "18.09";
    "19.03" = "19.03";
    "19.09" = "19.09";
    "20.03" = "20.03";
    "20.09" = "20.09";
  };

in
mapAttrs
  (v:
    import
      (builtins.fetchTarball "https://nixos.org/channels/nixos-${v}/nixexprs.tar.xz")
      { }
  )
  channels
