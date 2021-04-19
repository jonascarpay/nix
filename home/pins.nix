{ lib, ... }:
with lib;
{
  options.channels = mkOption {
    type = types.attrs;
    default = { };
    description = "Named channels";
  };
  config.channels =
    let
      fromTarball = url: sha:
        let src = builtins.fetchTarball {
          url = url;
          sha256 = sha;
        };
        in import src { };
      fromVersion = version: fromTarball "https://nixos.org/channels/nixos-${version}/nixexprs.tar.xz";
    in
    {

      loris = fromVersion
        "19.09"
        "c834750c0e9bc9f5ce5423f83331ea478c09347d42c7698116561ba5b204a4af";
      markhor = fromVersion
        "20.03"
        "c934750c0e9bc9f5ce5423f83331ea478c09347d42c7698116561ba5b204a4af";
      nightingale = fromVersion
        "20.09"
        "c835750c0e9bc9f5ce5423f83331ea478c09347d42c7698116561ba5b204a4af";

      unstable = fromTarball
        "https://github.com/NixOS/nixpkgs/archive/56ba373d1614d14e77d92a611433dc46692397df.tar.gz"
        "1gxw59pwcm47mkrb3f75mfra2g244gfz75m1zv4sxgicv2gblfnv";
    };
}
