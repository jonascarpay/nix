let
  fromTarball = url: sha256:
    let src = builtins.fetchTarball { inherit url sha256; };
    in import src { };
  fromVersion = version: fromTarball "https://nixos.org/channels/nixos-${version}/nixexprs.tar.xz";
in
{
  # loris
  "19.09" = fromVersion "19.09" "c834750c0e9bc9f5ce5423f83331ea478c09347d42c7698116561ba5b204a4af";

  # markhor
  "20.03" = fromVersion "20.03" "c934750c0e9bc9f5ce5423f83331ea478c09347d42c7698116561ba5b204a4af";

  # nightingale
  "20.09" = fromVersion "20.09" "c835750c0e9bc9f5ce5423f83331ea478c09347d42c7698116561ba5b204a4af";

  # 2020-04-19T22:00
  unstable = fromTarball
    "https://github.com/NixOS/nixpkgs/archive/56ba373d1614d14e77d92a611433dc46692397df.tar.gz"
    "1gxw59pwcm47mkrb3f75mfra2g244gfz75m1zv4sxgicv2gblfnv";

}
