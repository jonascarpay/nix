let
  pkgsTarball = url: sha256:
    let src = builtins.fetchTarball { inherit url sha256; };
    in import src { };
  fromVersion = version: pkgsTarball "https://nixos.org/channels/nixos-${version}/nixexprs.tar.xz";
in
{
  # loris
  "19.09" = fromVersion "19.09" "c834750c0e9bc9f5ce5423f83331ea478c09347d42c7698116561ba5b204a4af";

  # markhor
  "20.03" = fromVersion "20.03" "c934750c0e9bc9f5ce5423f83331ea478c09347d42c7698116561ba5b204a4af";

  # nightingale
  "20.09" = fromVersion "20.09" "c835750c0e9bc9f5ce5423f83331ea478c09347d42c7698116561ba5b204a4af";

  # 2020-04-29 10:00 JST
  unstable = pkgsTarball
    "https://github.com/NixOS/nixpkgs/archive/ae96c292c0ead2fdabb6abc631cf3c869f4de3d6.tar.gz"
    "1z1p5whvbkxwgw16r8yhrgjfcsvdng85v5rvxg4y367c2658g6gk";

  nixos-hardware = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixos-hardware/archive/936e4649098d6a5e0762058cb7687be1b2d90550.tar.gz";
    sha256 = "06g0061xm48i5w7gz5sm5x5ps6cnipqv1m483f8i9mmhlz77hvlw";
  };

}
