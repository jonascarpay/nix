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

  # 2020-06-04 08:00 JST
  unstable = pkgsTarball
    "https://github.com/NixOS/nixpkgs/archive/07759172ec997850eb73c937e2ecb12418cc426e.tar.gz"
    "028lpad46p6nwjwy8z4np1wypgn31gyrqch6f7qpm3v0wx7yzml5";

  nixos-hardware = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixos-hardware/archive/936e4649098d6a5e0762058cb7687be1b2d90550.tar.gz";
    sha256 = "06g0061xm48i5w7gz5sm5x5ps6cnipqv1m483f8i9mmhlz77hvlw";
  };

}
