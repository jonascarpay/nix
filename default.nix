let
  inherit (import <nixpkgs> { }) nixos linkFarmFromDrvs;

  paninix = (nixos (import machines/paninix.nix)).toplevel;

  onigiri =
    let
      pkgs = import <nixpkgs> { crossSystem.system = "aarch64-linux"; };
      onigiriConfig = args: import machines/onigiri.nix (args // { inherit pkgs; });
    in
    (pkgs.nixos onigiriConfig).toplevel;
in
{
  inherit paninix onigiri;
  all-machines = linkFarmFromDrvs "all-machines" [ paninix onigiri ];
}
