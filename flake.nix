{
  description = "nixos";

  # TODO follows
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/07759172ec997850eb73c937e2ecb12418cc426e";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    home-manager.url = "github:nix-community/home-manager/release-21.05";
    neuron.url = "github:srid/neuron";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    secrets.url = "git+ssh://git@github.com/jonascarpay/nix-secrets";
  };

  outputs = inputs:
    let
      system = "x86_64-linux";
      overlay = {
        flakes = inputs;
        unstable = import inputs.nixpkgs-unstable {
          inherit system;
          config.allowUnfree = true;
        };
      };
    in
    {
      nixosConfigurations = {
        anpan = inputs.nixpkgs.lib.nixosSystem {
          inherit system;
          modules = [
            ./machines/anpan.nix
            ({
              nixpkgs.overlays = [
                (_:_: overlay)
                inputs.emacs-overlay.overlay
              ];
            })
            inputs.home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.jmc = {
                imports = [
                  ./home
                  ./desktop
                ];
              };
            }
          ];
        };
      };
    };
}
