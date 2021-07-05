{
  description = "nixos";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/07759172ec997850eb73c937e2ecb12418cc426e";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    home-manager.url = "github:nix-community/home-manager/release-21.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    neuron.url = "github:srid/neuron";
    neuron.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    # secrets.url = "git+ssh://git@github.com/jonascarpay/nix-secrets";
    secrets.url = "/home/jmc/nix-secrets";
  };

  outputs = inputs:
    let

      mkSystem = { system, sysModules, homeModules }: inputs.nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [

          inputs.home-manager.nixosModules.home-manager

          {
            nixpkgs.overlays = [
              (_:_: {
                flakes = inputs;
                unstable = import inputs.nixpkgs-unstable {
                  inherit system;
                  config.allowUnfree = true;
                };
              })
              inputs.emacs-overlay.overlay
            ];
          }

          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.jmc.imports = homeModules;
          }
        ] ++ sysModules;
      };

    in

    {
      nixosConfigurations = {
        anpan = mkSystem {
          system = "x86_64-linux";
          sysModules = [ ./machines/anpan.nix ];
          homeModules = [ ./home ./desktop ];
        };
        onigiri = mkSystem {
          system = "aarch64-linux";
          sysModules = [
            inputs.nixos-hardware.nixosModules.raspberry-pi-4
            ./machines/onigiri.nix
          ];
          homeModules = [ ./home ];
        };
      };
    };
}
