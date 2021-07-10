{
  description = "nixos";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    home-manager.url = "github:nix-community/home-manager/release-21.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    neuron.url = "github:srid/neuron";
    neuron.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixpkgs";
    declarative-cachix.url = "github:jonascarpay/declarative-cachix";
  };

  outputs = inputs:
    let

      mkSystem = { system, sysModules, homeModules }: inputs.nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [

          inputs.home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.jmc.imports = homeModules;
          }

          inputs.agenix.nixosModules.age
          { environment.systemPackages = [ inputs.agenix.defaultPackage."${system}" ]; }

          inputs.declarative-cachix.nixosModules.declarative-cachix
          { home-manager.users.jmc.imports = [ inputs.declarative-cachix.homeManagerModules.declarative-cachix-experimental ]; }

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

        ] ++ sysModules;
      };

    in
    {
      nixosConfigurations = {
        anpan = mkSystem {
          system = "x86_64-linux";
          sysModules = with inputs.nixos-hardware.nixosModules; [
            common-pc-ssd
            common-pc
            ./machines/anpan.nix
          ];
          homeModules = [ ./home ./desktop ];
        };
        onigiri = mkSystem {
          system = "aarch64-linux";
          sysModules = [
            inputs.nixos-hardware.nixosModules.raspberry-pi-4
            ./machines/onigiri.nix
          ];
          homeModules = [
            ./home
            ./home/nord-openvpn-configs.nix
          ];
        };
      };
    };
}
