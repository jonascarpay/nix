{
  description = "nixos";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.05";
    unstable.url = "github:NixOS/nixpkgs";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    home-manager = {
      url = "github:nix-community/home-manager/release-21.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    neuron = {
      url = "github:srid/neuron";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    declarative-cachix.url = "github:jonascarpay/declarative-cachix";
    nord-configs = {
      url = "github:jonascarpay/nord-openvpn-configs";
      flake = false;
    };
  };

  outputs = inputs:
    let

      mkSystem = { system, sysModules, homeModules }: inputs.nixpkgs.lib.nixosSystem rec {
        inherit system;

        extraArgs = {
          inherit inputs;
          unstable = import inputs.unstable {
            inherit system;
            config.allowUnfree = true;
          };
        };

        modules = [

          inputs.home-manager.nixosModules.home-manager
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              users.jmc.imports = homeModules;
              extraSpecialArgs = extraArgs;
            };
          }

          inputs.agenix.nixosModules.age
          { environment.systemPackages = [ inputs.agenix.defaultPackage."${system}" ]; }

          inputs.declarative-cachix.nixosModules.declarative-cachix
          { home-manager.users.jmc.imports = [ inputs.declarative-cachix.homeManagerModules.declarative-cachix-experimental ]; }

          {
            nixpkgs.overlays = [
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
        paninix = mkSystem {
          system = "x86_64-linux";
          sysModules = with inputs.nixos-hardware.nixosModules; [
            lenovo-thinkpad-t480
            common-pc-laptop
            common-pc-laptop-ssd
            # common-gpu-nvidia
            ./machines/paninix.nix
          ];
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
