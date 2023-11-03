{
  description = "nixos";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    home-manager = {
      url = "github:nix-community/home-manager/release-23.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-doom-emacs = {
      url = "github:nix-community/nix-doom-emacs";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    declarative-cachix.url = "github:jonascarpay/declarative-cachix";
    hosts = {
      url = "github:StevenBlack/hosts";
      flake = false;
    };
    frecently = {
      url = "github:jonascarpay/frecently/preserve-index";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    dmenu = {
      url = "github:jonascarpay/dmenu";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    st = {
      url = "github:jonascarpay/st";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    programs-db = {
      url = "github:binplz/programs.sqlite";
      flake = false;
    };
    # adaptive-lighting = {
    #   url = "github:basnijholt/adaptive-lighting";
    #   flake = false;
    # };
    nix-darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs:
    let

      namedInputs = system: {
        inherit inputs;
        unstable = import inputs.unstable {
          inherit system;
          config.allowUnfree = true;
        };
      };

      mkSystem = { system, sysModules, homeModules }: inputs.nixpkgs.lib.nixosSystem rec {
        inherit system;

        modules = [

          # TODO the old extraArgs has been deprecated, but this seems kinda hacky at best
          { config._module.args = namedInputs system; }

          inputs.home-manager.nixosModules.home-manager
          {
            home-manager = {
              users.jmc.imports = homeModules;
              extraSpecialArgs = namedInputs system;
              # Currently necessary when using HM as a flake
              # https://nix-community.github.io/home-manager/index.html#sec-flakes-nixos-module
              # https://github.com/divnix/digga/issues/30#issuecomment-748530996
              useGlobalPkgs = true;
              useUserPackages = true;
            };
          }
          inputs.agenix.nixosModules.age
          { environment.systemPackages = [ inputs.agenix.packages.${system}.agenix ]; }

          inputs.declarative-cachix.nixosModules.declarative-cachix

          { home-manager.users.jmc.imports = [ inputs.nix-doom-emacs.hmModule ]; }

        ] ++ sysModules;
      };

      shell =
        let
          pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux;
          hspkgs = pkgs.haskellPackages;
        in
        pkgs.mkShell {
          packages = [
            pkgs.bashInteractive
            pkgs.python3
            pkgs.pyright
            pkgs.black
            pkgs.python3Packages.ipython
          ];
        };

      hardware = inputs.nixos-hardware.nixosModules;
    in
    {
      devShell.x86_64-linux = shell;

      darwinConfigurations.bagel = inputs.nix-darwin.lib.darwinSystem {
        modules = [
          ./machines/bagel
          inputs.home-manager.darwinModules.home-manager
          { config._module.args = namedInputs "aarch64-darwin"; }
          { home-manager.extraSpecialArgs = namedInputs "aarch64-darwin"; }
        ];
      };
      darwinPackages = inputs.self.darwinConfigurations.bagel.pkgs; # TODO remove?

      nixosConfigurations = {
        stupidvm = mkSystem {
          system = "aarch64-linux";
          sysModules = [
            ./machines/stupidvm
          ];
          homeModules = [ ./home ./desktop ];
        };
        anpan = mkSystem {
          system = "x86_64-linux";
          sysModules = [
            hardware.common-pc-ssd
            hardware.common-pc
            ./machines/anpan.nix
          ];
          homeModules = [ ./home ./desktop ];
        };
        paninix = mkSystem {
          system = "x86_64-linux";
          sysModules = [
            hardware.lenovo-thinkpad-t480
            hardware.common-pc-laptop-ssd #  2022-06-29 Actually just links to common-pc-ssd
            ./machines/paninix.nix
          ];
          homeModules = [ ./home ./desktop ];
        };
        onigiri = mkSystem {
          system = "aarch64-linux";
          sysModules = [
            hardware.raspberry-pi-4
            ./machines/onigiri.nix
          ];
          homeModules = [ ./home ];
        };
      };
    };
}
