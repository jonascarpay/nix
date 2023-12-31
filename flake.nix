{
  description = "nixos";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    home-manager = {
      url = "github:nix-community/home-manager/release-23.11";
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
    dmmono = {
      url = "github:jonascarpay/dm-mono-patched";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    st = {
      url = "github:jonascarpay/st";
      # url = "/home/jmc/Dev/st";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # replaced by nix-index?
    # programs-db = {
    #   url = "github:binplz/programs.sqlite";
    #   flake = false;
    # };
    adaptive-lighting = {
      url = "github:basnijholt/adaptive-lighting";
      flake = false;
    };
    nix-darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs:
    let

      mkNixosSystem = { system, module }: inputs.nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [ module ];
        specialArgs.unstable = import inputs.unstable { inherit system; };
        specialArgs.inputs = inputs;
      };

      mkDarwinSystem = { system, module }: inputs.nix-darwin.lib.darwinSystem {
        modules = [
          module
        ];
        specialArgs.unstable = import inputs.unstable { inherit system; };
        specialArgs.inputs = inputs;
      };

    in
    {
      homeManagerModules = {
        home = import ./home;
        desktop = import ./desktop;
        vim = import ./home/vim;
      };

      darwinConfigurations.bagel = mkDarwinSystem {
        system = "aarch64-darwin";
        module = ./machines/bagel;
      };

      nixosConfigurations.norf = mkNixosSystem {
        system = "x86_64-linux";
        module = ./machines/norf;
      };

      nixosConfigurations.dumpling = mkNixosSystem {
        system = "aarch64-linux";
        module = ./machines/dumpling;
      };

      nixosConfigurations.onigiri = mkNixosSystem {
        system = "aarch64-linux";
        module = ./machines/onigiri;
      };
    };
}
