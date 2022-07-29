{
  description = "nixos";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.05";
    unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    home-manager = {
      url = "github:nix-community/home-manager/release-22.05";
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
    hosts = {
      url = "github:StevenBlack/hosts";
      flake = false;
    };
    picom = {
      url = "github:yshui/picom/next";
      flake = false;
    };
    frecently = {
      url = "github:jonascarpay/frecently/preserve-index";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    dmenu = {
      url = "github:jonascarpay/dmenu/magic-match";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    st = {
      url = "github:jonascarpay/st";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    blender = {
      url = "github:edolstra/nix-warez?dir=blender";
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
              useUserPackages = true;
              users.jmc.imports =
                [{
                  # this is necessary when not using useGlobalPkgs
                  # TODO can this go?
                  _module.args.pkgsPath = builtins.toPath (inputs.nixpkgs);
                  nixpkgs.config.allowUnfree = true;
                }]
                ++ homeModules;
              extraSpecialArgs = namedInputs system;
            };
          }

          inputs.agenix.nixosModules.age
          { environment.systemPackages = [ inputs.agenix.defaultPackage."${system}" ]; }

          inputs.declarative-cachix.nixosModules.declarative-cachix

        ] ++ sysModules;
      };

      shell =
        let
          pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux;
          hspkgs = pkgs.haskellPackages;
        in
        pkgs.mkShell {
          packages = [
            (hspkgs.ghcWithPackages (p: [
              p.dbus
              p.xmonad
              p.xmonad-contrib
            ]))
            hspkgs.ormolu
            hspkgs.hlint
            hspkgs.haskell-language-server
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
      nixosConfigurations = {
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
