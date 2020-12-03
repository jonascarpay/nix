{ pkgs, lib, ... }: {

  boot = {
    initrd.availableKernelModules = [ "usbhid" ];
    initrd.kernelModules = [ ];
    kernelModules = [ ];
    extraModulePackages = [ ];
    loader = {
      grub.enable = false;
      raspberryPi = {
        enable = true;
        version = 4;
      };
    };
    kernelPackages = pkgs.linuxPackages_rpi4;
    # binfmt.emulatedSystems = [
    #   "armv7l-linux"
    # ];
  };

  # nixpkgs.localSystem.system = "armv7l-linux";
  # nixpkgs.crossSystem.system = "armv7l-linux";
  nixpkgs.overlays = [
    (self: super: {
      llvm_9 = let

        cmakeFlags = [
          "-DCMAKE_BUILD_TYPE=Release"
          "-DLLVM_INSTALL_UTILS=ON"
          "-DLLVM_ENABLE_FFI=ON"
          "-DLLVM_ENABLE_RTTI=ON"
          "-DLLVM_HOST_TRIPLE=${super.stdenv.hostPlatform.config}"
          "-DLLVM_DEFAULT_TARGET_TRIPLE=${super.stdenv.hostPlatform.config}"
          "-DLLVM_ENABLE_DUMP=ON"
          "-DLLVM_LINK_LLVM_DYLIB=ON"
          "-DLLVM_BINUTILS_INCDIR=${super.libbfd.dev}/include"
        ] ++ lib.optionals (super.stdenv.hostPlatform != super.stdenv.buildPlatform) [
          "-DCMAKE_CROSSCOMPILING=True"
          "-DLLVM_TABLEGEN=${self.buildPackages.llvm_9}/bin/llvm-tblgen"
        ];
      in (super.llvm_9.override({ enablePFM = false; })).overrideAttrs (_: { inherit cmakeFlags; });
      llvm = super.llvm.override({ enablePFM = false; });
    })
  ];

  networking = {
    hostName = "onigiri";
    # networking.wireless.enable = true;
    networkmanager.enable = true;
    firewall.allowedTCPPorts = [ 8384 ];
    firewall.enable = true;
  };

  security.sudo.wheelNeedsPassword = false;
  programs.bash.enableCompletion = true;

  environment.systemPackages = with pkgs; [
    vim
    git
    fish
    tmux
    ranger
    htop # TODO move to globals
  ];


  time.timeZone = "Asia/Tokyo";
  powerManagement.cpuFreqGovernor = lib.mkDefault "ondemand";

  users.users.jmc = {
    isNormalUser = true;
    createHome = true;
    extraGroups = [ "wheel" "networkmanager" ];
  };

  services = {
    openssh.enable = true;
    syncthing = {
      enable = true;
      dataDir = "/mnt/exthd/syncthing";
      guiAddress = "0.0.0.0:8384";
    };

    # transmission = {
    #   enable = true;
    #   settings.download-dir = "/mnt/exthd/syncthing/Transmission";
    # };

    # jellyfin.enable = true;
  };

  system.stateVersion = "21.03"; # Did you read the comment?

  fileSystems = {
    "/" = {
        device = "/dev/disk/by-uuid/44444444-4444-4444-8888-888888888888";
        fsType = "ext4";
      };

    "/boot" = {
        device = "/dev/disk/by-uuid/2178-694E";
        fsType = "vfat";
      };

    "/mnt/exthd" = {
      device = "/dev/disk/by-uuid/2A76DE2B76DDF811";
      fsType = "ntfs";
      neededForBoot = false;
      noCheck = true;
    };
  };

  hardware.enableRedistributableFirmware = lib.mkDefault true;

}

# vim: fdm=indent:foldlevel=1:foldcolumn=2
