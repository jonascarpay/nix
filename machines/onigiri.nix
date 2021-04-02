{ pkgs, lib, ... }: {

  imports = [
    ../system/openvpn.nix
    ../secrets/accounts.nix
  ];

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
        firmwareConfig = "gpu_mem=320\ndtoverlay=vc4-kms-v3d-pi4";
      };
    };
    kernelPackages = pkgs.linuxPackages_rpi4;
  };

  networking = {
    hostName = "onigiri";
    # networking.wireless.enable = true;
    networkmanager.enable = true;
    firewall = {
      enable = true;
      allowedTCPPorts = [
        8384 # syncthing GUI
        8096 # jellyfin HTTP
        8920 # jellyfin HTTPS
        9091 # transmission
        22000 # syncthing
        8090 # rclone webdav # TODO move to module
      ];
      allowedUDPPorts = [
        1900 # jellyfin
        7359 # jellyfin
        21027 # syncthing
      ];
    };
  };

  security.sudo.wheelNeedsPassword = false;
  programs.bash.enableCompletion = true;

  environment.systemPackages = with pkgs; [
    vim
    git
    fish
    tmux
    ranger
    libraspberrypi
    htop # TODO move to globals # TODO import globals?
  ];


  time.timeZone = "Asia/Tokyo";
  powerManagement.cpuFreqGovernor = lib.mkDefault "ondemand";

  users.users.jmc = {
    isNormalUser = true;
    createHome = true;
    extraGroups = [ "wheel" "networkmanager" ];
    shell = pkgs.fish;
  };

  services = {
    openssh = {
      enable = true;
      passwordAuthentication = false;
    };
    syncthing = {
      enable = true;
      dataDir = "/mnt/exthd";
      guiAddress = "0.0.0.0:8384";
    };

    transmission = {
      enable = false;
      settings = {
        download-dir = "/mnt/exthd/Transmission";
        incomplete-dir = "/mnt/exthd/Transmission";
        rpc-bind-address = "0.0.0.0";
        rpc-port = 9091;
        rpc-whitelist = "192.168.1.3";
      };
    };

    jellyfin = {
      # enable = true;
      # group = "jellyfin,video";
    };
    # xserver = {
    #   enable = true;
    #   videoDrivers = [ "modesetting" ];
    #   displayManager.lightdm.enable = true;
    # };
  };

  services.journald.extraConfig = ''
    SystemMaxUse=16M
  '';

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
  systemd.services = {
    rclone = {
      enable = true;
      script = ''
        ${pkgs.rclone}/bin/rclone serve webdav /mnt/exthd/ --addr :8090 --user dav --pass dav
      '';
      wants = [ "network-online.target" ];
      after = [ "network-online.target" ];
      wantedBy = [ "multi-user.target" ];
    };
  };

  hardware = {
    enableRedistributableFirmware = true;
    # deviceTree = {
    #   kernelPackage = pkgs.device-tree_rpi;
    #   overlays = [ "${pkgs.device-tree_rpi.overlays}/vc4-fkms-v3d.dtbo" ];
    # };
    opengl = {
      enable = true;
      setLdLibraryPath = true;
      package = pkgs.mesa_drivers;
      # extraPackages = with pkgs; [
      #   # vaapiIntel
      #   vaapiVdpau
      #   libvdpau-va-gl
      # ];
    };
  };
}

# vim: fdm=indent:foldlevel=1:foldcolumn=2
