{ pkgs, config, ... }:
let unstable = import <unstable> { inherit (config.nixpkgs) config; };

in {
  # nixpkgs.overlays = [ (_: _: { inherit (unstable) mopidy; }) ];

  services.mopidy = {
    enable = true;
    extensionPackages = with pkgs; [
      mopidy-spotify
      mopidy-iris
      # mopidy-moped # broken
      # mopidy-mopify # broken
      # mopidy-mpd # Not yet in stable, unstable don't work
      # mopidy-youtube # broken
      # mopidy-soundcloud # broken
    ];
    configuration = ''
      [spotify]
      enabled = true
      username = ${config.secrets.spotify.username}
      password = ${config.secrets.spotify.password}
      client_id = ${config.secrets.spotify.client_id}
      client_secret = ${config.secrets.spotify.client_secret}
      bitrate = 320
    '';
  };

}
