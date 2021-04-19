{ lib, ... }:
with lib;
{
  options.channels = mkOption {
    type = types.attrs;
    default = { };
    description = "Named channels";
  };
  config.channels = import ../channels.nix;
}
