{ config, ... }:
{
  age.secrets.xc-nixconf.file = ../secrets/xc-nixconf.age;
  age.secrets.xc-netrc.file = ../secrets/xc-netrc.age;
  nix.extraOptions = ''
    !include ${config.age.secrets.xc-nixconf.path}
    netrc-file = ${config.age.secrets.xc-netrc.path}
  '';
}
