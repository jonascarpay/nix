# Adapted from https://www.reddit.com/r/NixOS/comments/innzkw/pihole_style_adblock_with_nix_and_unbound/
{ pkgs, inputs, ... }:
let

  adblockLocalZones = pkgs.stdenv.mkDerivation {
    name = "unbound-zones-adblock";
    #TODO Move to niv
    src = inputs.hosts + "/hosts";
    phases = [ "installPhase" ];
    installPhase = ''
      ${pkgs.gawk}/bin/awk '{sub(/\r$/,"")} {sub(/^127\.0\.0\.1/,"0.0.0.0")} BEGIN { OFS = "" } NF == 2 && $1 == "0.0.0.0" { print "local-zone: \"", $2, "\" static"}' $src | tr '[:upper:]' '[:lower:]' | sort -u >  $out
    '';
  };

in
{
  networking.firewall.allowedUDPPorts = [ 53 ];
  networking.firewall.allowedTCPPorts = [ 53 ];
  services.unbound = {
    enable = true;
    settings = {
      server = {
        interface = [ "0.0.0.0" ];
        access-control = [
          "10.0.0.0/8 allow"
          "127.0.0.0/8 allow"
          "192.168.0.0/16 allow"
        ];
        hide-identity = true;
        hide-version = true;
        prefetch = true;
        tls-cert-bundle = "/etc/ssl/certs/ca-certificates.crt";
        cache-max-ttl = 14400;
        cache-min-ttl = 1200;
        local-zone = [
          ''"onigiri.lan" redirect''
        ];
        local-data = [
          ''"onigiri.lan A 192.168.1.6"''
        ];
        include = "${adblockLocalZones}";
      };
      remote-control.control-enable = true;
      forward-zone = {
        name = ".";
        forward-addr = [
          "45.90.28.0#fd2d38.dns1.nextdns.io"
          "2a07:a8c0::#fd2d38.dns1.nextdns.io"
          "45.90.30.0#fd2d38.dns2.nextdns.io"
          "2a07:a8c1::#fd2d38.dns2.nextdns.io"
          "1.1.1.1@53#one.one.one.one"
          "8.8.8.8@53#dns.google"
          "9.9.9.9@53#dns.quad9.net"
          "1.0.0.1@53#one.one.one.one"
          "8.8.4.4@53#dns.google"
          "149.112.112.112@53#dns.quad9.net"
        ];
      };
    };
  };

}
