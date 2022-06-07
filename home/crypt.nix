{ pkgs, ... }:
let
  passdir = "/home/jmc/Passwords";
in
{
  home.packages = [ pkgs.qtpass ];
  programs = {
    gpg.enable = true;
    password-store = {
      enable = true;
      settings.PASSWORD_STORE_DIR = passdir;
      package = pkgs.pass.withExtensions (exts: with exts; [
        pass-audit
        pass-checkup
        pass-genphrase
        pass-import
        pass-otp
        pass-tomb
        pass-update
      ]);
    };
  };
  services.gpg-agent = rec {
    enable = true;
    enableSshSupport = true;
    defaultCacheTtl = 6 * 3600;
    maxCacheTtl = 24 * 3600;
    defaultCacheTtlSsh = defaultCacheTtl;
    maxCacheTtlSsh = maxCacheTtl;
  };
}
