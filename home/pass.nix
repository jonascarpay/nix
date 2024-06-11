{ pkgs, ... }:
{
  programs = {
    gpg.enable = true;
    password-store = {
      enable = true;
      package = pkgs.pass.withExtensions (exts: with exts; [ pass-genphrase ]);
    };
  };
  services.gpg-agent = rec {
    enable = true;
    enableSshSupport = true;
    defaultCacheTtl = 6 * 3600;
    maxCacheTtl = 24 * 3600;
    defaultCacheTtlSsh = defaultCacheTtl;
    maxCacheTtlSsh = maxCacheTtl;
    pinentryPackage = pkgs.pinentry-qt;
  };
}
