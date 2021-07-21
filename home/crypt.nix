{ unstable, ... }:
{
  home.packages = [ unstable.qtpass ];
  programs = {
    gpg.enable = true;
    gpg.package = unstable.gnupg;
    password-store = {
      enable = true;
      settings.PASSWORD_STORE_DIR = "/home/jmc/passwords";
      package = unstable.pass.withExtensions (exts: with exts; [
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
  services.password-store-sync.enable = true;
  services.gpg-agent.enable = true;
}
