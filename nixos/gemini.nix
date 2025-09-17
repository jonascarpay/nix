{ pkgs, unstable, config, ... }:
let
  my-gemini = pkgs.writeShellScriptBin "gemini-cli" ''
    GEMINI_API_KEY=$(cat ${config.age.secrets.gemini-api-key.path}) ${unstable.gemini-cli-bin}/bin/gemini $@
  '';
in
{
  age.secrets.gemini-api-key = {
    file = ../secrets/gemini-api-key.age;
    owner = "jmc";
    group = "users";
  };
  home-manager.users.jmc = {
    home.packages = [
      my-gemini
    ];
  };
}
