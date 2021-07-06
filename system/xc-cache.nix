{ pkgs, config, ... }: {
  age.secrets.xc-cache.file = ../secrets/xc-cache.age;
  age.secrets.xc-cache.mode = "0777";
  nix.extraOptions = ''
    !include ${config.age.secrets.xc-cache.path}
  '';
  age.secrets.xc-s3 = {
    file = ../secrets/xc-s3.age;
    path = "/root/.aws/credentials";
    mode = "0777";
  };
}
