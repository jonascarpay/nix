{ pkgs, ... }:
let
  secrets = pkgs.flakes.secrets.xc-cache;
in
{
  caches.extraCaches = [{
    url = secrets.cache_url;
    key = secrets.cache_key;
  }];

  home.file.awsCredentials = {
    target = ".aws/credentials";
    text = ''
      [minio]
      aws_access_key_id = ${secrets.aws_access_key_id}
      aws_secret_access_key = ${secrets.aws_secret_access_key}
      [default]
      aws_access_key_id = ${secrets.aws_access_key_id}
      aws_secret_access_key = ${secrets.aws_secret_access_key}
    '';
  };
}
