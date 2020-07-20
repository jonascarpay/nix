{ pkgs, lib, ... }:
let
  pypkgs = pkgs.python3Packages;
  python = pypkgs.python;
  select = p: [
    p.idna
    p.pysocks
    p.certifi
    p.chardet
    p.urllib3
    p.requests
    p.numpy
  ];
  string = lib.concatStringsSep ":"
    (map (p: "${p}/${python.sitePackages}") (select pypkgs));
  blender = pkgs.blender.overrideAttrs (old: {
    postInstall = ''
      wrapProgram $blenderExecutable \
      --prefix PYTHONPATH : ${string} \
      --add-flags '--python-use-system-env'
    '';
  });
in { home.packages = [ blender ]; }
