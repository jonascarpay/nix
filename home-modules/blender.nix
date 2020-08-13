{ pkgs, lib, ... }:
let
  unstable = import <unstable> { };
  pypkgs = unstable.python3Packages;
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
  # blender = pkgs.blender.overrideAttrs (old: {
  blender = unstable.blender.overrideAttrs (old: {
    postInstall = ''
      wrapProgram $blenderExecutable \
      --prefix PYTHONPATH : ${string} \
      --unset XMODIFIER \
      --unset XMODIFIERS \
      --add-flags '--python-use-system-env'
    '';
  });
in { home.packages = [ blender ]; }
