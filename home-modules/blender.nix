{ pkgs, lib, ... }:
let
  unstable = import <unstable> { };
  pypkgs = unstable.python3Packages;
  python = pypkgs.python;
  select = p:
    with p; [
      certifi # Needed by Hard Ops/Box Cutter
      chardet # Needed by Hard Ops/Box Cutter
      idna # Needed by Hard Ops/Box Cutter
      lxml # Needed by Lily Surface Scraper
      numpy # Needed by Hard Ops/Box Cutter
      pysocks # Needed by Hard Ops/Box Cutter
      requests # Needed by Hard Ops/Box Cutter
      urllib3 # Needed by Hard Ops/Box Cutter
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
  # in { home.packages = [ blender ]; } Disabled because don't have time to wait right now
in { home.packages = [ unstable.blender ]; }
