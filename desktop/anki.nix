{ pkgs, ... }:
let
  extraArgs = ''
    makeWrapperArgs+=(
      --set ANKI_WEBSCALE 2
    )
  '';
  myAnki = pkgs.anki.overrideAttrs
    (old: { preFixup = old.preFixup + extraArgs; });
in
{ home.packages = [ myAnki ]; }
