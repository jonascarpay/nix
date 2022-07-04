{ pkgs, ... }:
{

  home.packages = [
    pkgs.ranger
    pkgs.python3Packages.ueberzug
  ];

  # see https://github.com/ranger/ranger/issues/1853
  xdg.configFile."ranger/rifle.conf".source =
    let
      confPatch = pkgs.writeText "patch" ''
        265c265
        < label open, has xdg-open = xdg-open -- "$@"
        ---
        > label open, has xdg-open = xdg-open "$@"
      '';
    in
    pkgs.runCommand "rifle-conf" { } ''
      ${pkgs.ranger}/bin/ranger --copy-config=rifle --confdir=$TMPDIR
      patch $TMPDIR/rifle.conf -o $out < ${confPatch}
    '';
  xdg.configFile."ranger/rc.conf".text = ''
    set preview_images true
    set preview_images_method ueberzug
  '';
}
