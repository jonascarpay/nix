{ pkgs, inputs, ... }:
let
  mydmenu = pkgs.dmenu.overrideAttrs
    (old: {
      src = builtins.toPath (inputs.dmenu);
    });
in
{
  home.packages = [ mydmenu ];
}
