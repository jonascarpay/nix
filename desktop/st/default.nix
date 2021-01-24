{ pkgs, ... }:
let
  inherit (pkgs) fetchurl;
  configFile = pkgs.writeText "config.def.h" (builtins.readFile ./config.h);
  myst = pkgs.st.overrideAttrs (old: {
    buildInputs = old.buildInputs ++ [ pkgs.harfbuzz ];
    patches = old.patches ++ [
      # Scrollback
      (fetchurl {
        url = "https://st.suckless.org/patches/scrollback/st-scrollback-0.8.4.diff";
        sha256 = "0i0fav13sxnsydpllny26139gnzai66222502cplh18iy5fir3j1";
      })
      # Shift-mousewheel scroll
      (fetchurl {
        url = "https://st.suckless.org/patches/scrollback/st-scrollback-mouse-20191024-a2c479c.diff";
        sha256 = "0z961sv4pxa1sxrbhalqzz2ldl7qb26qk9l11zx1hp8rh3cmi51i";
      })
      # non-shift mousewheel scroll (needs previous 2)
      ./mousewheelincrement.diff

      (fetchurl {
        url = "https://st.suckless.org/patches/nordtheme/st-nordtheme-0.8.2.diff";
        sha256 = "0ssj7gsb3snk1pqfkffwc0dshrbmvf7ffqvrdi4k2p451mnqmph1";
      })
      (fetchurl {
        url = "https://st.suckless.org/patches/workingdir/st-workingdir-20200317-51e19ea.diff";
        sha256 = "1pwx6gppqbx0gkyw62i1qwpigr9nw0chnzr0f3jylv9gzxyvxfhy";
      })
      (fetchurl {
        url = "https://st.suckless.org/patches/w3m/st-w3m-0.8.3.diff";
        sha256 = "1cwidwqyg6qv68x8bsnxns2h0gy9crd5hs2z99xcd5m0q3agpmlb";
      })
      (fetchurl {
        url = "https://st.suckless.org/patches/ligatures/0.8.3/st-ligatures-scrollback-20200430-0.8.3.diff";
        sha256 = "0c7g3wcacxlrs7v0j2drgqg2wksggjicsym6pqawca8fi15bkbfq";
      })

      ./font.diff
      ./zoom.diff
    ];
  });
in
{ home.packages = [ myst ]; }
