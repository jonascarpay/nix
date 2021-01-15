{ pkgs, ... }:
let
  inherit (pkgs) fetchpatch;
  configFile = pkgs.writeText "config.def.h" (builtins.readFile ./config.h);
  myst = pkgs.st.overrideAttrs (old: {
    patches = old.patches ++ [
      # Scrollback
      (fetchpatch {
        url = "https://st.suckless.org/patches/scrollback/st-scrollback-0.8.4.diff";
        sha256 = "0valvkbsf2qbai8551id6jc0szn61303f3l6r8wfjmjnn4054r3c";
      })
      # Shift-mousewheel scroll
      (fetchpatch {
        url = "https://st.suckless.org/patches/scrollback/st-scrollback-mouse-20191024-a2c479c.diff";
        sha256 = "0qg20sv64im5lcnfnphnbbiyizwywrg1g6zhxyxqqyf8g33lpbb7";
      })
      # mousehweel scroll (broken)
      # (fetchpatch {
      #   url = "https://st.suckless.org/patches/scrollback/st-scrollback-mouse-altscreen-20200416-5703aa0.diff";
      #   sha256 = "0j13prfvv9mjvyjiklachipzhjdradb1dymbmx7fk4d083il06m6";
      # })

      ./font.diff
      ./zoom.diff
      (fetchpatch {
        url = "https://st.suckless.org/patches/nordtheme/st-nordtheme-0.8.2.diff";
        sha256 = "03b709p3mywydzsi5gx8j24krb63kzm5if4dyqq7s0ahh85qiv7n";
      })
      (fetchpatch {
        url = "https://st.suckless.org/patches/workingdir/st-workingdir-20200317-51e19ea.diff";
        sha256 = "1sqzm920vn8hr3p8mv7p9kflzfksks59454yldw8bryvqrgspdzi";
      })
      (fetchpatch {
        url = "https://st.suckless.org/patches/w3m/st-w3m-0.8.3.diff";
        sha256 = "12gnp4hsidyps2n2cqscac3r2jyanxpwxp998057mdwi7grqcm4x";
      })

    ];
  });
in
{ home.packages = [ myst ]; }
