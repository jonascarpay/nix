{ pkgs, ... }:
let
  pyPkgs = pkgs.python3Packages;
  colorz = pyPkgs.buildPythonPackage rec {
    pname = "colorz";
    version = "1.0.3";
    propagatedBuildInputs = with pyPkgs; [ numpy pillow scipy ];
    src = pyPkgs.fetchPypi {
      inherit pname version;
      sha256 = "0ghd90lgplf051fs5n5bb42zffd3fqpgzkbv6bhjw7r8jqwgcky0";
    };
  };
  haishoku = pyPkgs.buildPythonPackage rec {
    pname = "haishoku";
    version = "1.1.8";
    propagatedBuildInputs = with pyPkgs; [ pillow ];
    src = pyPkgs.fetchPypi {
      inherit pname version;
      sha256 = "1m6bzxlm4pfdpjr827k1zy9ym3qn411lpw5wiaqq2q2q0d6a3fg4";
    };
  };
  mywal = pyPkgs.pywal.overrideAttrs (old: {
    propagatedBuildInputs = old.propagatedBuildInputs ++ [ colorz haishoku ];
  });
in {
  home.packages = [ mywal ];
  xsession.initExtra = ''
    wal -i ~/Wallpapers/papes
  '';
  programs.autorandr.hooks.postswitch = {
    restart-wal = ''wal -i "$(cat /home/jmc/.cache/wal/wal)" -R'';
  };
}
