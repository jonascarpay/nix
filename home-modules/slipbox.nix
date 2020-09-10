{ pkgs, lib, ... }:
let

  boxdir = "/home/jmc/slipbox";
  port = "9999";
  rev = "8894f9449d04b05d58f41c9b71472333eaaa4471";
  substitutions = [
    "s/Zettelkasten/Slipbox/g"
    "s/zettelkasten/slipbox/g"
    "s/Folgezettel/Index/g"
    "s/folgezettel/index/g"
    "s/Zettels/Slips/g"
    "s/zettels/slips/g"
    "s/Zettel/Slip/g"
    "s/zettel/slip/g"
  ];

  unstable = import <unstable> { };

  neuronSrc = builtins.fetchGit {
    url = "https://github.com/srid/neuron.git";
    inherit rev;
  };
  neuron = import "${patchSource neuronSrc}" { };
  neuron-plugin = pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "neuron-vim";
    version = "1";
    # For some reason, this kills the help page?
    src = patchSource (builtins.fetchGit {
      url = "https://github.com/ihsanturk/neuron.vim.git";
      rev = "07521a3ef2940bd726e7b4d50b82e46898e686cc";
    });
  };

  patchSource = source: source;
  # patchSource = source:
  #   let sedExpr = lib.concatStringsSep ";" substitutions;
  #   in pkgs.stdenv.mkDerivation {
  #     name = "src-unzettled";
  #     unpackPhase = "true";
  #     installPhase = ''
  #       set -e
  #       mkdir $out
  #       cd ${source}
  #       for SRC in $(find . -type f); do
  #         TRG=$(echo $SRC | sed "${sedExpr}")
  #         DIR=$(dirname $TRG)
  #         mkdir -p "$out/$DIR"
  #         sed "${sedExpr}" $SRC > "$out/$TRG"
  #       done
  #     '';
  #   };

in {

  home.packages = [ pkgs.ripgrep neuron ];

  systemd.user.services.neuron = {
    Unit.Description = "Neuron service";
    Install.WantedBy = [ "graphical-session.target" ];
    Service = {
      ExecStart =
        "${neuron}/bin/neuron -d ${boxdir} rib -s localhost:${port} -w";
    };
  };

  programs.fish.shellAbbrs = { vz = "cd ${boxdir}; and vim Index.md"; };

  programs.neovim.init.modules = gh: {
    zettel = {
      plugins = [ neuron-plugin ];
      config = ''
        let g:zkdir = "${boxdir}/"
        let g:path_jq = "${pkgs.jq}/bin/jq"
        let g:path_neuron = "${neuron}/bin/neuron"
      '';
    };
  };

}
