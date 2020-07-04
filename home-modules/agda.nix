{ pkgs, config, ... }: {
  config = {
    home = {
      packages = [ pkgs.haskellPackages.Agda ];
      file = {
        ".agda/standard-library.agda-lib".text = ''
          name: standard-library
          include: ${pkgs.AgdaStdlib}/share/agda/
        '';
        ".agda/libraries".text = ''
          /home/jmc/.agda/standard-library.agda-lib
        '';
        ".agda/defaults".text = ''
          standard-library
        '';
      };
    };
  };
}
