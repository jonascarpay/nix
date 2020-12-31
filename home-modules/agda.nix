{ pkgs, config, ... }: {
  home = {
    packages = [
      (pkgs.agda.withPackages (p: [ p.standard-library ]))
    ];
    file.".agda/defaults".text = "standard-library";
  };
}
