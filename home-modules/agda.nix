{ pkgs, config, ... }: {
  home = {
    packages = [
      (pkgs.agda.withPackages (p: [ p.standard-library ]))
      pkgs.mononoki
    ];
    file.".agda/defaults".text = "standard-library";
  };
  programs.emacs.init.prelude = ''
    (load-file (let ((coding-system-for-read 'utf-8))
                    (shell-command-to-string "agda-mode locate")))
    (set-face-attribute 'default nil
      :family "mononoki"
      :height 120
      :weight 'normal
      :width 'normal)
  '';
}
