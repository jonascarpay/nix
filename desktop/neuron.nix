{ pkgs, ... }:
let
  port = "8932";
  neuronRev = "2547c2132783c6909b6f15837ccd92988c389e8e"; # Dec 2020
  neuron = import (builtins.fetchTarball "https://github.com/srid/neuron/archive/${neuronRev}.tar.gz") { };
  neuronBin = "${neuron}/bin/neuron";
  notesDir = "/home/jmc/slipbox";
in
{
  home.packages = [ pkgs.ispell ];
  systemd.user.services.neuron = {
    Unit.Description = "Neuron zettelkasten service";
    Install.WantedBy = [ "graphical-session.target" ];
    Service.ExecStart = "${neuronBin} -d ${notesDir} gen -ws :${port}";
  };

  programs.emacs.init.modules.neuron-mode.config = ''
    (use-package neuron-mode
      :after general
      :config
      (setq neuron-default-zettelkasten-directory "~/slipbox")
      (setq neuron-executable "${neuronBin}")
      (setq neuron-rib-server-port ${port})
      (setq-default markdown-hide-markup t)
      (general-define-key
        :states '(normal insert)
        "C-c C-t" 'neuron-toggle-connection-type
      )
    )
  '';
  # programs.neovim.init.modules = _: {
  #   neuron = {
  #     plugins = [ (import <unstable> { }).vimPlugins.neuron-vim ];
  #     config = ''
  #       let g:neuron_dir = "${notesDir}/"
  #       let g:neuron_executable = "${neuronBin}"
  #     '';
  #   };
  # };
}
