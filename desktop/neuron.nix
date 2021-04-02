{ pkgs, ... }:
let
  port = "8932";
  neuronRev = "ed96d7fde277e31924d5d9cf2a315ba4c81f340d";
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
        :keymaps 'neuron-mode-map
        "C-c C-t" 'neuron-toggle-connection-type
        "C-c C-c C-l" 'neuron-toggle-id-visibility
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
