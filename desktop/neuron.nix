{ config, inputs, ... }:
let
  notesDirectory = "/home/jmc/Slipbox";
  port = "8080";
in
{
  imports = [ inputs.neuron.homeManagerModule ];
  services.neuron = { inherit port notesDirectory; };
  programs.emacs.init.modules.neuron-mode.config = ''
    (use-package neuron-mode
      :after general
      :config
      (setq neuron-default-zettelkasten-directory "${notesDirectory}")
      (setq neuron-executable "${config.services.neuron.package}/bin/neuron")
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
}
