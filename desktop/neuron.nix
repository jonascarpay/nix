{ pkgs, ... }:
let
  port = "8932";
  neuronPkg = pkgs.flakes.neuron.defaultPackage."x86_64-linux";
  neuronBin = "${neuronPkg}/bin/neuron";
  notesDir = "/home/jmc/slipbox";
in
{
  systemd.user = {
    services.neuron = {
      Unit.Description = "Neuron zettelkasten service";
      Install.WantedBy = [ "graphical-session.target" ];
      Service.ExecStart = "${neuronBin} -d ${notesDir} gen -ws :${port}";
    };
    services.neuron-sync = {
      Unit.Description = "Neuron github sync";
      Service = {
        Type = "oneshot";
        ExecStart =
          let script = pkgs.writeShellScript "neuron-sync" ''
            cd ${notesDir}
            ${pkgs.gitAndTools.git-sync}/bin/git-sync
          '';
          in "${script}";
      };
    };
    timers.neuron-sync = {
      Unit.Description = "Neuron sync timer";
      Timer.OnCalendar = "*:0/15";
      Install.WantedBy = [ "timers.target" ];
    };
  };
  programs.emacs.init.modules.neuron-mode.config = ''
    (use-package neuron-mode
      :after general
      :config
      (setq neuron-default-zettelkasten-directory ${notesDir})
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
}
