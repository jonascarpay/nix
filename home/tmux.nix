{ pkgs, ... }:
{
  home = {
    packages = [ pkgs.tmux ];
    file.".tmux.conf".text = with pkgs.tmuxPlugins; ''
      # run-shell ${sensible}/share/tmux-plugins/sensible/sensible.tmux
      run-shell ${pain-control}/share/tmux-plugins/pain-control/pain_control.tmux

      set  -g base-index      0
      setw -g pane-base-index 0
      setw -g window-size smallest

      set -g status-keys vi
      set -g mode-keys   vi
      setw -g clock-mode-style  24

      set -g mouse on
      bind v split-window -h
      bind V split-window -v
    '';
  };
}
