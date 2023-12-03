{ pkgs, ... }:
{
  home = {
    packages = [ pkgs.tmux ];
    file.".tmux.conf".text = ''
      run-shell ${pkgs.tmuxPlugins.sensible}/share/tmux-plugins/sensible/sensible.tmux
      run-shell ${pkgs.tmuxPlugins.pain-control}/share/tmux-plugins/pain-control/pain_control.tmux
      set -g mouse on
      bind v split-window -h
      bind V split-window -v
    '';
  };
}
