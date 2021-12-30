{
  programs = {
    zsh = {
      enable = true;
      enableCompletion = true;
      enableAutosuggestions = true;
      initExtra = ''
        autoload edit-command-line
        zle -N edit-command-line
        bindkey "^X^E" edit-command-line
        bindkey "^P" up-line-or-search
        bindkey "^N" down-line-or-search
      '';
    };

    # NOTE currently only works with ZSH for some reason
    # powerline-go.enable = true;
  };
}
