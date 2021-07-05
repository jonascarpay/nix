{
  zsh = {
    enable = true;
    enableCompletion = true;
    enableAutosuggestions = true;
    initExtra = ''
      autoload edit-command-line
      zle -N edit-command-line
      bindkey "^X^E" edit-command-line
    '';
  };

  # NOTE currently only works with ZSH for some reason
  powerline-go.enable = true;

}
