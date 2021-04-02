{ pkgs, ... }:

{
  programs.emacs.init.modules = {
    proof-general.config = ''
      (use-package proof-site
      :config
      (setq indent-line-function 'indent-relative)
      )
    '';
  };
}
