{
  programs.emacs.init.usePackage = {

    haskell-mode.enable = true;

    eglot = {
      enable = true;
      config = ''
        (add-to-list 'eglot-server-programs '(haskell-mode . ("ghcide" "--lsp")))
      '';
    };

  };
}
