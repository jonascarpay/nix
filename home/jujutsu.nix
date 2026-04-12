{
  programs.jujutsu = {
    enable = true;
    settings = {
      user.name = "Jonas Carpay";
      user.email = "jonascarpay@gmail.com";
      ui.diff-formatter = [ "difft" "--color=always" "$left" "$right" ];
      ui.paginate = "never";
    };
  };
  programs.jjui = {
    enable = true;
    settings = {
      preview.show_at_start = true;
    };
  };
  programs.fish.shellAbbrs = {
    j = "jjui";
    js = "jj status";
    je = "jj edit";
    jd = "jj describe";
    jn = "jj new";
    jl = "jj log";
  };
}
