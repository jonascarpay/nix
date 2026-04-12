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
    j = "jj";
    jb = "jj bookmark";
    jd = "jj describe";
    je = "jj edit";
    jg = "jj git";
    jl = "jj log";
    jn = "jj new";
    js = "jj status";
    u = "jjui";
  };
}
