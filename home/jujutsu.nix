{ unstable, ... }:
{
  programs.jujutsu = {
    enable = true;
    package = unstable.jujutsu;
    settings = {
      user = {
        name = "Jonas Carpay";
        email = "jonascarpay@gmail.com";
      };
      ui = {
        diff-formatter = [ "difft" "--color=always" "$left" "$right" ];
        paginate = "never";
        default-command = "log";
      };
      aliases = {
        init = [ "git" "init" ];
      };
      templates = {
        draft_commit_description = "builtin_draft_commit_description_with_diff";
        # log = ''builtin_log_compact ++ "\n"'';
      };

      # https://github.com/jj-vcs/jj/blob/main/cli/src/config/templates.toml
      template-aliases = {
        "format_timestamp(t)" = "t.ago()";
        "format_short_id(id)" = ''
          id.shortest(3)
        '';
        "format_short_commit_header(commit)" = ''
          separate(" ",
            format_short_change_id_with_change_offset(commit),
            if(!commit.mine(), format_short_signature(commit.author())),
            format_timestamp(commit_timestamp(commit)),
            commit.bookmarks(),
            commit.tags(),
            commit.working_copies(),
            format_commit_labels(commit),
            if(config("ui.show-cryptographic-signatures").as_boolean(),
              format_short_cryptographic_signature(commit.signature())
            ),
          )
        '';
      };
    };
  };
  programs.jjui = {
    enable = true;
    package = unstable.jjui;
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
