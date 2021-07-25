{ unstable, lib, config, ... }:
with lib;
let cfg = config.services.git-sync;
in
{
  options = with types;
    {
      services.git-sync = mkOption {
        type = attrsOf
          (submodule ({ name, ... }: {
            options = {
              enable = mkEnableOption "${name} sync service";
              name = mkOption {
                default = "${name}-sync";
                type = str;
              };
              description = mkOption {
                default = "${name} git-sync service";
                type = str;
              };
              path = mkOption {
                type = str;
              };
              time = mkOption {
                default = "*:0/15";
                type = str;
              };
              syncNewFiles = mkOption {
                type = bool;
                default = true;
              };
              requireFlag = mkOption {
                type = bool;
                default = true;
              };
            };
          }));
        default = { };
      };
    };

  config.systemd.user =
    let
      f = { enable, name, description, path, time, syncNewFiles, requireFlag }:
        optionalAttrs enable
          {
            services.${name} = {
              Unit.Description = description;
              Service = {
                Type = "oneshot";
                ExecStart =
                  # https://github.com/simonthum/git-sync
                  let script = unstable.writeShellScript "${name}" ''
                    cd ${path}
                    ${unstable.gitAndTools.git-sync}/bin/git-sync ${optionalString syncNewFiles "-n"} ${optionalString (!requireFlag) "-s"} sync
                  '';
                  in "${script}";
              };
            };
            timers.${name} = {
              Unit.Description = "${description} timer";
              Timer.OnCalendar = time;
              Install.WantedBy = [ "timers.target" ];
            };
          };
    in
    with builtins;
    foldl' recursiveUpdate { }
      (map f
        (attrValues config.services.git-sync));
}
