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
              enable = mkOption {
                type = bool;
                default = true;
              };
              name = mkOption {
                default = "${name}-sync";
                type = str;
              };
              description = mkOption {
                default = "${name} git-sync service";
                type = str;
              };
              directory = mkOption {
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
              message = mkOption {
                type = nullOr str;
                default = null;
              };
              preSync = mkOption {
                type = lines;
                default = "";
              };
              user = mkOption {
                type = str;
                default = "jmc";
              };
            };
          }));
        default = { };
      };
    };

  config =
    let
      f = { enable, name, description, directory, time, syncNewFiles, requireFlag, message, presync, user }:
        optionalAttrs enable
          {
            systemd.services."${name}-sync" = {
              inherit description;
              script =
                let
                  setMessage =
                    if isNull message
                    then "git config --local branch.$branch_name.syncCommitMsg ${message}"
                    else "git config --local --unset branch.$branch_name.syncCommitMsg || true";
                in
                ''
                  cd ${directory}

                  ${preSync}

                  branch_name=$(git symbolic-ref -q HEAD)
                  branch_name=\$${branch_name##refs/heads/}
                  ${setMessage}

                  ${unstable.gitAndTools.git-sync}/bin/git-sync ${optionalString syncNewFiles "-n"} ${optionalString (!requireFlag) "-s"} sync
                '';
              serviceConfig.Type = "oneshot";
              serviceConfig.User = user;
            };
            systemd.timers."${name}-sync" = {
              description = "${description} timer";
              timerConfig.OnCalendar = time;
              wantedBy = [ "default.target" "timers.target" ];
            };
          };
    in
    with builtins;
    foldl' recursiveUpdate { }
      (map f
        (attrValues config.services.git-sync));
}
