{ unstable, lib, config, ... }:

let cfg = config.services.git-sync; in
with lib;
{
  options.services.git-sync = with types; mkOption {
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
            default = false;
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

  config.systemd =
    let
      f = { enable, name, description, directory, time, syncNewFiles, requireFlag, message, preSync, user }:
        optionalAttrs enable
          {
            services."${name}" = {
              inherit description;
              path = with unstable.gitAndTools; [ git git-sync ];
              script =
                let
                  setMessage =
                    if isNull message
                    then "git config --local --unset branch.$branch_name.syncCommitMsg || true"
                    else "git config --local branch.$branch_name.syncCommitMsg \"${message}\"";
                in
                ''
                  cd ${directory}

                  ${preSync}

                  branch_name=$(git symbolic-ref -q HEAD)
                  branch_name=\$${branch_name##refs/heads/}
                  ${setMessage}

                  git-sync ${optionalString syncNewFiles "-n"} ${optionalString (!requireFlag) "-s"} sync
                '';
              serviceConfig.Type = "oneshot";
              serviceConfig.User = user;
            };
            timers."${name}" = {
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
