{ lib, ... }:
let
  inherit (lib) types;
  formatterType = types.submodule ({ name, ... }: {
    options = {
      exe = lib.mkOption {
        type = types.str;
      };
      args = lib.mkOption {
        default = [ ];
        type = types.listOf types.str;
      };
      stdin = lib.mkOption {
        default = true;
        type = types.bool;
      };
    };
  });
in
{
  options.programs.neovim = {
    formatters = lib.mkOption {
      type = types.attrsOf formatterType;
    };
    extraLspConfig = lib.mkOption {
      type = types.lines;
      default = "";
    };
  };
}
