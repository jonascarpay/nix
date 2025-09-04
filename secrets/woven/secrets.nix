let
  keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICfujv3vIl7EeRvjUyJyBZpFTSU6DguSYlJpSXzD7H7X id_ed25519"

    # Recipients
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPUuRgb7gAxHOQ32Y48a5eUeyUr3UoFEh8SYOtHnFxNX dumple"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICvKw1YnFtsHBXtRTCudAzMsq1R2CYSa960SLUbCVOJW floorbox"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIElbM8ysdx1wzQ+QSigNY21OEUnewcQ4NdXjtmxYykjB deskflap"
  ];
in
{
  "id_personal_github.age".publicKeys = keys;
  "netrc.age".publicKeys = keys;
}
