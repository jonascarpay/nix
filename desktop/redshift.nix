{
  services.redshift =
    let
      delft = {
        latitude = "52.0115769";
        longitude = "4.3570677";
      };
      tokyo = {
        latitude = "35.6762";
        longitude = "139.6503";
      };
    in
    {
      enable = true;
      tray = true;
      inherit (tokyo) latitude longitude;
    };
}
