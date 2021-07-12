let
  keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMIPyYw6s7FqmIhmQagfUTzYhe4ZLMTBEDUDsDy8j5cS anpan"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICLZXTo2vmkOc2SlC+7O/SJpyyCezMTKOMfVWXNVAQ9v onigiri"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJZkJAZGwyuLYvevoVsxYlcKZUYgFOznQMA5ugwPQH/p paninix"
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDOlH/zVT9nxSmWA2xcenBhHg0+21WtYV3ZugYWopq1CKdHuPKcgbJ5mq3yvQaWEDJu/bxeuj98F2jJVALJRuVk215GQQxBZlUn3HQefEO5wxpL9lb64B/ElBcoMLBbD3PqdXGvd2QEwOnJvDB+AJY5AtgjEdAvxgfeta/Yd4bmKlSC2Gq6UbpM3zAfN4gH1wRnUm22hTpEsyPVygfu0vfwSY54/DtAXoK7t/6WseLEcIfO/1KD0KZRH+7KDXh3prAV4MwtrNItLGUXDoUzUbF+da/5NHQG+bQrNQiubQJzoGPvo7ecDfvwLDQwBbzS2+OIBQZhP171cQyTmPU0xzwts2cF8XIcJpLVdmU36zHUXxCQIBDzxqAqjLGeh8DbK/e04tlmtfMw0xygUyXI9rXxOFoIy3c5DTDfveYj4G22W2WG+3zX1LznFYNXQLjlsiEheBzt0utC4zbrL7lc0SEihni4yajeWOcCElBBI57NDspI2HtgVXPI15NzbuNM1WCiz6Hq9xtTgums5n2K6bIKFvvqvhMBbJctBFqgthG3j24J/ajK6ZhCl5aqsxr6tRKr0M5qvAGnu0cNxAhmVkx2L7gOopxccVRuPx91moDOvPb185SWf/zL0NSkiSNhuO2lMZ9XjxMOf+VShSvEPu8mTksy0GumhxEjmXx4yhhAnw== jonascarpay@gmail.com"
  ];
in
{
  "openvpn.age".publicKeys = keys;
  "xc-cache.age".publicKeys = keys;
  "xc-s3.age".publicKeys = keys;
}

# vim: nowrap
