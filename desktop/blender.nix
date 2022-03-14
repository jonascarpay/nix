{ unstable, ... }:
let
  my-aseprite = unstable.aseprite-unfree.overrideAttrs (old: rec {
    version = "1.2.32";
    src = unstable.fetchFromGitHub {
      owner = "aseprite";
      repo = "aseprite";
      rev = "v${version}";
      fetchSubmodules = true;
      sha256 = "sha256-DWcX8umWPKD0D5gx75kDIRig1t294ehFZoLnFhaixFY=";
    };
  });
  my-blender = unstable.stdenv.mkDerivation {
    pname = "blender-bin";
    version = "3.1";
    src = builtins.fetchTarball {
      url = https://builder.blender.org/download/daily/blender-3.1.0-stable+v31.c77597cd0e15-linux.x86_64-release.tar.xz;
      sha256 = "sha256:1zz26yv8w9qhccky31q0gjkq8i35v35085dza0av7c5syz75r9ja";
    };
    buildInputs = [ unstable.makeWrapper ];
    preUnpack = ''
      mkdir -p $out/libexec
      cd $out/libexec
    '';
    installPhase = ''
      cd $out/libexec
      mkdir $out/bin
      makeWrapper $out/libexec/source/blender $out/bin/blender \
        --prefix LD_LIBRARY_PATH : /run/opengl-driver/lib:${unstable.lib.makeLibraryPath [ unstable.xorg.libX11 unstable.xorg.libXi unstable.xorg.libXxf86vm unstable.xorg.libXfixes unstable.xorg.libXrender unstable.libGLU unstable.libglvnd unstable.numactl unstable.SDL2 unstable.libdrm unstable.ocl-icd ]}
      patchelf --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" \
        source/blender
      patchelf --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)"  \
        $out/libexec/source/*/python/bin/python3*
    '';
    meta.mainProgram = "blender";
  };
in
{
  home.packages = [
    my-blender
    # my-aseprite
  ];
}
