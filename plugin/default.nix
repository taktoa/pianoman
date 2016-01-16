# Build dependencies
{ clangStdenv, cmake, pkgconfig, doxygen, ninja
# Program dependencies
, boost, zeromq, cppzmq, libmsgpack
# Misc dependencies
, guile, parallel
}:

clangStdenv.mkDerivation rec {
  name = "teamspeak-zeromq-plugin-0.0.1";

  src = ./.;

  buildInputs = [ cmake pkgconfig ninja doxygen
                  boost zeromq cppzmq libmsgpack
                  guile parallel ];

  cmakeFlags = "-GNinja";

  preConfigure = ''
      cp ${cppzmq}/include/zmq.hpp include/
  '';

  buildPhase = "ninja";

  installPhase = "ninja install";

  shellHook = ''
      cd ${toString src}
      [[ -d "$(pwd)/build" ]] && rm -rf build
      source ${../helpers}
      OLD_CMAKE_FLAGS="$cmakeFlags"
      cmakeFlags="-DCMAKE_CXX_FLAGS=-v $cmakeFlags" cmakeConfigurePhase
      BUILD_LOG="$(mktemp -p /tmp build-log.XXXXXX)"
      ninja &> "$BUILD_LOG"
      echo ""
      echo ""
      echo ""
      note "FLYCHECK-CLANG-INCLUDE-PATH:"
      echo ""
      INCLUDE_PATH="$(generate-emacs-include-path "$BUILD_LOG")"
      echo "$INCLUDE_PATH"
      echo ""
      echo ""
      echo ""
      read -p "Copy generated include path into clipboard? [y/N] "
      if text-represents-yes "$REPLY"; then
          echo "$INCLUDE_PATH" | clipboard
      fi
      echo ""
      echo ""
      echo ""
      rm "$BUILD_LOG"
      cmakeFlags="$OLD_CMAKE_FLAGS"
      unset OLD_CMAKE_FLAGS BUILD_LOG
      refresh-plugin-build "${toString src}"
      note "Entering nix-shell"
  '';
}
