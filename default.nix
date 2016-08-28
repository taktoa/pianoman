{ pkgs ? import <nixpkgs> {} }:


let
  callPackage = pkgs.newScope self;
  self = rec {
    termcolor = callPackage ./misc/termcolor {};

    azmq = callPackage ./misc/azmq {};

    jsoncpp = pkgs.jsoncpp.overrideDerivation (old: {
        cmakeFlags = [
            "-DBUILD_SHARED_LIBS=ON"
            "-DBUILD_STATIC_LIBS=OFF"
            "-DJSONCPP_WITH_CMAKE_PACKAGE=ON"
        ];
    });

    headlessQt = callPackage ./headless-teamspeak/qt5.nix {};

    headlessTeamSpeak = callPackage ./headless-teamspeak {};

    zmqPlugin = callPackage ./plugin { qt5 = headlessQt; };

    headlessTeamSpeakWithPlugins = callPackage ./teamspeak {
      teamspeak_client = headlessTeamSpeak;
      plugins = [ zmqPlugin ];
    };

    teamspeakWithPlugins = callPackage ./teamspeak {
      plugins = [ zmqPlugin ];
    };

    pianoman = callPackage ./pianoman {
      teamspeak_client = teamspeakWithPlugins;
      headlessTeamSpeak = headlessTeamSpeakWithPlugins;
    };
  };
in self
