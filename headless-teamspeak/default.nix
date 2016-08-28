{ stdenv, teamspeak_client, libpulseaudio, headlessQt }:

let ts3Version = (builtins.parseDrvName teamspeak_client.name).version;
    rename = stdenv.lib.setName "teamspeak-headless-client-${ts3Version}";
    inputs = {
      inherit libpulseaudio;
      inherit (headlessQt) qtbase;
      # We don't want to provide freetype or any of the X libraries,
      # but we need to give a path
      freetype = headlessQt.qtbase;
      xorg = {
        libSM       = headlessQt.qtbase;
        libICE      = headlessQt.qtbase;
        libxcb      = headlessQt.qtbase;
        libXrender  = headlessQt.qtbase;
        libXrandr   = headlessQt.qtbase;
        libXfixes   = headlessQt.qtbase;
        libXcursor  = headlessQt.qtbase;
        libXinerama = headlessQt.qtbase;
        libXext     = headlessQt.qtbase;
        libX11      = headlessQt.qtbase;
      };
    };
in rename ((teamspeak_client.override inputs).overrideDerivation (old: {
  postInstall = old.installPhase + ''
      mv $out/bin/ts3client $out/bin/headless-teamspeak
      mv $out/lib/teamspeak $out/lib/headless-teamspeak
      rm $out/bin/.ts3client-wrapped
      ln -s $out/lib/headless-teamspeak/ts3client \
            $out/bin/.ts3client-wrapped
      rm -rf $out/share
  '';
}))
