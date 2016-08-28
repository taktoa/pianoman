{ stdenv, teamspeak_client, libpulseaudio, headlessQt }:

let ts3Version = (builtins.parseDrvName teamspeak_client.name).version;
    rename = stdenv.lib.setName "teamspeak-headless-client-${ts3Version}";
    inputs = {
      inherit libpulseaudio;
      qtbase   = headlessQt;
      # We don't want to provide freetype or any of the X libraries,
      # but we need to give a path
      freetype = headlessQt;
      xorg = {
        libSM       = headlessQt;
        libICE      = headlessQt;
        libxcb      = headlessQt;
        libXrender  = headlessQt;
        libXrandr   = headlessQt;
        libXfixes   = headlessQt;
        libXcursor  = headlessQt;
        libXinerama = headlessQt;
        libXext     = headlessQt;
        libX11      = headlessQt;
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
