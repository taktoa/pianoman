{ buildEnv, teamspeak_client, plugins ? [] }:

let version = (builtins.parseDrvName teamspeak_client.name).version;
    ts3 = teamspeak_client;
in buildEnv rec {
  name = "teamspeak-with-plugins-${version}";
  paths = plugins ++ [ ts3 ];
  pathsToLink = [ "/lib" "/share" ];
  postBuild = ''
    rm "$out/lib/teamspeak/ts3client"
    cp -vi "${ts3}/lib/teamspeak/ts3client" "$out/lib/teamspeak/ts3client"
    mkdir "$out/bin"
    substitute "${ts3}/bin/ts3client" "$out/bin/ts3client" \
      --replace "${ts3}/bin/.ts3client-wrapped" "$out/lib/teamspeak/ts3client"
    chmod +x "$out/bin/ts3client"
  '';
}
