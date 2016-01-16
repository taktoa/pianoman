{ qt55, lib,
  libpulseaudio,
  openssl, sqlite, icu,
  fontconfig, freetype,
  glib, udev, dbus,
  libxml2, libxslt, pcre,
  zlib, libjpeg, libpng, libtiff
}:

let version = (builtins.parseDrvName qt55.qtbase.name).version;
in lib.setName "headless-teamspeak-qt5-${version}" (qt55.qtbase.overrideDerivation (old: {
  nativeBuildInputs = old.nativeBuildInputs ++ [ libpulseaudio pcre ];

  propagatedBuildInputs = [
    libpulseaudio
    openssl sqlite icu
    fontconfig freetype
    glib udev dbus.libs
    libxml2 libxslt pcre
    zlib libjpeg libpng libtiff
  ];

  # Notes:
  # This builds with the linuxfb platform.
  # If you want to run this headlessly, put this in the environment:
  #     > export QT_QPA_PLATFORM="linuxfb:fb=/dev/null"
  # You may also want to try building with wayland or eglfs or kms.
  # The flags to futz with are:
  #     -xcb -linuxfb -qpa xcb ... -no-eglfs -no-kms -no-wayland -no-opengl
  # I think -qpa xcb just sets the default QPA, but I am not sure.
  # We will obviously ultimately want to only build one platform.

  configureFlags = ''
      -verbose
      -confirm-license
      -opensource
      -shared
      -pkg-config
      -release
      -c++11
      -rpath
      -strip
      -reduce-relocations
      -optimized-qmake
      -qml-debug
      -gui
      -widgets
      -nis
      -iconv
      -icu
      -pch
      -qpa offscreen
      -no-qpa-platform-guard
      -glib
      -largefile
      -pulseaudio
      -dbus
      -no-sql-psql
      -no-sql-mysql
      -no-sql-odbc
      -no-sql-tds
      -no-sql-oci
      -no-sql-db2
      -no-sql-ibase
      -no-gtkstyle
      -no-eglfs
      -no-kms
      -no-cups
      -no-wayland
      -no-xcb
      -no-linuxfb
      -no-opengl
      -make   libs
      -nomake tools
      -nomake examples
      -nomake tests
      -system-zlib
      -system-libpng
      -system-libjpeg
      -system-sqlite
      -openssl-linked
      -dbus-linked
  '';
}))
