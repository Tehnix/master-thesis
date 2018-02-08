(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    common = ./common;
    backend = ./backend;
    frontend = ./frontend;
  };

  shells = {
    ghc = ["common" "backend"];
    ghcjs = ["common" "frontend"];
  };

  overrides = self: super: {
    freer-simple = self.callCabal2nix "freer-simple" (pkgs.fetchFromGitHub {
          owner = "Tehnix";
          repo = "freer-simple";
          rev = "e6ed830d99817e30699bde2d514d85c1823b0230";
          sha256 = "11x1b0bfslq6gzx6mphif3gisvvd9r1d9fxyr7gwkfnia98lsaiv";
          fetchSubmodules = true;
    }) {};
    # req = self.callCabal2nix "req" (pkgs.fetchFromGitHub {
    #       owner = "Tehnix";
    #       repo = "req";
    #       rev = "1694c8f35d9b3b8f22b21160a38ab087a1c192e4";
    #       sha256 = "08qby35pk061ys1gfzly34v4wrqfmx3mm88nccj2bn48vbk6zlwp";
    #       fetchSubmodules = true;
    # }) {};
  };

  android.frontend = {
    executableName = "frontend";
    applicationId = "org.example.frontend";
    displayName = "Example Android App";
  };

  ios.frontend = {
    executableName = "frontend";
    bundleIdentifier = "org.example.frontend";
    bundleName = "Example iOS App";
  };
})
