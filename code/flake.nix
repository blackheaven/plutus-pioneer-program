{
  description = "pioneers-program";
  inputs = {
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
    horizon-wave-ocean.url = "git+https://gitlab.horizon-haskell.net/package-sets/horizon-wave-ocean";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };
  outputs =
    inputs@{ self
    , flake-utils
    , horizon-wave-ocean
    , nixpkgs
    , ...
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
    let
      pkgs = import nixpkgs { inherit system; };
      hsPkgs =
        with pkgs.haskell.lib;
        horizon-wave-ocean.legacyPackages.${system}.extend (hfinal: hprev:
          {
            ppp-w02 = disableLibraryProfiling (hprev.callCabal2nix "ppp-w02" ./Week02 { });
            ppp-w03 = disableLibraryProfiling (hprev.callCabal2nix "ppp-w03" ./Week03 { });
          });
    in
    {
      devShells.default = hsPkgs.ppp-w03.env.overrideAttrs (attrs: {
        buildInputs = attrs.buildInputs ++ [
          hsPkgs.cabal-install
          pkgs.nixpkgs-fmt
        ];
      });
      packages.default = hsPkgs.ppp-w03;
    });
}
