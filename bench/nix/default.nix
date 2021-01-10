let
  sources = import ./sources.nix;
  nixpkgs = import sources.nixpkgs {
    overlays = [
      (_: _: { niv = import sources.niv {}; })
    ];
  };
in with nixpkgs;
mkShell {
  name = "preql";
  buildInputs = [
    stack
    postgresql_12
    zlib
  ];
}
