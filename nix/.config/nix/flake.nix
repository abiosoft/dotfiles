{
  description = "My dotfiles";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs, nixpkgs-unstable, ... }:
    let
      name = "abiola";
      build = { system }: (
        let
          pkgs = import nixpkgs { system = "${system}"; };
          pkgs-unstable = import nixpkgs-unstable { system = "${system}"; };
          env = pkgs.buildEnv {
            name = "${name}";
            paths = import ./core.nix { nixpkgs = pkgs; nixpkgs-unstable = pkgs-unstable; };
          };
        in
        { ${name} = env; default = env; }
      );
    in
    {
      packages.aarch64-linux = build { system = "aarch64-linux"; };
      packages.x86_64-linux = build { system = "x86_64-linux"; };
      packages.aarch64-darwin = build { system = "aarch64-darwin"; };
      packages.x86_64-darwin = build { system = "x86_64-darwin"; };
    };
}
