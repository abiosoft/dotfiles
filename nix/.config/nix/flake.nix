{
  description = "My dotfiles";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs, nixpkgs-unstable, ... }:
    let
      name = "abiola";
      build = { system }: (
        let
          pkgs = import nixpkgs { inherit system; config = { allowUnfree = true; }; };
          pkgs-unstable = import nixpkgs-unstable { inherit system; config = { allowUnfree = true; }; };
          env = pkgs.buildEnv {
            name = "${name}";
            paths = import ./core.nix { nixpkgs = pkgs; nixpkgs-unstable = pkgs-unstable; };
          };
        in
        { "${name}" = env; default = env; }
      );
      systems = [ "aarch64-linux" "x86_64-linux" "aarch64-darwin" "x86_64-darwin" ];
      list = map
        (system: {
          name = "${system}";
          value = (build { inherit system; });
        })
        systems;
    in
    { packages = builtins.listToAttrs list; };
}
