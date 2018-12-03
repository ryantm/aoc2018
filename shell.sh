nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [parsers parsec Unique])"
