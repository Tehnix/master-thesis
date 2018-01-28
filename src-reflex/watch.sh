# { ghcid --command 'nix-shell -A shells.ghc --command "cabal new-repl common"' \
#   & ghcid --command 'nix-shell -A shells.ghc --command "cabal new-repl frontend"' \
#   & ghcid --command 'nix-shell -A shells.ghc --command "cabal new-repl backend"'; }

watchman-make \
    --make 'clear && cabal new-build all -fno-bytecode' \
    -p '**/*.hs' \
    -t ''
