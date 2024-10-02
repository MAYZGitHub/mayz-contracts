#!/bin/bash

chmod +x /home/plutus/workspace/.devcontainer/scripts/on-start-container.sh

echo "Container created. Executing cabal update..."

cabal update

echo "Done!"
