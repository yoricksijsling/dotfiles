#!/bin/bash

# Rebuild the documentation and hoogle database for a stack project.
#
# Usage:
#
#   cd ~/my/stack/project
#   ~/dotfiles/hoogle-build.sh
#
# Use a different stack executable:
#
#   STACK="stack -j4" ~/dotfiles/hoogle-build.sh
#

if [ -z "$STACK" ]; then
    STACK="stack"
fi

$STACK build hoogle

# First generate haddock documentation
$STACK build \
   --fast \
   --haddock --haddock-arguments '--no-print-missing-docs' \
   --test --no-run-tests \
   --bench --no-run-benchmarks

# Now generate the database. Somewhat similar to what
# https://github.com/commercialhaskell/stack/blob/89d165b8df7f09263e3e15fc165d9da35bf37b60/src/Stack/Hoogle.hs#L66
# does.
$STACK exec hoogle -- generate \
                    --local=$($STACK path --local-doc-root) \
                    --database=$($STACK path --local-hoogle-root)/database.hoo

# Now you can search with:
# st exec hoogle -- search Shmonduit --database=$(st path --local-hoogle-root)/database.hoo
