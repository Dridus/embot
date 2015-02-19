cabal install -j4 wreq transformers-0.3.0.0 transformers-compat-0.3.3.3 mtl-2.1.3.1
DYLD_LIBRARY_PATH=/usr/local/opt/icu4c/lib            \
  cabal install --only-dependencies                   \
    --extra-include-dirs=/usr/local/opt/icu4c/include \
    --extra-lib-dirs=/usr/local/opt/icu4c/lib

