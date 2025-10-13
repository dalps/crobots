default:
    dune exec crobots test/sniper test/rook test/rook

build:
    dune build

build-binary: build
    cp _build/default/bin/main.exe crobots.exe