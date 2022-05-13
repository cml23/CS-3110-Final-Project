## System Requirements
- OCaml >= 4.14
- Opam >= 2.1.2
- Dune >= 3.03

Other versions may work but only this version has been tested.

#### Required Packages

- ANSITerminal
- Lablgtk
- Graphics
- Camlimages

## Package Installation

#### For both MacOS and Windows:
``` console
opam install yojson
opam install ANSITerminal
opam install onunit2
```
#### Installing Graphics and CamlImages for Windows:
``` console
sudo apt-get install pkg-config
opam install graphics
opam install user-setup
opam user-setup install
```
``` console
sudo apt-get install libgtk2.0-dev
opam install lablgtk
opam install camlimages
```
Follow these instructions to download an X-server: https://www.youtube.com/watch?v=4SZXbl9KVsw
``` console
eval $(opam env)
export DISPLAY=$(awk '/nameserver / {print $2; exit}' /etc/resolv.conf 2>/dev/null):0
export LIBGL_ALWAYS_INDIRECT=1
```
#### Installing Graphics for MacOS:
``` console
opam install graphics
opam install camlimages
```
Computer restart may be necessary after installation.

## Running the Game
- Clone this repository
- `cd` to the cloned folder
- Run `make play` to start the game
- Follow the instructions printed in the terminal
