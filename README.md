# mesh
The Mesh Engine that implements the Mesh Language, a computational language for web3.

## Dependencies
- libzmq 5+
  - Debian / Ubuntu: `apt install libzmq5-dev`
  - RHEL / CentOS: `yum install zeromq-devel` (epel-release required)
  - MacOS: `brew install zeromq`
  - Arch: `sudo pacman -S zeromq`

## Mesh Jupyter kernel
After building the project with `esy build`, run `esy install-kernel`.

Once that is done, the Mesh kernel should be available in your jupyter notebook.