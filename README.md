# pagure-cli

`pagure-cli` is written in Haskell and currently requires a working Haskell
stack to use. In the near future, a COPR will be made which will allow easier
installation until all of our dependencies are officially packaged.

We are using `stack` for this project. There are Fedora and RHEL/CentOS
repositories provided by the upstream here:
https://github.com/commercialhaskell/stack/wiki/Downloads#fedora

## Usage

See `pagure --help` for the list of available commands. Use
`pagure <command> --help` to get command-specific help.

Some existing commands are:

* `pagure version` - show the current pagure-cli and pagure API versions
* `pagure tags <repository>` - show tags of the given repository

## License

`pagure-cli` is released under a BSD-2 license. See `LICENSE` for terms and
details.
