# pagure-cli

`pagure-cli` is written in Haskell and currently requires a working Haskell
stack to use. In the near future, a COPR will be made which will allow easier
installation until all of our dependencies are officially packaged.

We are using `stack` for this project. There are Fedora and RHEL/CentOS
repositories provided by the upstream here:
https://github.com/commercialhaskell/stack/wiki/Downloads#fedora

## Usage

### Configuration

pagure-cli uses Git's configuration system for storing and accessing certain
information such as the Pagure instance to connect to, and the key with which to
authenticate.

You can set your "default" instance by having a *global* `git-config` variable
point to it. For specific projects, you can override it as a *local*
`git-config` variable.

To do this, run

```
$ git config --global pagure.instance https://pagure.io
```

To override this value for a specific project, run

```
$ git config --local pagure.instance https://your.instance.tld
```

from your project's working directory.

To administrate your project, you'll need to store an API key. We suggest you
store this key in a *local* `git-config` variable to your project's working
directory. To do this, from your project's working directory, run

```
$ git config --local pagure.key YOUR_KEY_HERE
```

### Commands

See `pagure --help` for the list of available commands. Use
`pagure <command> --help` to get command-specific help.

Some existing commands are:

* `pagure version` - show the current pagure-cli and pagure API versions
* `pagure tags <repository>` - show tags of the given repository

## License

`pagure-cli` is released under a BSD-2 license. See `LICENSE` for terms and
details.
