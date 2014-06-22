# 𝜋-Base

See the live site at [topology.jdabbs.com](http://topology.jdabbs.com).

### Contributing

I would _love_ bug reports and feature requests. Feel free to submit those in the [GitHub issues](https://github.com/jamesdabbs/pi-base.hs/issues).

If you'd like to contribute code, open up a pull request. I'll review it, merge it in, and push it live once it's good to go.

### Running locally

There are two options for getting a version of the site running locally. In either case, get in touch if you'd like a data set to test against.

In both cases it's important that the port match the one expected in the settings file (3000), so be sure to change that file if you're binding to another port.

#### Direct Setup

Before you begin, you will need to install [the Haskell platform](http://www.haskell.org/platform/).

You will need to make sure that the database specified in `config/` actually exists - either by installing Postgres and creating a database, or by changing the database settings to suit your local environment.

Then run:

```bash
$ cabal sandbox
$ cabal install
$ yesod --dev devel
```

to install the dependencies to your local sandbox and then start dev-mode auto-reloading. Note that the first install may take a while.

#### With Vagrant

You may prefer to develop against a [Vagrant](http://www.vagrantup.com/) box. If so, be sure you have Vagrant installed and do:

```bash
locally $ vagrant up && vagrant ssh

vagrant $ sudo su
vagrant $ cd /vagrant && PATH=$HOME/.cabal/bin:$PATH yesod devel
```

Note that the included Vagrantfile uses some Virtualbox-specific hooks to increase the available memory. If you are using a different provider and processes are getting killed (which may happen - old versions of cabal can be especially memory heavy), you may need to include similar log to up your vm's memory.
