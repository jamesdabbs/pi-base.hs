# 𝜋-Base

See the live site at [topology.jdabbs.com](http://topology.jdabbs.com).

### Contributing

I would _love_ bug reports and feature requests. Feel free to submit those in the [GitHub issues](https://github.com/jamesdabbs/pi-base.hs/issues).

If you'd like to contribute code, open up a pull request. I'll review it, merge it in, and push it live once it's good to go.

### Running locally

__Note:__ there is an [open issue](https://github.com/jamesdabbs/pi-base.hs/issues/20) to distribute a Vagrant box to make this setup easier. Let me know if this is important to you, so that I can prioritize it.

Before you begin, you will need to install [the Haskell platform](http://www.haskell.org/platform/).

You will need to make sure that the database specified in `config/` actually exists - either by installing Postgres and creating a database, or by changing the database settings to suit your local environment.

Then run:

```bash
$ cabal sandbox
$ cabal install
$ yesod --dev devel
```

to install the dependencies to your local sandbox and then start dev-mode auto-reloading. Note that the first install make take a while.
