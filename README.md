# 𝜋-Base

See the live site at [topology.jdabbs.com](http://topology.jdabbs.com).

_*N.B.* There is a [rewrite in progress](https://github.com/jamesdabbs/pi-base.hs/tree/rewrite) - aimed at splitting out a Servant-based API from a more robust and featureful React frontend, and being in a position to support new auth schemes after Persona shuts down. Progress is halting, but the new version will certainly land by November (as that's end of life for Persona)._

### Contributing

I would _love_ bug reports and feature requests. Feel free to submit those in the [GitHub issues](https://github.com/jamesdabbs/pi-base.hs/issues).

If you'd like to contribute code, open up a pull request. I'll review it, merge it in, and push it live once it's good to go.

### Running locally

There are two options for getting a version of the site running locally. In either case, get in touch if you'd like a data set to test against.

You will need to create a settings file at `config/settings.yml` - refer to `config/settings.yml.example` for an example. Be sure the port declared there is actually the port in use (3000 is the default).

#### Direct Setup

##### Haskell Platform
Before you begin, you will need to install [the Haskell platform](http://www.haskell.org/platform/). Cabal should have at least version 1.18 (to use sandboxes). You may have to update Cabal using Cabal and change the link to the newer version:

```bash
$ cabal update
$ cabal install cabal cabal-install
$ sudo mv /usr/bin/cabal /usr/bin/cabal-old
$ sudo ln -s ~/.cabal/bin/cabal /usr/bin/
```

##### Database

You will need to make sure that the database specified in `config/` actually exists - either by installing Postgres and creating a user and a database, or by changing the database settings to suit your local environment. Installing postgres for the first time can be challenging: you may want to edit `pg_hba.conf` after setting up your role (account) as superuser before you can use something like `createuser -d -r -p 5432 pi_base --password`, see the [postgresql documentation on database roles](http://www.postgresql.org/docs/9.3/static/database-roles.html). The password that is expected can be changed in `config/postgresql.yml`

##### Initial initialization

Initialize a Cabal sandbox, install pi-base and its dependencies and run a yesod development server (auto-reloading upon changes to the code) with:

```bash
$ cabal sandbox init
$ cabal install
$ yesod devel
```

Note that the first install may take a while.

##### Debugging

For debugging, it can be helpful to empty the database. This can be achieved with the following:

```bash
$ dropdb pi_base_dev --port=5432 --username=pi_base
$ createdb pi_base_dev --port=5432 --username=pi_base
```

##### Become admin in pi-base

After you have logged in in the pi-base web application, you can promote yourself to an admin account (only) by manually editing the database. Assuming you have user id 1 in the database, you can do this with psql:

```bash
$ psql --username=pi_base --dbname=pi_base_dev -c "UPDATE remote_users SET admin = True WHERE id = 1;"
```



#### With Vagrant

You may prefer to develop against a [Vagrant](http://www.vagrantup.com/) box. If so, be sure you have Vagrant installed and do:

```bash
locally $ vagrant up && vagrant ssh

vagrant $ sudo su
vagrant $ cd /vagrant && PATH=$HOME/.cabal/bin:$PATH yesod devel
```

Note that the included Vagrantfile uses some Virtualbox-specific hooks to increase the available memory. If you are using a different provider and processes are getting killed (which may happen - old versions of cabal can be especially memory heavy), you may need to include similar log to up your vm's memory.

#### Deployment

The production site is running under [Keter](https://github.com/snoyberg/keter). At some point, we should codify how to spin up a prod / build server. For now you get these notes:

* General security hardening - disable root, password login, fail2ban, change ports, firewall, updates, etc.
* Install and configure keter, start upstart job
* Configure periodic postgres backups
* Configure some sort of monitoring (?)

Use the keter standard `scp pi-base.keter ...:/opts/keter/incoming` to deploy.

#### Building for Deployment

Assuming you have a [docker machine](https://docs.docker.com/machine/) set up `docker-compose start keter` should build a keter package.
