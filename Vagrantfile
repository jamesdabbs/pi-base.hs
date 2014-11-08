# -*- mode: ruby -*-
# vi: set ft=ruby :

VAGRANTFILE_API_VERSION = "2"

$script = <<SCRIPT
apt-get -q -y update
apt-get -q -y install haskell-platform libpq-dev postgresql
sudo -u postgres psql -c "CREATE ROLE pi_base WITH LOGIN PASSWORD 'pi_base';"
sudo -u postgres psql -c "CREATE DATABASE pi_base_dev WITH OWNER pi_base;"
cabal update
cabal install cabal-install
cd /vagrant && $HOME/.cabal/bin/cabal install . yesod-bin --max-backjumps=-1 --reorder-goals
SCRIPT

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|
  config.vm.box = "ubuntu-14.04-x64"
  config.vm.box_url = "https://cloud-images.ubuntu.com/vagrant/trusty/current/trusty-server-cloudimg-amd64-vagrant-disk1.box"

  config.vm.network :forwarded_port, host: 3000, guest: 3000

  config.vm.provider :virtualbox do |v|
    v.memory = 4 * 1024
  end

  config.vm.provision "shell", inline: $script
end
