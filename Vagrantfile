$script = <<EOF
apt-get update -q
apt-get install -yq software-properties-common
add-apt-repository ppa:webupd8team/java
echo "deb https://dl.bintray.com/sbt/debian /" | tee -a /etc/apt/sources.list.d/sbt.list
echo "deb http://cran.rstudio.com/bin/linux/ubuntu trusty/" | tee -a /etc/apt/sources.list.d/r.list

apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 642AC823
gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9
gpg -a --export E084DAB9 | sudo apt-key add -

apt-get update -q

echo debconf shared/accepted-oracle-license-v1-1 select true | debconf-set-selections
apt-get install -yq oracle-java8-installer sbt unzip r-base ghostscript

wget -q http://www.scala-lang.org/files/archive/scala-2.12.2.deb
dpkg -i scala-2.12.2.deb
rm scala-2.12.2.deb

ZIPFILE=z3-4.5.0-x64-ubuntu-14.04.zip
URL=https://github.com/Z3Prover/z3/releases/download/z3-4.5.0/$ZIPFILE
wget $URL
unzip $ZIPFILE
mv z3-4.5.0-x64-ubuntu-14.04 z3
cp z3/bin/z3 /bin/z3

cp /bin/true /bin/synth

EOF

Vagrant.configure("2") do |config|
  config.vm.box = "ubuntu/trusty64"
  config.ssh.forward_x11 = false
  config.vm.synced_folder ".", "/vagrant"
  config.vm.provision "shell", inline: $script, privileged: true

  config.vm.provider :virtualbox do |vb|
    vb.gui = false
    vb.customize ["modifyvm", :id, "--memory", "4096"]
  end
end
