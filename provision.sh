#!/usr/bin/env bash

sudo apt-get remove scala-library scala
wget http://apt.typesafe.com/repo-deb-build-0002.deb
sudo dpkg -i repo-deb-build-0002.deb
sudo apt-get update
sudo apt-get install -y sbt nasm

mkdir -p  /home/vagrant/.sbt/.lib/0.13.1/
wget http://typesafe.artifactoryonline.com/typesafe/ivy-releases/org.scala-sbt/sbt-launch/0.13.1/sbt-launch.jar -O /home/vagrant/.sbt/.lib/0.13.1/sbt-launch.jar

sudo bash -c "echo 1 > /proc/sys/vm/overcommit_memory"
echo "cd /vagrant" >> ~/.bashrc
