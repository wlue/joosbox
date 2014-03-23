#!/usr/bin/env bash

sudo apt-get update
sudo apt-get remove scala-library scala
wget www.scala-lang.org/files/archive/scala-2.10.3.deb
sudo dpkg -i scala-2.10.3.deb
sudo apt-get install -f -y
sudo apt-get install -y scala
sudo apt-get install -f -y
sudo apt-get install -y scala
sudo apt-get install -y nasm build-essential
sudo apt-get install -y sbt

# THANKS OBAMA
update-alternatives --install /usr/bin/bugpoint bugpoint /usr/bin/bugpoint-3.0 100
update-alternatives --install /usr/bin/llc llc /usr/bin/llc-3.0 100
update-alternatives --install /usr/bin/lli lli /usr/bin/lli-3.0 100
update-alternatives --install /usr/bin/llvm-ar llvm-ar /usr/bin/llvm-ar-3.0 100
update-alternatives --install /usr/bin/llvm-as llvm-as /usr/bin/llvm-as-3.0 100
update-alternatives --install /usr/bin/llvm-bcanalyzer llvm-bcanalyzer /usr/bin/llvm-bcanalyzer-3.0 100
update-alternatives --install /usr/bin/llvm-config llvm-config /usr/bin/llvm-config-3.0 100
update-alternatives --install /usr/bin/llvm-cov llvm-cov /usr/bin/llvm-cov-3.0 100
update-alternatives --install /usr/bin/llvm-diff llvm-diff /usr/bin/llvm-diff-3.0 100
update-alternatives --install /usr/bin/llvm-dis llvm-dis /usr/bin/llvm-dis-3.0 100
update-alternatives --install /usr/bin/llvm-dwarfdump llvm-dwarfdump /usr/bin/llvm-dwarfdump-3.0 100
update-alternatives --install /usr/bin/llvm-extract llvm-extract /usr/bin/llvm-extract-3.0 100
update-alternatives --install /usr/bin/llvm-ld llvm-ld /usr/bin/llvm-ld-3.0 100
update-alternatives --install /usr/bin/llvm-link llvm-link /usr/bin/llvm-link-3.0 100
update-alternatives --install /usr/bin/llvm-mc llvm-mc /usr/bin/llvm-mc-3.0 100
update-alternatives --install /usr/bin/llvm-nm llvm-nm /usr/bin/llvm-nm-3.0 100
update-alternatives --install /usr/bin/llvm-objdump llvm-objdump /usr/bin/llvm-objdump-3.0 100
update-alternatives --install /usr/bin/llvm-prof llvm-prof /usr/bin/llvm-prof-3.0 100
update-alternatives --install /usr/bin/llvm-ranlib llvm-ranlib /usr/bin/llvm-ranlib-3.0 100
update-alternatives --install /usr/bin/llvm-rtdyld llvm-rtdyld /usr/bin/llvm-rtdyld-3.0 100
update-alternatives --install /usr/bin/llvm-size llvm-size /usr/bin/llvm-size-3.0 100
update-alternatives --install /usr/bin/llvm-stub llvm-stub /usr/bin/llvm-stub-3.0 100
update-alternatives --install /usr/bin/llvm-tblgen llvm-tblgen /usr/bin/llvm-tblgen-3.0 100
update-alternatives --install /usr/bin/macho-dump macho-dump /usr/bin/macho-dump-3.0 100
update-alternatives --install /usr/bin/opt opt /usr/bin/opt-3.0 100
