#!/bin/bash -ex
curl -s https://raw.github.com/paulp/sbt-extras/master/sbt > sbt
chmod u+x sbt
./sbt compile
