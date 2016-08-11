# Builds an image with standalone Puppet installed.
FROM ubuntu:14.04
MAINTAINER Arjun Guha <arjun@cs.umass.edu>

RUN apt-get update
RUN apt-get upgrade -y -qq
RUN apt-get install -y -qq wget

RUN wget -q https://apt.puppetlabs.com/puppetlabs-release-trusty.deb
RUN dpkg -i puppetlabs-release-trusty.deb
RUN rm puppetlabs-release-trusty.deb
RUN apt-get install -y -qq vim puppet

USER root

WORKDIR /root

ENTRYPOINT /bin/bash

