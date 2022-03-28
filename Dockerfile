#!/bin/sh
FROM ubuntu:latest
MAINTAINER NupurDave <ncd2123@columbia.edu>

RUN apt-get update
RUN apt-get -y install curl
RUN curl -L nupurd89.github.io/folder/tslsynth --output tslsynth
RUN apt-get install -y gnupg
RUN apt-get install wget
RUN wget http://www.lrde.epita.fr/dload/spot/spot-2.10.4.tar.gz
RUN apt-get install unzip
RUN tar -xvzf spot-2.10.4.tar.gz
#RUN ls /spot-2.10.4
RUN apt-get -y install build-essential
RUN apt-get install -y build-essential python3.6 python3-pip 
RUN cd spot-2.10.4 && ./configure --prefix ~/usr
RUN cd spot-2.10.4 && make
RUN cd spot-2.10.4 && make install
#RUN wget -O /etc/apt/trusted.gpg.d/lrde.gpg https://www.lrde.epita.fr/repo/debian.gpg
#RUN echo 'deb http://www.lrde.epita.fr/repo/debian/ unstable/' >> /etc/apt/sources.list
#RUN cat /etc/apt/sources.list
#RUN apt-get install spot
#RUN chmod +x /tslsynth
COPY ./tsltools/lambda/index.js .
COPY ./tsltools/sample.tsl .
