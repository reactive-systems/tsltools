#!/bin/sh
FROM ubuntu:latest
MAINTAINER NupurDave <ncd2123@columbia.edu>

RUN apt-get update
RUN apt-get -y install curl
RUN curl -L nupurd89.github.io/folder/tslsynth --output tslsynth
RUN apt-get install -y gnupg
RUN apt-get install wget
RUN wget -O /etc/apt/trusted.gpg.d/lrde.gpg https://www.lrde.epita.fr/repo/debian.gpg
RUN echo 'deb http://www.lrde.epita.fr/repo/debian/ unstable/' >> /etc/apt/sources.list
RUN cat /etc/apt/sources.list
RUN apt-get update
RUN apt-get install spot
#RUN chmod +x /tslsynth
#COPY ./tsltools/lambda/index.js .
#COPY ./tsltools/sample.tsl .
