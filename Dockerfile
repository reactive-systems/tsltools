#!/bin/sh
FROM ubuntu:latest AS ubuntu-latest
MAINTAINER NupurDave <ncd2123@columbia.edu>
ARG FUNCTION_DIR="/home/app/"
#RUN apt-get update
RUN apt-get install libstdc++6
#RUN apt-get install -y build-essential
RUN apt-get install -y\
    libtool \
    autoconf \
    automake \
    libexecinfo-dev \
    make \
    cmake \
    libcurl

ARG FUNCTION_DIR
RUN mkdir -p ${FUNCTION_DIR}
#bottom line might have a problem
COPY app/* ${FUNCTION_DIR}
RUN python3 -m pip install awslambdaric --target ${FUNCTION_DIR}

#final runtime image
FROM ubuntu-latest
ARG FUNCTION_DIR
WORKDIR ${FUNCTION_DIR}
COPY --from=build-image ${FUNCTION_DIR} ${FUNCTION_DIR}
COPY entry.sh /
RUN chmod 755 /usr/bin/aws-lambda-rie /entry.sh
ENTRYPOINT [ "/entry.sh" ]
CMD [ "app.handler"] 


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
RUN chmod +x /tslsynth
COPY ./lambda/index.js .
COPY ./sample.tsl .
RUN ./tslsynth --python sample.tsl
