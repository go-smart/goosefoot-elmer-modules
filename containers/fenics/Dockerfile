FROM fenicsproject/stable-ppa:latest

RUN apt-get update && \
    apt-get -qqy install python3-pip libyaml-dev python-yaml && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN pip3 install hachiko

COPY gosmart_module /usr/local/lib/python2.7/dist-packages/gosmart
COPY init.sh /
