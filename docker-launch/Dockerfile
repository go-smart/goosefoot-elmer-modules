FROM fenicsproject/stable-ppa:latest

RUN apt-get update && \
    apt-get -qqy install python3-pip && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN pip3 install hachiko
