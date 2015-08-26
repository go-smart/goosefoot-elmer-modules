FROM qnib/openfoam:u1410of231

RUN apt-get install -y cmake

COPY gfoam /opt

WORKDIR "/opt"

RUN ./configure

COPY init.sh /
