FROM qnib/openfoam:u1410of231

RUN apt-get install -y cmake

COPY galilfoam /opt

WORKDIR "/opt"

RUN ./configure
