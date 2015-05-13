

ADD_DEFINITIONS(-Wall -fmax-errors=0)
FILE(GLOB f90s *.f90)
ADD_LIBRARY(numa-progress SHARED ${f90s} err.c)
INSTALL(TARGETS numa-progress LIBRARY DESTINATION "share/elmersolver/lib")
