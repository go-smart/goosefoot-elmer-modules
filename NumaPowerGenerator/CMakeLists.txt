

ADD_DEFINITIONS(-Wall -fmax-errors=0)
FILE(GLOB f90s *.f90)
ADD_LIBRARY(numa-powergenerator SHARED ${f90s})
INSTALL(TARGETS numa-powergenerator LIBRARY DESTINATION "share/elmersolver/lib")
