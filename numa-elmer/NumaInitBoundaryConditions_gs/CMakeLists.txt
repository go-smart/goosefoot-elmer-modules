

ADD_DEFINITIONS(-Wall -fmax-errors=0)
FILE(GLOB f90s *.f90)
ADD_LIBRARY(numa-initboundaryconditions_gs SHARED ${f90s})
INSTALL(TARGETS numa-initboundaryconditions_gs LIBRARY DESTINATION "share/elmersolver/lib")
