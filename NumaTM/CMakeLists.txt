

ADD_DEFINITIONS(-Wall -fmax-errors=0)
FILE(GLOB f90s *.f90)
ADD_LIBRARY(numa-tm SHARED ${f90s})
INSTALL(TARGETS numa-tm LIBRARY DESTINATION "share/elmersolver/lib")
