

ADD_DEFINITIONS(-Wall -fmax-errors=0)
FILE(GLOB f90s *.f90)
ADD_LIBRARY(numa-powerfield SHARED ${f90s})
INSTALL(TARGETS numa-powerfield LIBRARY DESTINATION "share/elmersolver/lib")
