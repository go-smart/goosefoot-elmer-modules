

ADD_DEFINITIONS(-Wall -fmax-errors=0)
FILE(GLOB f90s *.f90)
ADD_LIBRARY(numa-heatsolver SHARED ${f90s})
INSTALL(TARGETS numa-heatsolver LIBRARY DESTINATION "share/elmersolver/lib")
