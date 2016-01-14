

ADD_DEFINITIONS(-Wall -fmax-errors=0)
FILE(GLOB f90s *.f90)
ADD_LIBRARY(numa-legacy-heatsolver SHARED ${f90s})
INSTALL(TARGETS numa-legacy-heatsolver LIBRARY DESTINATION "share/elmersolver/lib")
