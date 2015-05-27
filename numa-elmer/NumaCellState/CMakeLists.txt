

ADD_DEFINITIONS(-Wall -fmax-errors=0)
FILE(GLOB f90s *.f90)
ADD_LIBRARY(numa-cellstate SHARED ${f90s})
INSTALL(TARGETS numa-cellstate LIBRARY DESTINATION "share/elmersolver/lib")
