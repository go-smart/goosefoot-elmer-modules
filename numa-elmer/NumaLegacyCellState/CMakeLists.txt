

ADD_DEFINITIONS(-Wall -fmax-errors=0)
FILE(GLOB f90s *.f90)
ADD_LIBRARY(numa-legacy-cellstate SHARED ${f90s})
INSTALL(TARGETS numa-legacy-cellstate LIBRARY DESTINATION "share/elmersolver/lib")
