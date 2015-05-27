ADD_DEFINITIONS(-Wall -fmax-errors=0)
ADD_LIBRARY(numa-alternatebc SHARED AlternatingBCSolver.f90)
INSTALL(TARGETS numa-alternatebc LIBRARY DESTINATION "share/elmersolver/lib")
