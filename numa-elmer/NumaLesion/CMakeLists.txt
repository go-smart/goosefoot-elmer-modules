ADD_DEFINITIONS(-Wall -fmax-errors=0)
ADD_LIBRARY(numa-lesion SHARED LesionExtraction.f90)
INSTALL(TARGETS numa-lesion LIBRARY DESTINATION "share/elmersolver/lib")
