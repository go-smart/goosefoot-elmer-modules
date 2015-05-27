EXTRA_ARGS=$@

cmake \
  -DCMAKE_BUILD_TYPE:STRING=DEBUG \
  -DTrilinos_ENABLE_TESTS:BOOL=ON \
  -DTPL_ENABLE_CUDA:BOOL=on \
  -DTPL_ENABLE_Thrust:BOOL=ON \
  -DTPL_Thrust_INCLUDE_DIRS=/usr/local/cuda-5.5/include/thrust \
  -DTrilinos_ENABLE_DEVELOPMENT_MODE:BOOL=OFF \
  -DTrilinos_ENABLE_Epetra=ON \
  -DTrilinos_ENABLE_ALL_OPTIONAL_PACKAGES:BOOL=ON \
  -DTPL_ENABLE_MPI:BOOL=ON \
  -DBUILD_SHARED_LIBS:BOOL=ON \
  -DCMAKE_INSTALL_PREFIX=${HOME}/.local \
  -DTrilinos_ENABLE_Teuchos:BOOL=ON \
  -DTrilinos_ENABLE_Belos:BOOL=ON \
  -DTrilinos_ENABLE_Kokkos:BOOL=ON \
  -DTrilinos_ENABLE_KokkosClassic:BOOL=ON \
  -DKokkosClassic_ENABLE_NodeAPI:BOOL=ON \
  -DKokkosClassic_ENABLE_TESTS:BOOL=OFF \
  -DKokkosClassic_ENABLE_TSQR:BOOL=ON \
  -DTrilinos_ENABLE_KokkosArray:BOOL=OFF \
  -DTrilinos_ENABLE_Ifpack2:BOOL=ON \
  -DKokkosClassic_ENABLE_Cusp:BOOL=OFF \
  -DKokkosClassic_ENABLE_CUSPARSE:BOOL=ON \
  -DCUDA_TOOLKIT_ROOT_DIR=/usr/local/cuda-5.5 \
  $EXTRA_ARGS \
  ${SOURCE_BASE}
