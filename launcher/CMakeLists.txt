# With thanks to: http://bloerg.net/2012/11/10/cmake-and-distutils.html

# Originally, certain libraries were not available as Python3 (PythonOCC, in particular),
# so we need to have a Python2 component
find_program(PYTHON2 "python2")
find_program(PYTHON3 "python3")

# TODO: error out (sensibly & consistently) if no Py3: the user is not going to get much further
if (PYTHON3)
    # The CMake template for setup.py - effectively, this allows us to include CMake variables in
    # our setup.py, such as the target directory, source git revision, etc.
    set(SETUP_PY_IN "${CMAKE_CURRENT_SOURCE_DIR}/setup.py.in")
    set(SETUP_PY    "${CMAKE_CURRENT_BINARY_DIR}/setup.py")
    set(DEPS        "${CMAKE_CURRENT_SOURCE_DIR}/scripts/go-smart-launcher")

    # This timestamp allows us to check whether a re-run is needed, based on the setup.py.in
    # modification time
    set(OUTPUT      "${CMAKE_CURRENT_BINARY_DIR}/build/timestamp")

    # Connect the input setup.py(.in) to the output
    configure_file(${SETUP_PY_IN} ${SETUP_PY})

    # Add the command that (triggers processing of and) runs setup.py [build]
    add_custom_command(OUTPUT ${OUTPUT}
                       COMMAND ${PYTHON3} ${SETUP_PY} build
                       COMMAND ${CMAKE_COMMAND} -E touch ${OUTPUT}
                       DEPENDS ${DEPS})

    # Add this new command as a target for ALL
    add_custom_target(target ALL DEPENDS ${OUTPUT} ${CMAKE_CURRENT_SOURCE_DIR}/conf/vigilant.cfg)

    # On install, run setup.py [install]
    install(CODE "execute_process(COMMAND ${PYTHON3} ${SETUP_PY} install --prefix=${CMAKE_INSTALL_PREFIX})")
    install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/conf/vigilant.cfg DESTINATION ${ETC_LOCATION} COMPONENT config)
endif()

# TODO: at least provide a useful warning or error out if truly essential
if (PYTHON2)
    # As in the Python3 case
    set(SETUP_PY_IN "${CMAKE_CURRENT_SOURCE_DIR}/setup2.py.in")
    set(SETUP_PY    "${CMAKE_CURRENT_BINARY_DIR}/setup2.py")
    set(DEPS        "${CMAKE_CURRENT_SOURCE_DIR}/scripts2/go-smart-preprocessor")
    set(OUTPUT      "${CMAKE_CURRENT_BINARY_DIR}/build/timestamp2")

    configure_file(${SETUP_PY_IN} ${SETUP_PY})

    add_custom_command(OUTPUT ${OUTPUT}
                       COMMAND ${PYTHON2} ${SETUP_PY} build
                       COMMAND ${CMAKE_COMMAND} -E touch ${OUTPUT}
                       DEPENDS ${DEPS})

    # Different target name required than for Python3, still added to ALL
    add_custom_target(target2 ALL DEPENDS ${OUTPUT})

    install(CODE "execute_process(COMMAND ${PYTHON2} ${SETUP_PY} install --prefix=${CMAKE_INSTALL_PREFIX})")
endif()
