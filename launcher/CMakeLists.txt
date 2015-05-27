# With thanks to: http://bloerg.net/2012/11/10/cmake-and-distutils.html

find_program(PYTHON2 "python2")
find_program(PYTHON3 "python3")

if (PYTHON3)
    set(SETUP_PY_IN "${CMAKE_CURRENT_SOURCE_DIR}/setup.py.in")
    set(SETUP_PY    "${CMAKE_CURRENT_BINARY_DIR}/setup.py")
    set(DEPS        "${CMAKE_CURRENT_SOURCE_DIR}/scripts/go-smart-launcher")
    set(OUTPUT      "${CMAKE_CURRENT_BINARY_DIR}/build/timestamp")

    configure_file(${SETUP_PY_IN} ${SETUP_PY})

    add_custom_command(OUTPUT ${OUTPUT}
                       COMMAND ${PYTHON3} ${SETUP_PY} build
                       COMMAND ${CMAKE_COMMAND} -E touch ${OUTPUT}
                       DEPENDS ${DEPS})

    add_custom_target(target ALL DEPENDS ${OUTPUT})

    install(CODE "execute_process(COMMAND ${PYTHON3} ${SETUP_PY} install --prefix=${CMAKE_INSTALL_PREFIX})")
endif()

if (PYTHON2)
    set(SETUP_PY_IN "${CMAKE_CURRENT_SOURCE_DIR}/setup2.py.in")
    set(SETUP_PY    "${CMAKE_CURRENT_BINARY_DIR}/setup2.py")
    set(DEPS        "${CMAKE_CURRENT_SOURCE_DIR}/scripts2/go-smart-preprocessor")
    set(OUTPUT      "${CMAKE_CURRENT_BINARY_DIR}/build/timestamp2")

    configure_file(${SETUP_PY_IN} ${SETUP_PY})

    add_custom_command(OUTPUT ${OUTPUT}
                       COMMAND ${PYTHON2} ${SETUP_PY} build
                       COMMAND ${CMAKE_COMMAND} -E touch ${OUTPUT}
                       DEPENDS ${DEPS})

    add_custom_target(target2 ALL DEPENDS ${OUTPUT})

    install(CODE "execute_process(COMMAND ${PYTHON2} ${SETUP_PY} install --prefix=${CMAKE_INSTALL_PREFIX})")
endif()
