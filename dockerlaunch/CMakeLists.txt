find_program(PYTHON3 "python3")

set(SETUP_PY_IN "${CMAKE_CURRENT_SOURCE_DIR}/setup.py.in")
set(SETUP_PY    "${CMAKE_CURRENT_BINARY_DIR}/setup.py")
set(DEPS        "${CMAKE_CURRENT_SOURCE_DIR}/scripts/dockerlaunchd")
set(OUTPUT      "${CMAKE_CURRENT_BINARY_DIR}/build/timestamp")
set(OUTPUT2     "${CMAKE_CURRENT_BINARY_DIR}/build/timestamp2")

configure_file(${SETUP_PY_IN} ${SETUP_PY})

add_custom_command(OUTPUT ${OUTPUT}
                   COMMAND ${PYTHON3} ${SETUP_PY} build
                   COMMAND ${CMAKE_COMMAND} -E touch ${OUTPUT}
                   DEPENDS ${DEPS})

add_custom_command(OUTPUT ${OUTPUT2}
                   COMMAND ${PYTHON3} ${SETUP_PY} dockerlaunch_setup
                   COMMAND ${CMAKE_COMMAND} -E touch ${OUTPUT2}
                   DEPENDS ${DEPS})

add_custom_target(dockerlaunch ALL DEPENDS ${OUTPUT})
add_custom_target(dockerlaunch_setup DEPENDS ${OUTPUT2})

install(CODE "execute_process(COMMAND ${PYTHON3} ${SETUP_PY} install --prefix=${CMAKE_INSTALL_PREFIX})")
