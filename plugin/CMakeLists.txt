cmake_minimum_required (VERSION 2.8.11)
project(TEAMSPEAK_ZEROMQ_PLUGIN)

add_subdirectory(doc)

find_package(PkgConfig REQUIRED)
find_package(Boost     REQUIRED program_options thread)

pkg_check_modules(ZMQ REQUIRED libzmq)
pkg_check_modules(MSG REQUIRED msgpack)
pkg_check_modules(QT5 REQUIRED Qt5Gui)

set(CMAKE_AUTOMOC ON)

find_package(Qt5Gui)

set(CMAKE_C_FLAGS
  "${CMAKE_C_FLAGS} -g -std=c11 -Werror"
)

set(CMAKE_CXX_FLAGS
  "${CMAKE_CXX_FLAGS} -g -std=c++14 -Werror"
)

include_directories(
  ${CMAKE_SOURCE_DIR}/include
  ${QT5_INCLUDE_DIRS}
  ${Boost_INCLUDE_DIRS}
  ${ZMQ_INCLUDE_DIRS}
  ${MSG_INCLUDE_DIRS}
)

link_directories(
  ${Boost_LIBRARY_DIRS}
  ${ZMQ_LIBRARY_DIRS}
  ${MSG_LIBRARY_DIRS}
)

add_definitions(
  ${QT5_CFLAGS_OTHER}
  ${Boost_CFLAGS_OTHER}
  ${ZMQ_CFLAGS_OTHER}
  ${MSG_CFLAGS_OTHER}
)

set(GENERAL_LIBRARIES
  ${Boost_LIBRARIES}
  ${ZMQ_LIBRARIES}
  ${QT5_LIBRARIES}
)

set(PLUGIN_SOURCES
  src/plugin.cpp
  src/rpc.cpp
)

add_library(teamspeak-zeromq-plugin SHARED ${PLUGIN_SOURCES})
target_link_libraries(teamspeak-zeromq-plugin ${GENERAL_LIBRARIES})

add_executable(simple_test src/simple_test.cpp ${PLUGIN_SOURCES})
target_link_libraries(simple_test ${GENERAL_LIBRARIES})

add_executable(integration_test src/integration_test.cpp ${PLUGIN_SOURCES})
target_link_libraries(integration_test ${GENERAL_LIBRARIES})

install(
  TARGETS teamspeak-zeromq-plugin
  RUNTIME DESTINATION bin
  LIBRARY DESTINATION lib/teamspeak/plugins
)
