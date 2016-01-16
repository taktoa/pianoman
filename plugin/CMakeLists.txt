cmake_minimum_required (VERSION 2.8.11)
project(TEAMSPEAK_ZEROMQ_PLUGIN)

add_subdirectory(doc)

find_package(PkgConfig REQUIRED)
find_package(Boost     REQUIRED program_options thread)

pkg_check_modules(ZMQ REQUIRED libzmq)
pkg_check_modules(MSG REQUIRED msgpack)

set(CMAKE_C_FLAGS
  "${CMAKE_C_FLAGS} -g -std=c11 -Werror")
set(CMAKE_CXX_FLAGS
  "${CMAKE_CXX_FLAGS} -g -std=c++14 -Werror")

include_directories(
  ${CMAKE_SOURCE_DIR}/include
  ${Boost_INCLUDE_DIRS}
  ${ZMQ_INCLUDE_DIRS}
  ${MSG_INCLUDE_DIRS})

link_directories(
  ${Boost_LIBRARY_DIRS}
  ${ZMQ_LIBRARY_DIRS}
  ${MSG_LIBRARY_DIRS})

add_definitions(
  ${Boost_CFLAGS_OTHER}
  ${ZMQ_CFLAGS_OTHER}
  ${MSG_CFLAGS_OTHER})

file(GLOB_RECURSE PLUGIN_C_SOURCES src/*.c)
file(GLOB_RECURSE PLUGIN_C_HEADERS src/*.h)
file(GLOB_RECURSE PLUGIN_CXX_SOURCES src/*.cpp)
file(GLOB_RECURSE PLUGIN_CXX_HEADERS src/*.hpp)

add_library(teamspeak-zeromq-plugin SHARED
  ${PLUGIN_C_SOURCES}
  ${PLUGIN_C_HEADERS}
  ${PLUGIN_CXX_SOURCES}
  ${PLUGIN_CXX_HEADERS})

install(TARGETS teamspeak-zeromq-plugin
        RUNTIME DESTINATION bin
        LIBRARY DESTINATION lib/teamspeak/plugins)
