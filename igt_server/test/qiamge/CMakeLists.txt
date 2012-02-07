cmake_minimum_required(VERSION 2.8)
project(qimage_on_net)
find_package(Qt4 REQUIRED)

set(S_SRCS qImageSender.cpp)
set(R_SRCS qImageRecv.cpp)
# set(igt_server_HEADERS webcam_stream.h)
include(${QT_USE_FILE})
add_definitions(${QT_DEFINITIONS})

add_executable(qimage_on_net_s ${S_SRCS})
add_executable(qimage_on_net_r ${R_SRCS})

target_link_libraries(qimage_on_net_r ${QT_LIBRARIES})
target_link_libraries(qimage_on_net_s ${QT_LIBRARIES})

