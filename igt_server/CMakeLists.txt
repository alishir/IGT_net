# Good CMake Tutorial
# http://cor-net.org/hello-world-in-qt-using-cmake-and-openembedded/

cmake_minimum_required(VERSION 2.8)
set(CMAKE_BUILD_TYPE Debug)
set(CMAKE_CXX_FLAGS "-g -Wall")

project(igt_server)
find_package(Qt4 COMPONENTS QtCore QtGui QtNetwork REQUIRED)
# find_package(Qt4 REQUIRED)
find_package(OpenCV REQUIRED)
find_package(Threads REQUIRED)

set(OpenCVHeaders "/home/ali/dl/opencv/OpenCV-2.3.1/modules/highgui/include/")
set(OpenCVHeaders1 "/home/ali/dl/opencv/OpenCV-2.3.1/include/")
include_directories("${OpenCVHeaders} ${OpenCVHeaders1}")

set(IGT_SRV_QT4_SRC sender.h framegrabber.h scoresender.h)
QT4_WRAP_CPP(IGT_SRV_QT4_MOC_CPP ${IGT_SRV_QT4_SRC})

set(igt_server_SOURCES webcam_stream.cpp sender.cpp framegrabber.cpp scoresender.cpp ${IGT_SRV_QT4_MOC_CPP})
# set(igt_server_HEADERS sender.h)

include(${QT_USE_FILE})
add_definitions(${QT_DEFINITIONS})

add_executable(igt_server ${igt_server_SOURCES})
target_link_libraries(igt_server ${QT_LIBRARIES} ${OpenCV_LIBS})

