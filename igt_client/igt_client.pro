# -------------------------------------------------
# Project created by QtCreator 2011-12-04T13:01:56
# -------------------------------------------------
QT += network \
    opengl
TARGET = igt_client
TEMPLATE = app
LIBS += -lml \
    -lcvaux \
    -lcv \
    -lcxcore
INCLUDEPATH += /usr/include/opencv/
SOURCES += main.cpp \
    mainwindow.cpp \
    clientthread.cpp \
    igtscreen.cpp \
    igtsubject.cpp
HEADERS += mainwindow.h \
    clientthread.h \
    igtscreen.h \
    igtsubject.h
FORMS += mainwindow.ui \
    igtscreen.ui
