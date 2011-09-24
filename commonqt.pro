unix:TEMPLATE     = lib
win32:TEMPLATE    = vclib

HEADERS     += commonqt.h
SOURCES     += commonqt.cpp
CONFIG      += qt thread debug dll
target.path =  $$[QT_INSTALL_LIBS]
INSTALLS    += target

unix:LIBS += -lsmokeqtcore
win32:LIBS += smokeqtcore.lib
