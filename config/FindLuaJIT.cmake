# Locate LuaJIT library
# This module defines
#  LUAJUT_FOUND, if false, do not try to link to Lua
#  LUAJIT_LIBRARIES

FIND_LIBRARY(LUAJIT_LIBRARY
  NAMES luajit-5.1
  HINTS
  $ENV{LUAJIT_DIR}
  PATH_SUFFIXES lib64 lib
  PATHS
  ~/Library/Frameworks
  /Library/Frameworks
  /usr/local
  /usr
  /sw
  /opt/local
  /opt/csw
  /opt
)

IF(LUAJIT_LIBRARY)
  IF(UNIX AND NOT APPLE)
    FIND_LIBRARY(LUAJIT_MATH_LIBRARY m)
    SET( LUAJIT_LIBRARIES "${LUAJIT_LIBRARY};${LUAJIT_MATH_LIBRARY}" CACHE STRING "LuaJIT Libraries")
  ENDIF()
ENDIF()

INCLUDE(FindPackageHandleStandardArgs)
# handle the QUIETLY and REQUIRED arguments and set LUAJIT_FOUND to TRUE if
# all listed variables are TRUE
FIND_PACKAGE_HANDLE_STANDARD_ARGS(LUAJIT DEFAULT_MSG LUAJIT_LIBRARIES)

MARK_AS_ADVANCED(LUAJIT_LIBRARIES LUAJIT_LIBRARY LUAJIT_MATH_LIBRARY)
