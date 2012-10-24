# Locate MKL library
# This module defines
#  MKL_FOUND, if false, do not try to link to Lua
#  MKL_LIBRARIES
#  MKL_INCLUDE_DIRS

FIND_LIBRARY(MKL_CORE_LIBRARY
  NAMES mkl_core
  HINTS
  "$ENV{MKLROOT}/lib/intel64/"
  )

set(MKLROOT $ENV{MKLROOT})

IF(MKL_CORE_LIBRARY)
  IF(UNIX AND NOT APPLE)
    set(MKL_LIBRARIES "mkl_lapack95_lp64"; "mkl_intel_lp64"; "mkl_sequential"; "mkl_core"; "pthread"; "m")
    set(MKL_INCLUDE_DIRS "${MKLROOT}/include"; "${MKLROOT}/include/intel64/lp64")
  ENDIF()
ENDIF()

INCLUDE(FindPackageHandleStandardArgs)
# handle the QUIETLY and REQUIRED arguments and set MKL_FOUND to TRUE if
# all listed variables are TRUE
FIND_PACKAGE_HANDLE_STANDARD_ARGS(MKL DEFAULT_MSG MKL_LIBRARIES)

MARK_AS_ADVANCED(MKL_LIBRARIES MKL_LIBRARY)
