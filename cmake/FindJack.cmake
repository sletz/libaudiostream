# - Try to find jack-2.6
# Once done this will define
#
#  JACK_FOUND - system has jack
#  JACK_INCLUDE_DIRS - the jack include directory
#  JACK_LIBRARIES - Link these to use jack
#  JACK_DEFINITIONS - Compiler switches required for using jack
#
#  Copyright (c) 2008 Andreas Schneider <mail@cynapses.org>
#  Modified for other libraries by Lasse Kärkkäinen <tronic>
#  Simplified to more modern CMake style by Jean-Michaël Celerier <jeanmichael.celerier@gmail.com>
#
#  Redistribution and use is allowed according to the terms of the New
#  BSD license.
#  For details see the accompanying COPYING-CMAKE-SCRIPTS file.
#

find_package(PkgConfig)
if (PKG_CONFIG_FOUND)
  pkg_check_modules(_JACK jack)
endif()

find_path(JACK_INCLUDE_DIR
  NAMES
    jack/jack.h
  PATHS
    ${_JACK_INCLUDEDIR}
    /usr/include
    /usr/local/include
    /opt/local/include
    /sw/include
)

find_library(JACK_LIBRARY
  NAMES
    jack
  PATHS
    ${_JACK_LIBDIR}
    /usr/lib
    /usr/local/lib
    /opt/local/lib
    /sw/lib
)

find_library(NETJACK_LIBRARY
  NAMES
    jacknet
  PATHS
    ${_JACK_LIBDIR}
    /usr/lib
    /usr/local/lib
    /opt/local/lib
    /sw/lib
)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(JACK  DEFAULT_MSG  JACK_LIBRARY  JACK_INCLUDE_DIR)

if(JACK_FOUND)
  set(JACK_INCLUDE_DIRS ${JACK_INCLUDE_DIR})
  set(JACK_LIBRARIES ${JACK_LIBRARIES} ${JACK_LIBRARY})

  if(NETJACK_LIBRARY)
    set(JACK_LIBRARIES ${JACK_LIBRARIES} ${NETJACK_LIBRARY})
    set(JACK_NETJACK 1 CACHE INTERNAL "" FORCE)
  else()
    set(JACK_NETJACK 0 CACHE INTERNAL "" FORCE)
  endif()
endif()

mark_as_advanced(JACK_INCLUDE_DIRS JACK_LIBRARIES)
