# Find librubberband

find_path(RUBBERBAND_INCLUDE_DIR rubberband/rubberband-c.h
    HINTS ${RUBBERBAND_INCLUDE_DIR_HINT})

set(RUBBERBAND_NAMES ${RUBBERBAND_NAMES} rubberband librubberband)
find_library(RUBBERBAND_LIBRARY NAMES ${RUBBERBAND_NAMES}
    HINTS ${SAMPLERATE_LIB_DIR_HINT})

if(RUBBERBAND_INCLUDE_DIR AND RUBBERBAND_LIBRARY)
        set(RUBBERBAND_FOUND TRUE)
endif()

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(Rubberband DEFAULT_MSG
        RUBBERBAND_LIBRARY RUBBERBAND_INCLUDE_DIR)

if(RUBBERBAND_FOUND)
    set(RUBBERBAND_LIBRARIES ${RUBBERBAND_LIBRARY})
else()
    set(RUBBERBAND_LIBRARIES)
endif()
