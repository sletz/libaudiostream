# Find libsamplerate

find_path(SAMPLERATE_INCLUDE_DIR samplerate.h
    HINTS ${SAMPLERATE_INCLUDE_DIR_HINT})

set(SAMPLERATE_NAMES ${SAMPLERATE_NAMES} samplerate libsamplerate)
find_library(SAMPLERATE_LIBRARY NAMES ${SAMPLERATE_NAMES}
    HINTS ${SAMPLERATE_LIB_DIR_HINT})

if(SAMPLERATE_INCLUDE_DIR AND SAMPLERATE_LIBRARY)
        set(SAMPLERATE_FOUND TRUE)
endif()

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(Samplerate DEFAULT_MSG
        SAMPLERATE_LIBRARY SAMPLERATE_INCLUDE_DIR)

if(SAMPLERATE_FOUND)
    set(SAMPLERATE_LIBRARIES ${SAMPLERATE_LIBRARY})
else()
    set(SAMPLERATE_LIBRARIES)
endif()