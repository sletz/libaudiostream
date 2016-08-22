# Configuration
cmake_minimum_required(VERSION 3.4 FATAL_ERROR)
set(CMAKE_MODULE_PATH
  "${CMAKE_MODULE_PATH}"
  "${CMAKE_CURRENT_SOURCE_DIR}/cmake"
)

option(LIBAUDIOSTREAM_SHARED "Build a shared library" OFF)
option(LIBAUDIOSTREAM_SAMPLES "Build examples" OFF)
option(LIBAUDIOSTREAM_TESTS "Build tests" OFF)
if(APPLE)
    option(LIBAUDIOSTREAM_FRAMEWORK "Build an OS X framework" ON)
    option(LIBAUDIOSTREAM_UNIVERSAL "Build universal binaries" OFF)

    if(LIBAUDIOSTREAM_UNIVERSAL)
        set(CMAKE_OSX_ARCHITECTURES "i386;x86_64")
    endif()
endif()

project(libaudiostream
    VERSION 2.0.3
    LANGUAGES C CXX)


if(UNIX AND NOT APPLE)
    find_package(DL REQUIRED)
endif()

# Sources
set(CSRC
#src/atomic/lffifo.c
#src/atomic/lflifo.c

src/atomic/lffifoIntel.c
src/atomic/lflifoIntel.c

# src/atomic/lffifoppc.c
# src/atomic/lflifoppc.c
# src/sorter/msSorter.c
)

set(CPPSRC
src/Envelope.cpp
src/la_smartpointer.cpp
src/Object.cpp

src/StringTools.cpp
src/TAdapterAudioStream.cpp
src/TBufferedAudioStream.cpp
src/TCutEndAudioStream.cpp
src/TEffectAudioStream.cpp
src/TEventAudioStream.cpp
src/TFadeAudioStream.cpp
src/TFileAudioStream.cpp
src/TLoopAudioStream.cpp
src/TMixAudioStream.cpp
src/TParAudioStream.cpp
src/TRendererAudioStream.cpp
src/TSelectAudioStream.cpp
src/TSeqAudioStream.cpp
src/TThreadCmdManager.cpp
src/renderer/TAudioRenderer.cpp
src/renderer/TOfflineRenderer.cpp

# These files require Faust to build
src/LibAudioStreamMC++.h
src/LibAudioStreamMC.cpp
src/TAudioGlobals.cpp
src/TAudioRendererFactory.cpp
src/TAudioStreamFactory.cpp
src/TExpAudioEngine.cpp
src/TExpAudioMixer.cpp
)

# Other files :
# src/FaustReader.cpp

# src/LibAudioStream.cpp

# src/TAudioEngine.cpp
# src/TAudioMixer.cpp

# src/TTransformAudioStream.cpp
# src/TSoundTouchAudioStream.cpp
# src/TAudioChannel.cpp
# src/TAudioEffect.cpp

# src/LAS-test.cpp
# src/LAS-tester.cpp
# src/tester1.cpp
# src/tester2.cpp
# src/tester3.cpp
# src/tester.cpp

# Renderers
set(COREAUDIO_RENDERER src/renderer/TCoreAudioRenderer.cpp)
set(PA18_RENDERER src/renderer/TPortAudioRenderer.cpp)
set(PA19_RENDERER src/renderer/TPortAudioV19Renderer.cpp)
set(JACK_RENDERER
src/renderer/TJackRenderer.cpp
src/renderer/TAudioRenderer.cpp
src/renderer/TNetJackRenderer.cpp
)

# Streams
set(RUBBERBAND_STREAM
src/TRubberBandAudioStream.cpp
)
set(SAMPLERATE_STREAM
src/TSampleRateAudioStream.cpp
)
set(FILE_STREAM
src/TReadFileAudioStream.cpp
src/TWriteFileAudioStream.cpp
)
set(FAUST_STREAM
src/TFaustAudioEffect.cpp
)

# Dependencies
find_package(Threads REQUIRED)
find_package(Jack)
find_package(Portaudio)
find_package(Sndfile REQUIRED)
find_package(Samplerate REQUIRED)
find_package(Rubberband REQUIRED)
find_package(Faust REQUIRED)

if(APPLE AND LIBAUDIOSTREAM_FRAMEWORK)
    add_library(LibAudioStream SHARED ${CSRC} ${CPPSRC})
    set_target_properties(LibAudioStream PROPERTIES 
        FRAMEWORK TRUE
        FRAMEWORK_VERSION A
        MACOSX_FRAMEWORK_INFO_PLIST ${CMAKE_CURRENT_SOURCE_DIR}/macosx/Info-LibAudioStreamPA.plist
        VERSION ${PROJECT_VERSION}
        SOVERSION 2.0.0
        PUBLIC_HEADER src/LibAudioStreamMC++.h)
else()
    if(LIBAUDIOSTREAM_SHARED)
        add_library(LibAudioStream SHARED ${CSRC} ${CPPSRC})
    else()
        add_library(LibAudioStream ${CSRC} ${CPPSRC})
    endif()
endif()

set_target_properties(LibAudioStream PROPERTIES PREFIX "")

if(WIN32)
    target_compile_definitions(LibAudioStream
        PUBLIC WIN32
        PRIVATE NOMINMAX _CRT_SECURE_NO_WARNINGS)
    set_target_properties(LibAudioStream
        PROPERTIES
          WINDOWS_EXPORT_ALL_SYMBOLS 1)
endif()
target_compile_definitions(LibAudioStream
    PRIVATE LIBAUDIOSTREAM_EXPORTS)

set_target_properties(
    LibAudioStream
    PROPERTIES
        CXX_STANDARD 14)
target_include_directories(LibAudioStream PRIVATE
  src
  src/atomic
  src/renderer
)
target_include_directories(LibAudioStream SYSTEM INTERFACE
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/atomic>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/renderer>

    $<INSTALL_INTERFACE:include>
)

target_link_libraries(LibAudioStream PUBLIC ${Threads_LIBRARIES})
# Dependencies
# OS X
if(APPLE)
  target_sources(LibAudioStream PRIVATE ${COREAUDIO_RENDERER})

  find_library(COREFOUNDATION_LIBRARY CoreFoundation)
  find_library(COREAUDIO_LIBRARY CoreAudio)
  find_library(CORESERVICES_LIBRARY CoreServices)
  find_library(AUDIOUNIT_LIBRARY AudioUnit)
  find_library(ACCELERATE_LIBRARY Accelerate)
  find_library(AUDIOTOOLBOX_LIBRARY AudioToolbox)
  target_link_libraries(LibAudioStream PUBLIC ${COREFOUNDATION_LIBRARY} ${COREAUDIO_LIBRARY} ${CORESERVICES_LIBRARY} ${AUDIOUNIT_LIBRARY} ${ACCELERATE_LIBRARY} ${AUDIOTOOLBOX_LIBRARY})
  target_compile_definitions(LibAudioStream PRIVATE
      __COREAUDIO__)
endif()

# Linux
if(UNIX AND NOT APPLE)
    target_link_libraries(LibAudioStream PUBLIC ${LIBDL_LIBRARIES})
endif()

# Jack
if(${JACK_FOUND})
  target_sources(LibAudioStream PRIVATE ${JACK_RENDERER})

  target_compile_definitions(LibAudioStream PRIVATE
      ${JACK_DEFINITIONS}
      __JACK__)
  target_link_libraries(LibAudioStream PUBLIC ${JACK_LIBRARIES})
  target_include_directories(LibAudioStream PRIVATE ${JACK_INCLUDE_DIRS})
endif()

# Portaudio
if(${PORTAUDIO_FOUND})
  message("${PORTAUDIO_VERSION}")
  if("${PORTAUDIO_VERSION}" EQUAL 18)
    target_sources(LibAudioStream PRIVATE ${PA18_RENDERER})
  elseif("${PORTAUDIO_VERSION}" EQUAL 19)
    target_sources(LibAudioStream PRIVATE ${PA19_RENDERER})
    target_compile_definitions(LibAudioStream PRIVATE __PORTAUDIOV19__)
  endif()

  target_compile_definitions(LibAudioStream PRIVATE
      ${PORTAUDIO_DEFINITIONS}
      __PORTAUDIO__)
  target_link_libraries(LibAudioStream PUBLIC ${PORTAUDIO_LIBRARIES})
  target_include_directories(LibAudioStream PRIVATE ${PORTAUDIO_INCLUDE_DIRS})
endif()

# Sndfile
if(${SNDFILE_FOUND})
  target_sources(LibAudioStream PRIVATE ${FILE_STREAM})

  target_link_libraries(LibAudioStream PUBLIC ${SNDFILE_LIBRARIES})
  target_include_directories(LibAudioStream PRIVATE ${SNDFILE_INCLUDE_DIR})
endif()

# Samplerate
if(${SAMPLERATE_FOUND})
  target_sources(LibAudioStream PRIVATE ${SAMPLERATE_STREAM})

  target_link_libraries(LibAudioStream PUBLIC ${SAMPLERATE_LIBRARY})
  target_include_directories(LibAudioStream PRIVATE ${SAMPLERATE_INCLUDE_DIR})
endif()

# Rubberband
if(${RUBBERBAND_FOUND})
  target_sources(LibAudioStream PRIVATE ${RUBBERBAND_STREAM})

  target_link_libraries(LibAudioStream PUBLIC ${RUBBERBAND_LIBRARIES})
  target_include_directories(LibAudioStream PRIVATE ${RUBBERBAND_INCLUDE_DIR})
endif()

# Faust
if(${FAUST_FOUND})
    target_sources(LibAudioStream PRIVATE ${FAUST_STREAM})
    message(${FAUST_LIBRARIES})

    target_link_libraries(LibAudioStream PUBLIC ${FAUST_LIBRARIES})
    target_include_directories(LibAudioStream PRIVATE ${FAUST_INCLUDE_DIR})
endif()

if(LIBAUDIOSTREAM_SAMPLES)
    add_executable(fileplaycpp samples/fileplay.cpp)
    add_executable(record samples/record.cpp)
    add_executable(fileplay samples/fileplay.c)

    target_link_libraries(fileplaycpp PUBLIC LibAudioStream)
    target_link_libraries(record PUBLIC LibAudioStream)
    target_link_libraries(fileplay PUBLIC LibAudioStream)

    target_include_directories(fileplaycpp PRIVATE src)
    target_include_directories(record PRIVATE src)
    target_include_directories(fileplay PRIVATE src)

    set_target_properties(fileplaycpp PROPERTIES CXX_STANDARD 14)
    set_target_properties(record PROPERTIES CXX_STANDARD 14)
endif()

if(LIBAUDIOSTREAM_TESTS)
    add_executable(test1 src/tester1.cpp)
    add_executable(test2 src/tester2.cpp)
    add_executable(test3 src/tester3.cpp)

    target_link_libraries(test1 PUBLIC LibAudioStream)
    target_link_libraries(test2 PUBLIC LibAudioStream)
    target_link_libraries(test3 PUBLIC LibAudioStream)

    target_include_directories(test1 PRIVATE src)
    target_include_directories(test2 PRIVATE src)
    target_include_directories(test3 PRIVATE src)

    set_target_properties(test1 PROPERTIES CXX_STANDARD 14)
    set_target_properties(test2 PROPERTIES CXX_STANDARD 14)
    set_target_properties(test3 PROPERTIES CXX_STANDARD 14)
endif()


### install setup ###

install(TARGETS LibAudioStream
    EXPORT LibAudioStream-targets
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib/static
    RUNTIME DESTINATION bin
    FRAMEWORK DESTINATION lib
    INCLUDES DESTINATION include)

install(
    FILES src/LibAudioStreamMC++.h
    DESTINATION include/
    COMPONENT Devel)

include(CMakePackageConfigHelpers)
write_basic_package_version_file(
  "${CMAKE_CURRENT_BINARY_DIR}/LibAudioStream/LibAudioStream-ConfigVersion.cmake"
  VERSION ${PROJECT_VERSION}
  COMPATIBILITY AnyNewerVersion
)

export(EXPORT LibAudioStream-targets
  FILE "${CMAKE_CURRENT_BINARY_DIR}/LibAudioStream/LibAudioStream-Targets.cmake"
  NAMESPACE LibAudioStream::
)

configure_file(cmake/LibAudioStream-Config.cmake.in
  "${CMAKE_CURRENT_BINARY_DIR}/LibAudioStream/LibAudioStream-Config.cmake"
  COPYONLY
)

set(ConfigPackageLocation lib/cmake/LibAudioStream)
install(EXPORT LibAudioStream-targets
        DESTINATION "${ConfigPackageLocation}"
        NAMESPACE LibAudioStream::)
