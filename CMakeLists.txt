project(libaudiostream LANGUAGES C CXX)

# Configuration
cmake_minimum_required(VERSION 3.4 FATAL_ERROR)
set(CMAKE_MODULE_PATH
  "${CMAKE_MODULE_PATH}"
  "${CMAKE_CURRENT_SOURCE_DIR}/cmake"
)

if(UNIX AND NOT APPLE)
    find_package(DL REQUIRED)
endif()

# Sources
set(CSRC
src/atomic/lffifo.c
src/atomic/lflifo.c

# src/atomic/lffifoIntel.c
# src/atomic/lflifoIntel.c

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
find_package(Sndfile)
find_package(Samplerate REQUIRED)
find_package(Rubberband REQUIRED)
find_package(Faust REQUIRED)

add_library(libaudiostream ${CSRC} ${CPPSRC})
if(WIN32)
    target_compile_definitions(libaudiostream
        PUBLIC WIN32
        PRIVATE NOMINMAX)
    set_target_properties(libaudiostream
        PROPERTIES
          WINDOWS_EXPORT_ALL_SYMBOLS 1)
endif()
target_compile_definitions(libaudiostream
    PRIVATE LIBAUDIOSTREAM_EXPORTS)

set_target_properties(
    libaudiostream
    PROPERTIES
        CXX_STANDARD 14)
target_include_directories(libaudiostream PRIVATE
  src
  src/atomic
  src/renderer
)
target_include_directories(libaudiostream SYSTEM INTERFACE
  src
  src/atomic
  src/renderer
)

target_link_libraries(libaudiostream PUBLIC ${Threads_LIBRARIES})
# Dependencies
# OS X
if(APPLE)
  target_sources(libaudiostream PRIVATE ${COREAUDIO_RENDERER})

  find_library(COREFOUNDATION_LIBRARY CoreFoundation)
  find_library(COREAUDIO_LIBRARY CoreAudio)
  find_library(CORESERVICES_LIBRARY CoreServices)
  find_library(AUDIOUNIT_LIBRARY AudioUnit)
  find_library(ACCELERATE_LIBRARY Accelerate)
  find_library(AUDIOTOOLBOX_LIBRARY AudioToolbox)
  target_link_libraries(libaudiostream PUBLIC ${COREFOUNDATION_LIBRARY} ${COREAUDIO_LIBRARY} ${CORESERVICES_LIBRARY} ${AUDIOUNIT_LIBRARY} ${ACCELERATE_LIBRARY} ${AUDIOTOOLBOX_LIBRARY})
  target_compile_definitions(libaudiostream PRIVATE
      __COREAUDIO__)
endif()

# Linux
if(UNIX AND NOT APPLE)
    target_link_libraries(libaudiostream PUBLIC ${LIBDL_LIBRARIES})
endif()

# Jack
if(${JACK_FOUND})
  target_sources(libaudiostream PRIVATE ${JACK_RENDERER})

  target_compile_definitions(libaudiostream PRIVATE
      ${JACK_DEFINITIONS}
      __JACK__)
  target_link_libraries(libaudiostream PUBLIC ${JACK_LIBRARIES})
  target_include_directories(libaudiostream PRIVATE ${JACK_INCLUDE_DIRS})
endif()

# Portaudio
if(${PORTAUDIO_FOUND})
  message("${PORTAUDIO_VERSION}")
  if("${PORTAUDIO_VERSION}" EQUAL 18)
    target_sources(libaudiostream PRIVATE ${PA18_RENDERER})
  elseif("${PORTAUDIO_VERSION}" EQUAL 19)
    target_sources(libaudiostream PRIVATE ${PA19_RENDERER})
    target_compile_definitions(libaudiostream PRIVATE __PORTAUDIOV19__)
  endif()

  target_compile_definitions(libaudiostream PRIVATE
      ${PORTAUDIO_DEFINITIONS}
      __PORTAUDIO__)
  target_link_libraries(libaudiostream PUBLIC ${PORTAUDIO_LIBRARIES})
  target_include_directories(libaudiostream PRIVATE ${PORTAUDIO_INCLUDE_DIRS})
endif()

# Sndfile
if(${SNDFILE_FOUND})
  target_sources(libaudiostream PRIVATE ${FILE_STREAM})

  target_link_libraries(libaudiostream PUBLIC ${SNDFILE_LIBRARIES})
  target_include_directories(libaudiostream PRIVATE ${SNDFILE_INCLUDE_DIR})
endif()

# Samplerate
if(${SAMPLERATE_FOUND})
  target_sources(libaudiostream PRIVATE ${SAMPLERATE_STREAM})

  target_link_libraries(libaudiostream PUBLIC ${SAMPLERATE_LIBRARY})
  target_include_directories(libaudiostream PRIVATE ${SAMPLERATE_INCLUDE_DIR})
endif()

# Rubberband
if(${RUBBERBAND_FOUND})
  target_sources(libaudiostream PRIVATE ${RUBBERBAND_STREAM})

  target_link_libraries(libaudiostream PUBLIC ${RUBBERBAND_LIBRARIES})
  target_include_directories(libaudiostream PRIVATE ${RUBBERBAND_INCLUDE_DIR})
endif()

# Faust
if(${FAUST_FOUND})
    target_sources(libaudiostream PRIVATE ${FAUST_STREAM})
    message(${FAUST_LIBRARIES})

    target_link_libraries(libaudiostream PUBLIC ${FAUST_LIBRARIES})
    target_include_directories(libaudiostream PRIVATE ${FAUST_INCLUDE_DIR})
endif()

if(LIBAUDIOSTREAM_SAMPLES)
    add_executable(fileplaycpp samples/fileplay.cpp)
    add_executable(record samples/record.cpp)
    add_executable(fileplay samples/fileplay.c)

    target_link_libraries(fileplaycpp PUBLIC libaudiostream)
    target_link_libraries(record PUBLIC libaudiostream)
    target_link_libraries(fileplay PUBLIC libaudiostream)

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

    target_link_libraries(test1 PUBLIC libaudiostream)
    target_link_libraries(test2 PUBLIC libaudiostream)
    target_link_libraries(test3 PUBLIC libaudiostream)

    target_include_directories(test1 PRIVATE src)
    target_include_directories(test2 PRIVATE src)
    target_include_directories(test3 PRIVATE src)

    set_target_properties(test1 PROPERTIES CXX_STANDARD 14)
    set_target_properties(test2 PROPERTIES CXX_STANDARD 14)
    set_target_properties(test3 PROPERTIES CXX_STANDARD 14)
endif()
