project(libaudiostream LANGUAGES C CXX)

# Configuration
cmake_minimum_required(VERSION 3.4 FATAL_ERROR)
set(CMAKE_MODULE_PATH
  "${CMAKE_MODULE_PATH}"
  "${CMAKE_CURRENT_SOURCE_DIR}/cmake"
)

# Sources
set(CSRC
src/atomic/lffifo.c
src/atomic/lffifoIntel.c
src/atomic/lffifoppc.c
src/atomic/lflifo.c
src/atomic/lflifoIntel.c
src/atomic/lflifoppc.c
src/sorter/msSorter.c
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

# These files require Faust to build
src/LibAudioStreamMC.cpp
src/TAudioGlobals.cpp
src/TAudioRendererFactory.cpp
src/TAudioStreamFactory.cpp
src/TExpAudioEngine.cpp
src/TExpAudioMixer.cpp
)


set(NOCPP
src/FaustReader.cpp

src/LibAudioStream.cpp

src/TAudioEngine.cpp
src/TAudioMixer.cpp

src/TTransformAudioStream.cpp
src/TSoundTouchAudioStream.cpp
src/TAudioChannel.cpp
src/TAudioEffect.cpp

src/LAS-test.cpp
src/LAS-tester.cpp
src/tester1.cpp
src/tester2.cpp
src/tester3.cpp
src/tester.cpp
)

# Renderers
set(COREAUDIO_RENDERER
src/renderer/TCoreAudioRenderer.cpp
)
set(PA18_RENDERER 
src/renderer/TPortAudioRenderer.cpp
)
set(PA19_RENDERER
src/renderer/TPortAudioV19Renderer.cpp
)
set(JACK_RENDERER
src/renderer/TJackRenderer.cpp
src/renderer/TAudioRenderer.cpp
src/renderer/TOfflineRenderer.cpp
)
set(NETJACK_RENDERER
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
find_package(Faust REQUIRED)
find_package(Jack)
find_package(Portaudio)
find_package(Sndfile)
find_package(Samplerate)
find_package(Rubberband)

add_library(libaudiostream ${CSRC} ${CPPSRC})

target_include_directories(libaudiostream PRIVATE
	src/atomic
	src/renderer
)

target_link_libraries(libaudiostream PRIVATE ${Threads_LIBRARIES})

# Dependencies
# Jack
if(${JACK_FOUND})
  target_sources(libaudiostream PRIVATE ${JACK_RENDERER})
  
  target_compile_definitions(libaudiostream PRIVATE ${JACK_DEFINITIONS})
  target_link_libraries(libaudiostream PRIVATE ${Jack_LIBRARIES})
  target_include_directories(libaudiostream PRIVATE ${JACK_INCLUDE_DIRS})
endif()

# Portaudio
if(${PORTAUDIO_FOUND})
  if(${PORTAUDIO_VERSION} EQUAL 18)
    target_sources(libaudiostream PRIVATE ${PA18_RENDERER})
  elseif(${PORTAUDIO_VERSION} EQUAL 19)
    target_sources(libaudiostream PRIVATE ${PA19_RENDERER})
  endif()
  
  target_compile_definitions(libaudiostream PRIVATE ${PORTAUDIO_DEFINITIONS})
  target_link_libraries(libaudiostream PRIVATE ${PORTAUDIO_LIBRARIES})
  target_include_directories(libaudiostream PRIVATE ${PORTAUDIO_INCLUDE_DIRS})
endif()

# Sndfile
if(${SNDFILE_FOUND})
  target_sources(libaudiostream PRIVATE ${FILE_STREAM})
  
  target_link_libraries(libaudiostream PRIVATE ${SNDFILE_LIBRARIES})
  target_include_directories(libaudiostream PRIVATE ${SNDFILE_INCLUDE_DIR})
endif()

# Samplerate
if(${SAMPLERATE_FOUND})
  target_sources(libaudiostream PRIVATE ${SAMPLERATE_STREAM})
  
  target_link_libraries(libaudiostream PRIVATE ${SAMPLERATE_LIBRARY})
  target_include_directories(libaudiostream PRIVATE ${SAMPLERATE_INCLUDE_DIR})
endif()

# Rubberband
if(${RUBBERBAND_FOUND})
  target_sources(libaudiostream PRIVATE ${RUBBERBAND_STREAM})
  
  target_link_libraries(libaudiostream PRIVATE ${RUBBERBAND_LIBRARY})
  target_include_directories(libaudiostream PRIVATE ${RUBBERBAND_INCLUDE_DIR})
endif()
