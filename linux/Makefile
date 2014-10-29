
CPPSRC = $(wildcard ../src/[^m]*.cpp)
NOCPPSRC = ../src/TAudioMixer.cpp ../src/TAudioEngine.cpp	\
../src/LAS-test.cpp ../src/TTransformAudioStream.cpp		\
../src/TAudioChannel.cpp ../src/tester.cpp ../src/tester1.cpp	\
../src/tester2.cpp ../src/tester3.cpp ../src/LAS-tester.cpp	\
../src/TSoundTouchAudioStream.cpp ../src/FaustReader.cpp	\
../src/TAudioEffect.cpp ../src/LibAudioStream.cpp

CPPSRC := $(filter-out $(NOCPPSRC), $(CPPSRC))

# CPPSRC := $(filter-out ../src/TAudioMixer.cpp, $(CPPSRC))
# CPPSRC := $(filter-out ../src/TAudioEngine.cpp, $(CPPSRC))
# CPPSRC := $(filter-out ../src/LAS-test.cpp, $(CPPSRC))



CPPSRC := $(filter-out ../src/renderer/TCoreAudioRenderer.cpp, $(CPPSRC))

CPPSRC_PORTAUDIO_V18 = $(CPPSRC) ../src/renderer/TPortAudioRenderer.cpp
CPPSRC_PORTAUDIO_V19 = $(CPPSRC) ../src/renderer/TPortAudioV19Renderer.cpp
CPPSRC_JACK = $(CPPSRC) ../src/renderer/TJackRenderer.cpp ../src/renderer/TOfflineRenderer.cpp  ../src/renderer/TAudioRenderer.cpp 

CSRC = $(wildcard ../src/atomic/*ifo.c)

OBJ = $(patsubst %.cpp, %.o, $(CPPSRC)) $(patsubst %.c, %.o, $(CSRC))
OBJ_PORTAUDIO_V18 = $(patsubst %.cpp, %.o, $(CPPSRC_PORTAUDIO_V18)) $(patsubst %.c, %.o, $(CSRC))
OBJ_PORTAUDIO_V19 = $(patsubst %.cpp, %.o, $(CPPSRC_PORTAUDIO_V19)) $(patsubst %.c, %.o, $(CSRC))
OBJ_JACK = $(patsubst %.cpp, %.o, $(CPPSRC_JACK)) $(patsubst %.c, %.o, $(CSRC))

INCLUDES = -I../src/atomic -I../src/renderer -I../src -I/usr/local/include/faust


#CXXFLAG = -Wall -I../src/atomic -I../src/renderer -I../src -D__JACK__ -D__PORTAUDIO__
#CXXFLAGS = -Wall -I../src/atomic -I../src/renderer -I../src -D__PORTAUDIO__ -D__PORTAUDIOV19__
# CXXFLAGS = -Wall -I../src/atomic -I../src/renderer -I../src -D__PORTAUDIO__
# CXXFLAGS = -Wall -I../src/atomic -I../src/renderer -I../src -D__PORTAUDIO__ -std=gnu++11  -fPIC -march=native
# CXXFLAGS = -Wall  $(INCLUDES) -D__PORTAUDIO__ -D__PORTAUDIOV19__ -std=gnu++11  -fPIC -march=native
# CXXFLAGS = -Wall   $(INCLUDES) -D__PORTAUDIO__ -D__PORTAUDIOV19__ -fPIC 
# CXXFLAGS =  -Wall   $(INCLUDES) -D__JACK__ -fPIC 
# CFLAGS = -fPIC

CXXFLAGS =  -Wall -Wl,-z,defs $(INCLUDES) -D__JACK__  -fPIC 
CFLAGS =  -Wall -Wl,-z,defs -fPIC 

# CXXFLAGS = -m32 -Wall -Wl,-z,defs   $(INCLUDES) -D__JACK__  -march=i686
# CFLAGS = -m32 -Wall -Wl,-z,defs -march=i686

LIBS = -L/usr/local/lib -lpthread -lrubberband -lfaust -lsamplerate

TARGET = libaudiostream.a

$(TARGET) : $(OBJ_JACK)
	ar -ru $(TARGET) $(OBJ_JACK) 
	ranlib  $(TARGET)
# $(TARGET) : $(OBJ_PORTAUDIO_V19)
# 	ar -ru $(TARGET) $(OBJ_PORTAUDIO_V19) 
# 	ranlib  $(TARGET)
# $(TARGET) : $(OBJ_PORTAUDIO_V18)
# 	ar -ru $(TARGET) $(OBJ_PORTAUDIO_V18) 
# 	ranlib  $(TARGET)

dynamic_portaudio_v18 : $(OBJ_PORTAUDIO_V18)
	$(CXX) $(OBJ_PORTAUDIO_V18) -lportaudio -ljack -lsndfile -lstdc++ -shared -fPIC -o libaudiostream.so

dynamic_portaudio_v19 : $(OBJ_PORTAUDIO_V19)
	$(CXX) $(OBJ_PORTAUDIO_V19) -lportaudio -ljack -lsndfile -lstdc++ -shared -fPIC -o libaudiostream.so

jack : $(OBJ_JACK)
	$(CXX) $(CFLAGS) $(OBJ_JACK) $(LIBS) -ldl -ljack -lsndfile -lstdc++ -shared -fPIC -o libaudiostream.so

test :
	$(CXX)  -I../src  ../src/main.cpp -lportaudio -ljack -lsndfile -lstdc++ -ldl -lfaust libaudiostream.a -o testplayer

samples :
	$(CXX)  -I../src  ../samples/fileplay.cpp -lportaudio -ljack -lsndfile -lstdc++ libaudiostream.a -o fileplaycpp
	$(CXX)  -I../src  ../samples/record.cpp -lportaudio -ljack -lsndfile -lstdc++ libaudiostream.a -o recordcpp
	$(CC) -I../src  ../samples/fileplay.c -lportaudio -ljack -lsndfile -lstdc++ libaudiostream.a -o fileplay

samples1 :
	$(CXX)  -I../src  ../samples/fileplay.cpp -laudiostream -o fileplaycpp
	$(CXX)  -I../src  ../samples/record.cpp -laudiostream -o recordcpp
	$(CC)  -I../src  ../samples/fileplay.c -laudiostream -o fileplay

clean :
	rm -f $(TARGET) $(OBJ)

dep:
	makedepend -- $(CXXFLAGS) -- $(CPPSRC) $(CSRC)

