
CPPSRC = $(wildcard ../src/[^m]*.cpp)

CPPSRC_PORTAUDIO_V18 = $(CPPSRC) ../src/renderer/TPortAudioRenderer.cpp
CPPSRC_PORTAUDIO_V19 = $(CPPSRC) ../src/renderer/TPortAudioV19Renderer.cpp

CSRC = $(wildcard ../src/atomic/*ifo.c)

OBJ = $(patsubst %.cpp, %.o, $(CPPSRC)) $(patsubst %.c, %.o, $(CSRC))
OBJ_PORTAUDIO_V18 = $(patsubst %.cpp, %.o, $(CPPSRC_PORTAUDIO_V18)) $(patsubst %.c, %.o, $(CSRC))
OBJ_PORTAUDIO_V19 = $(patsubst %.cpp, %.o, $(CPPSRC_PORTAUDIO_V19)) $(patsubst %.c, %.o, $(CSRC))

#CXXFLAG = -Wall -I../src/atomic -D__JACK__ -D__PORTAUDIO__
#CXXFLAGS = -Wall -I../src/atomic -D__PORTAUDIO__ -D__PORTAUDIOV19__
CXXFLAGS = -Wall -I../src/atomic -D__PORTAUDIO__

TARGET = libaudiostream.a

$(TARGET) : $(OBJ)
	
	ar -ru $(TARGET) $(OBJ) 
	ranlib  $(TARGET)

dynamic_portaudio_v18 : $(OBJ_PORTAUDIO_V18)
	g++ $(OBJ_PORTAUDIO_V18) -lportaudio -ljack -lsndfile -lstdc++ -shared -fPIC -o libaudiostream.so

dynamic_portaudio_v19 : $(OBJ_PORTAUDIO_V19)
	g++ $(OBJ_PORTAUDIO_V19) -lportaudio -ljack -lsndfile -lstdc++ -shared -fPIC -o libaudiostream.so

test :
	g++  -I../src  ../src/main.cpp -lportaudio -ljack -lsndfile -lstdc++ libaudiostream.a -o testplayer

samples :
	g++  -I../src  ../samples/fileplay.cpp -lportaudio -ljack -lsndfile -lstdc++ libaudiostream.a -o fileplaycpp
	g++  -I../src  ../samples/record.cpp -lportaudio -ljack -lsndfile -lstdc++ libaudiostream.a -o recordcpp
	gcc  -I../src  ../samples/fileplay.c -lportaudio -ljack -lsndfile -lstdc++ libaudiostream.a -o fileplay

samples1 :
	g++  -I../src  ../samples/fileplay.cpp -laudiostream -o fileplaycpp
	g++  -I../src  ../samples/record.cpp -laudiostream -o recordcpp
	gcc  -I../src  ../samples/fileplay.c -laudiostream -o fileplay

clean :
	rm -f $(TARGET) $(OBJ)

dep:
	makedepend -- $(CXXFLAGS) -- $(CPPSRC) $(CSRC)

