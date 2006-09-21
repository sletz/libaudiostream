
CPPSRC = $(wildcard ../src/[^m]*.cpp)
CSRC = $(wildcard ../src/atomic/*ifo.c)
OBJ = $(patsubst %.cpp, %.o, $(CPPSRC)) $(patsubst %.c, %.o, $(CSRC))
CXXFLAGS = -Wall -I../src/atomic -D__JACK__ -D__PORTAUDIO__

TARGET = libaudiostream.a


$(TARGET) : $(OBJ)
	ar -ru $(TARGET) $(OBJ) 
	ranlib  $(TARGET)

test :
	g++  -I../src  ../src/main.cpp -lportaudio -ljack -lsndfile -lstdc++ libaudiostream.a -o testplayer


samples :
	g++  -I../src  ../samples/fileplay.cpp -lportaudio -ljack -lsndfile -lstdc++ libaudiostream.a -o fileplaycpp
	g++  -I../src  ../samples/record.cpp -lportaudio -ljack -lsndfile -lstdc++ libaudiostream.a -o recordcpp
	gcc  -I../src  ../samples/fileplay.c -lportaudio -ljack -lsndfile libaudiostream.a -o fileplay


clean :
	rm -f $(TARGET) $(OBJ)

dep:
	makedepend -- $(CXXFLAGS) -- $(CPPSRC) $(CSRC)

