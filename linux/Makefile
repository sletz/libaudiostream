
CPPSRC = $(wildcard ../src/[^m]*.cpp)
CSRC = $(wildcard ../src/atomic/*ifo.c)
OBJ = $(patsubst %.cpp, %.o, $(CPPSRC)) $(patsubst %.c, %.o, $(CSRC))
CXXFLAGS = -Wall -I../src/atomic -D__JACK__ -D__PORTAUDIO__

TARGET = libaudiostream.a


$(TARGET) : $(OBJ)
	ar -ru $(TARGET) $(OBJ) 
	ranlib  $(TARGET)

dynamic : $(OBJ)
	g++ $(OBJ) -lportaudio -ljack -lsndfile -lstdc++ -shared -fPIC -o libaudiostream.so

test :
	g++  -I../src  ../src/main.cpp -lportaudio -ljack -lsndfile -lstdc++ libaudiostream.a -o testplayer


samples :
	g++  -I../src  ../samples/fileplay.cpp -lportaudio -ljack -lsndfile -lstdc++ libaudiostream.a -o fileplaycpp
	g++  -I../src  ../samples/record.cpp -lportaudio -ljack -lsndfile -lstdc++ libaudiostream.a -o recordcpp
	gcc  -I../src  ../samples/fileplay.c -lportaudio -ljack -lsndfile -lstdc++ libaudiostream.a -o fileplay

samples1 :
	g++  -I../src  ../samples/fileplay.cpp -lportaudio -ljack -lsndfile -lstdc++ -laudiostream -o fileplaycpp
	g++  -I../src  ../samples/record.cpp -lportaudio -ljack -lsndfile -lstdc++ -laudiostream -o recordcpp
	gcc  -I../src  ../samples/fileplay.c -lportaudio -ljack -lsndfile -lstdc++ -laudiostream -o fileplay

clean :
	rm -f $(TARGET) $(OBJ)

dep:
	makedepend -- $(CXXFLAGS) -- $(CPPSRC) $(CSRC)

