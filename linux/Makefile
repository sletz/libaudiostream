
CPPSRC = $(wildcard ../src/[^m]*.cpp)
CSRC = $(wildcard ../src/atomic/*ifo.c)
OBJ = $(patsubst %.cpp, %.o, $(CPPSRC)) $(patsubst %.c, %.o, $(CSRC))
CXXFLAGS = -Wall -I../src/atomic

TARGET = libaudiostream.a


$(TARGET) : $(OBJ)
	ar -ru $(TARGET) $(OBJ) 
	ranlib  $(TARGET)

samples :
	gcc  -I../src  ../samples/fileplay.cpp -lportaudio -ljack -lsndfile -lstdc++ libaudiostream.a -o fileplay

clean :
	rm -f $(TARGET) $(OBJ)

dep:
	makedepend -- $(CXXFLAGS) -- $(CPPSRC) $(CSRC)

