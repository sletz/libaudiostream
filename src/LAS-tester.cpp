
#include "LAS-test.h"


void test1()
{
    AudioStream s = MakeConstantSound(2, 50, 0.5f);
    FileRender(s, 512, "test1.txt");
}

void test2()
{
    AudioStream s = MakeFadeSound(MakeConstantSound(2, 301, 0.5f), 100, 100);
    FileRender(s, 10, "test2.txt");
}

void test3()
{
    AudioStream s = MakeSeqSound(MakeConstantSound(2, 400, 0.5f), MakeConstantSound(2, 400, 1.f), 0);
    FileRender(s, 100, "test3.txt");
}



int main()
{
    long tmpInChan = 2;
    long tmpOutChan = 2;
    long tmpBufferSize = 512;
    long tmpSampleRate = 44100;
    
    long inputDevice = 2;
    long ouputDevice = 2;

    AudioGlobalsInit(tmpInChan, tmpOutChan, tmpSampleRate, tmpBufferSize, 65536, 65536, 1);

    test1();
    test2();
    //test3();
}
