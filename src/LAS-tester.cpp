
#include "LAS-test.h"


void test1()
{
    AudioStream s = MakeConstantSound(2, 50, 0.5f);
    FileRender(s, 512, "test1.txt");
}

void test2_1()
{
    AudioStream s = MakeFadeSound(MakeConstantSound(2, 300, 0.5f), 100, 100);
    FileRender(s, 10, "test2_1.txt");
}

void test2_2()
{
    AudioStream s = MakeFadeSound(MakeConstantSound(2, 300, 0.5f), 100, 100);
    FileRender(s, 50, "test2_2.txt");
}

void test2_3()
{
    AudioStream s = MakeFadeSound(MakeConstantSound(2, 300, 0.5f), 100, 100);
    FileRender(s, 100, "test2_3.txt");
}

void test2_4()
{
    AudioStream s = MakeFadeSound(MakeConstantSound(2, 300, 0.5f), 100, 100);
    FileRender(s, 1000, "test2_4.txt");
}

void test2_5()
{
    AudioStream s = MakeFadeSound(MakeConstantSound(2, 300, 0.5f), 100, 1000);
    printf("error = %s\n", GetLastLibError());
    FileRender(s, 1000, "test2_5.txt");
}

void test3()
{
    AudioStream s1 = MakeConstantSound(2, 400, 0.5f);
    AudioStream s2 = MakeConstantSound(2, 400, 1.f);
    printf("len s1 %d\n", GetLengthSound(s1));
    printf("len s2 %d\n", GetLengthSound(s2));
    
    AudioStream s3 = MakeSeqSound(s1, s2, 1000);
    printf("error = %s\n", GetLastLibError());
    
    printf("len s3 %d\n", GetLengthSound(s3));
    FileRender(s3, 100, "test3.txt");
}

int main()
{
    AudioGlobalsInit(2, 2, 44100, 512, 65536, 65536, 1);

    test1();
    printf("error = %s\n", GetLastLibError());
    test2_1();
    printf("error = %s\n", GetLastLibError());
    test2_2();
    printf("error = %s\n", GetLastLibError());
    test2_3();
    printf("error = %s\n", GetLastLibError());
    test2_4();
    printf("error = %s\n", GetLastLibError());
    test2_5();
    printf("error = %s\n", GetLastLibError());
    test3();
    printf("error = %s\n", GetLastLibError());
}
