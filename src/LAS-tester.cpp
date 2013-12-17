
#include "LAS-test.h"

// Test CONSTANT
void test0()
{
    AudioStream s = MakeConstantSound(2, 50, 0.5f);
    FileRender(s, 512, "test0.txt");
}

// Test CUT
void test1_1()
{
    AudioStream s1 = MakeConstantSound(2, 10, 0.5f);
    AudioStream s2 = MakeCutSound(s1, 0, 5);
    printf("error = %s\n", GetLastLibError());
    FileRender(s2, 512, "test1_1.txt");
}

void test1_2()
{
    AudioStream s1 = MakeConstantSound(2, 10, 0.5f);
    AudioStream s2 = MakeCutSound(s1, 0, 9);
    printf("error = %s\n", GetLastLibError());
    FileRender(s2, 512, "test1_2.txt");
}

void test1_3()
{
    AudioStream s1 = MakeConstantSound(2, 10, 0.5f);
    AudioStream s2 = MakeCutSound(s1, 10, 20);
    printf("error = %s\n", GetLastLibError());
    FileRender(s2, 512, "test1_3.txt");
}

// Test FADE
void test2_1()
{
    AudioStream s = MakeFadeSound(MakeConstantSound(2, 20, 1.f), 5, 5);
    FileRender(s, 60, "test2_1_1.txt");
    FileRender(s, 60, "test2_1_2.txt", true);
}

void test2_2()
{
    AudioStream s = MakeFadeSound(MakeConstantSound(2, 300, 1.f), 100, 100);
   // FileRender(s, 50, "test2_2_1.txt");
    FileRender(s, 50, "test2_2_2.txt", true);
}

void test2_3()
{
    AudioStream s = MakeFadeSound(MakeConstantSound(2, 300, 1.f), 100, 100);
    FileRender(s, 100, "test2_3_1.txt");
    FileRender(s, 100, "test2_3_2.txt", true);
}

void test2_4()
{
    AudioStream s = MakeFadeSound(MakeConstantSound(2, 300, 1.f), 100, 100);
    FileRender(s, 1000, "test2_4_1.txt");
    FileRender(s, 1000, "test2_4_2.txt", true);
}

void test2_5()
{
    AudioStream s = MakeFadeSound(MakeConstantSound(2, 300, 1.f), 100, 1000);
    printf("error = %s\n", GetLastLibError());
    FileRender(s, 1000, "test2_5_1.txt");
    FileRender(s, 1000, "test2_5_2.txt", true);
}

// Test SEQUENCE
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

// Test MIX
void test4()
{
    AudioStream s1 = MakeConstantSound(2, 100, 0.5f);
    AudioStream s2 = MakeConstantSound(2, 400, 1.f);
    AudioStream s3 = MakeMixSound(s1, s2);
    
    printf("len s3 %d\n", GetLengthSound(s3));
    FileRender(s3, 100, "test4.txt");
}

// Test PAR
void test5()
{
    AudioStream s1 = MakeConstantSound(2, 100, 0.5f);
    AudioStream s2 = MakeConstantSound(2, 400, 1.f);
    AudioStream s3 = MakeParSound(s1, s2);
    
    printf("len s3 %d\n", GetLengthSound(s3));
    FileRender(s3, 100, "test5.txt");
}

// Test SELECTION
void test6_1()
{
    AudioStream s1 = MakeConstantSound(1, 100, 0.5f);
    AudioStream s2 = MakeConstantSound(1, 100, 1.f);
    AudioStream s3 = MakeParSound(s1, s2);
    
    std::vector <int> selection1;
    selection1.push_back(1);
    selection1.push_back(2);
    AudioStream s4 = MakeSelectSound(s3, selection1);
    printf("error = %s\n", GetLastLibError());
    
    printf("len s4 %d\n", GetLengthSound(s4));
    FileRender(s4, 100, "test6_1.txt");
}

void test6_2()
{
    AudioStream s1 = MakeConstantSound(2, 100, 0.5f);
    AudioStream s2 = MakeConstantSound(2, 100, 1.f);
    AudioStream s3 = MakeParSound(s1, s2);
    
    std::vector <int> selection1;
    selection1.push_back(1);
    selection1.push_back(2);
    AudioStream s4 = MakeSelectSound(s3, selection1);
    printf("error = %s\n", GetLastLibError());
    
    printf("len s4 %d\n", GetLengthSound(s4));
    FileRender(s4, 100, "test6_2.txt");
}

// Test EFFECT
void test7()
{
    AudioStream s1 = MakeConstantSound(2, 1000, 1.f);
    printf("len s1 %d\n", GetLengthSound(s1));
    AudioStream s2 = MakeEffectSound(s1, MakeFaustAudioEffect("process = _*0.5,_*0.5;", "", ""), 10, 10);
    printf("error = %s\n", GetLastLibError());
    
    printf("len s2 %d\n", GetLengthSound(s2));
    FileRender(s2, 100, "test7.txt");
}


int main()
{
    AudioGlobalsInit(2, 2, 44100, 512, 65536, 65536, 1);
    /*
    test0();
    test1_1();
    test1_2();
    test1_3();
    
    test1();
    printf("error = %s\n", GetLastLibError());
     */
    test2_1();
    printf("error = %s\n", GetLastLibError());
   
    
    //test2_2();
    //printf("error = %s\n", GetLastLibError());
    
    /*
    
    test2_3();
    printf("error = %s\n", GetLastLibError());
    test2_4();
    printf("error = %s\n", GetLastLibError());
    test2_5();
    printf("error = %s\n", GetLastLibError());
    /*
    test3();
    printf("error = %s\n", GetLastLibError());
    test4();
    printf("error = %s\n", GetLastLibError());
    test5();
    printf("error = %s\n", GetLastLibError());
    */
    //test6_1();
    //test6_2();
    //printf("error = %s\n", GetLastLibError());
}
