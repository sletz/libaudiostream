
#include <stdio.h>
#include "LibAudioStreamMC++.h"

#define LLVM_EFFECT1 "/Documents/faust-sf/examples/freeverb.dsp"
#define LLVM_EFFECT2 "/Documents/faust-sf/examples/zita_rev1.dsp"
#define LLVM_EFFECT3 "/Documents/faust-sf/examples/freeverb4.dsp"

#define FILENAME1 "/Users/letz/Music/Sounds/levot.wav"
#define FILENAME2 "/Users/letz/Music/Sounds/tango.wav"
#define FILENAME3 "/Users/letz/son1.wav"
#define FILENAME4 "/Users/letz/Music/Sounds/levot-mono.aiff"

// Global context
static long gSampleRate = 0;
static long gBufferSize = 0;
static AudioRendererPtr gAudioRenderer = 0;
static AudioPlayerPtr gAudioPlayer = 0;

static long tmpInChan = 4;
static long tmpOutChan = 4;

//static long tmpInChan = 2;
//static long tmpOutChan = 2;

static long tmpBufferSize = 512;
static long tmpSampleRate = 44100;
  
static AudioEffect faust_effect1 = MakeFaustAudioEffect(LLVM_EFFECT1, "", "");
static AudioEffect faust_effect2 = MakeFaustAudioEffect(LLVM_EFFECT2, "", "");
static AudioEffect faust_effect3 = MakeFaustAudioEffect("process = _@10000,_@10000,_@10000,_@10000;", "", "");
static AudioEffect faust_effect4 = MakeFaustAudioEffect(LLVM_EFFECT3, "", "");
static AudioEffect faust_effect5 = MakeFaustAudioEffect("process = _*vslider(\"Volume\", 1, 0, 1, 0.1);", "", "");

class TAudioLASClient : public TAudioClient {

    public:

        TAudioLASClient()
        {}
        virtual ~TAudioLASClient()
        {}

	    virtual bool AudioCallback(float** inputs, float** outputs, long frames)
        {
            //printf("AudioCallback frames = %d\n", frames);
            return true;
        }
};

static audio_frames_t getCurDate()
{
    AudioRendererPtr renderer = GetAudioPlayerRenderer(gAudioPlayer);
    RendererInfo info;
    GetAudioRendererInfo(renderer, &info);
    return info.fCurFrame;
}
    
static void printControls(AudioEffect faust_effect)
{
    printf("Faust effect: param num %ld\n", GetControlCountEffect(faust_effect));
    for (int i = 0; i < GetControlCountEffect(faust_effect); i++) {
        float min, max, init;
        char label[512];
        GetControlParamEffect(faust_effect, i, label, &min, &max, &init); 
        printf("Faust effect: param label = %s min = %f max = %f init = %f value = %f\n", label, min, max, init, GetControlValueEffect(faust_effect, i));
    }
}

static void test0()
{
    AudioStream stream1 = MakeRegionSound(FILENAME1, 5 * tmpSampleRate, tmpSampleRate * 200);
    StartSound(gAudioPlayer, stream1, GenRealDate(gAudioPlayer, 0));
    //StopSound(gAudioPlayer, stream1, GenRealDate(gAudioPlayer, tmpSampleRate * 5));
}

static void test1()
{
    AudioStream stream1 = MakeRegionSound(FILENAME1, 5 * tmpSampleRate, tmpSampleRate * 15);
    StartSound(gAudioPlayer, stream1, GenRealDate(gAudioPlayer, 0));

    AudioStream stream2 = MakeRegionSound(FILENAME2, 0, tmpSampleRate * 25);
    StartSound(gAudioPlayer, stream2, GenRealDate(gAudioPlayer, 0));
}

static void test2()
{
    AudioStream stream1 = MakeRegionSound(FILENAME1, 5 * tmpSampleRate, tmpSampleRate * 70);
    AudioStream stream2 = MakeRegionSound(FILENAME2, 0, tmpSampleRate * 70);
    AudioStream stream3 = MakeParSound(stream1, stream2);
    StartSound(gAudioPlayer, stream3, GenRealDate(gAudioPlayer, 0));
}

static void test3()
{
    AudioStream stream1 = MakeRegionSound(FILENAME4, 5 * tmpSampleRate, tmpSampleRate * 15);
    AudioStream stream2 = MakeRegionSound(FILENAME4, 7 * tmpSampleRate, tmpSampleRate * 15);
    
    AudioStream stream3 = MakeRegionSound(FILENAME2, 0, tmpSampleRate * 25);
    AudioStream stream4 = MakeParSound(stream1, MakeParSound(stream2, stream3));
    StartSound(gAudioPlayer, stream4, GenRealDate(gAudioPlayer, 0));
}

static void test4()
{
    AudioStream stream1 = MakeEffectSound(MakeRegionSound(FILENAME1, 5 * tmpSampleRate, tmpSampleRate * 50), faust_effect1, 100, 100);
    
    printf("stream1 chan %d\n", GetChannelsSound(MakeRegionSound(FILENAME1, 5 * tmpSampleRate, tmpSampleRate * 50)));
    printf("stream1 chan %d\n", GetChannelsSound(stream1));
    
    StartSound(gAudioPlayer, stream1, GenRealDate(gAudioPlayer, 0));
    
    AudioStream stream2 = MakeEffectSound(MakeRegionSound(FILENAME2, 5 * tmpSampleRate, tmpSampleRate * 50), faust_effect2, 100, 100);
    StartSound(gAudioPlayer, stream2, GenRealDate(gAudioPlayer, 0));
}

static void test5()
{
    AudioStream stream1 = MakeEffectSound(MakeRegionSound(FILENAME1, 5 * tmpSampleRate, tmpSampleRate * 50), faust_effect1, 100, 100);
    AudioStream stream2 = MakeEffectSound(MakeRegionSound(FILENAME2, 5 * tmpSampleRate, tmpSampleRate * 50), faust_effect2, 100, 100);
    AudioStream stream3 = MakeParSound(stream1, stream2);
    StartSound(gAudioPlayer, stream3, GenRealDate(gAudioPlayer, 0));
}

static void test6()
{
    AudioStream stream1 = MakeEffectSound(MakeSharedInputSound(), faust_effect4, 100, 100); 
    StartSound(gAudioPlayer, stream1, GenRealDate(gAudioPlayer, 0));
}

static void test7()
{
    AudioStream stream1 = MakeSeqSound(MakeCutSound(MakeSharedInputSound(), 0, 10*tmpSampleRate),
                            MakeLoopSound(MakeCutSound(MakeEffectSound(MakeSharedInputSound(), faust_effect4, 100, 100), 0*tmpSampleRate, 10*tmpSampleRate), 4), 0);
     
    StartSound(gAudioPlayer, stream1, GenRealDate(gAudioPlayer, 0));
}

double pitch_shift = 1.0;
double time_strech = 0.5;

static void test8()
{
    AudioStream stream2 = MakeRegionSound(FILENAME2, 0, tmpSampleRate * 25);
    StartSound(gAudioPlayer, MakePitchSchiftTimeStretchSound(stream2, &pitch_shift, &time_strech), GenRealDate(gAudioPlayer, 0));
}

static void test9()
{
    AudioStream stream1 = MakeRegionSound(FILENAME1, 0, tmpSampleRate * 25);
    AudioStream stream2 = MakeRegionSound(FILENAME2, 0, tmpSampleRate * 25);
    AudioStream stream3 = MakeParSound(stream1, stream2);
    StartSound(gAudioPlayer, MakePitchSchiftTimeStretchSound(stream3, &pitch_shift, &time_strech), GenRealDate(gAudioPlayer, 0));
}

static void test10()
{
    //AudioStream stream1 = MakeEffectSound(MakeSharedInputSound(), faust_effect5, 100, 100);
    //AudioStream stream1 = MakeEffectSound(MakeSharedInputSound(), faust_effect1, 100, 100); 
    AudioStream stream1 = MakeEffectSound(MakeSharedInputSound(), faust_effect4, 100, 100); 
    
    printf("Error %s\n", GetLastLibError()); 
    StartSound(gAudioPlayer, stream1, GenRealDate(gAudioPlayer, 0));
}

static void test11()
{
    //AudioStream stream1 = MakeEffectSound(MakeSharedInputSound(), faust_effect5, 100, 100);
    //AudioStream stream1 = MakeEffectSound(MakeSharedInputSound(), faust_effect1, 100, 100); 
    
    std::vector <int> selection;
    selection.push_back(0);
    
    AudioStream stream1 = MakeSelectSound(MakeEffectSound(MakeSharedInputSound(), faust_effect4, 100, 100), selection); 
    
    printf("Error %s\n", GetLastLibError()); 
    StartSound(gAudioPlayer, stream1, GenRealDate(gAudioPlayer, 0));
}

static void test12()
{
    AudioStream stream1 = MakeRegionSound(FILENAME1, 5 * tmpSampleRate, tmpSampleRate * 50);
    AudioStream stream2 = MakeRegionSound(FILENAME2, 5 * tmpSampleRate, tmpSampleRate * 50);
    std::vector <int> selection;
    selection.push_back(1);
    selection.push_back(2);
    AudioStream stream3 = MakeSelectSound(MakeParSound(stream1, stream2), selection);
    StartSound(gAudioPlayer, stream3, GenRealDate(gAudioPlayer, 0));
}

static void test13()
{
    std::vector <int> selection1;
    selection1.push_back(2);
    AudioStream stream3 = MakeSelectSound(MakeParSound(MakeRegionSound(FILENAME1, 5 * tmpSampleRate, tmpSampleRate * 50), 
        MakeRegionSound(FILENAME2, 5 * tmpSampleRate, tmpSampleRate * 50)), selection1);
        
    std::vector <int> selection2;
    selection2.push_back(1);
    AudioStream stream4 = MakeSelectSound(MakeParSound(MakeRegionSound(FILENAME1, 5 * tmpSampleRate, tmpSampleRate * 50), 
        MakeRegionSound(FILENAME2, 5 * tmpSampleRate, tmpSampleRate * 50)), selection2);
        
    AudioStream stream5 = MakeParSound(stream3, stream4);
    StartSound(gAudioPlayer, stream5, GenRealDate(gAudioPlayer, 0));
}

static void test14()
{
    AudioStream stream1 = MakeRegionSound(FILENAME1, 5 * tmpSampleRate, tmpSampleRate * 7);
    
    audio_frames_t curdate = getCurDate();
    printf("info.Frames %lld\n", curdate);
    
    //AddSound(gAudioPlayer, stream1);
    
    StartSound(gAudioPlayer, MakeRegionSound(FILENAME1, 5*tmpSampleRate, 7*tmpSampleRate), GenRealDate(gAudioPlayer, curdate));
    StartSound(gAudioPlayer, MakeRegionSound(FILENAME1, 5*tmpSampleRate, 7*tmpSampleRate), GenRealDate(gAudioPlayer, curdate + 5*tmpSampleRate));
    
    StartSound(gAudioPlayer, MakeRegionSound(FILENAME1, 5*tmpSampleRate, 7*tmpSampleRate), GenRealDate(gAudioPlayer, curdate + 6*tmpSampleRate));
    StartSound(gAudioPlayer, MakeRegionSound(FILENAME2, 5*tmpSampleRate, 7*tmpSampleRate), GenRealDate(gAudioPlayer, curdate + 6*tmpSampleRate));
    
    sleep(1);
     
    StartSound(gAudioPlayer, MakeRegionSound(FILENAME1, 5*tmpSampleRate, 7*tmpSampleRate), GenRealDate(gAudioPlayer, curdate + 11*tmpSampleRate));
    StartSound(gAudioPlayer, MakeRegionSound(FILENAME2, 5*tmpSampleRate, 7*tmpSampleRate), GenRealDate(gAudioPlayer, curdate + 11*tmpSampleRate));
    
    sleep(1);
     
    StartSound(gAudioPlayer, MakeRegionSound(FILENAME1, 5*tmpSampleRate, 7*tmpSampleRate), GenRealDate(gAudioPlayer, curdate + 13*tmpSampleRate));
    StartSound(gAudioPlayer, MakeRegionSound(FILENAME2, 5*tmpSampleRate, 7*tmpSampleRate), GenRealDate(gAudioPlayer, curdate + 13*tmpSampleRate));
}

static void test15()
{
    AudioStream stream1 = MakeRegionSound(FILENAME1, 5 * tmpSampleRate, tmpSampleRate * 7);
    
    audio_frames_t curdate = getCurDate();
    printf("info.Frames %lld\n", curdate);
    
    //AddSound(gAudioPlayer, stream1);
    
    StartSound(gAudioPlayer, MakeRegionSound(FILENAME1, 5*tmpSampleRate, 7*tmpSampleRate), GenRealDate(gAudioPlayer, curdate));
    StartSound(gAudioPlayer, MakeRegionSound(FILENAME1, 5*tmpSampleRate, 7*tmpSampleRate), GenRealDate(gAudioPlayer, curdate + 13*tmpSampleRate));
    
    StartSound(gAudioPlayer, MakeRegionSound(FILENAME1, 5*tmpSampleRate, 7*tmpSampleRate), GenRealDate(gAudioPlayer, curdate + 12*tmpSampleRate));
    StartSound(gAudioPlayer, MakeRegionSound(FILENAME2, 5*tmpSampleRate, 7*tmpSampleRate), GenRealDate(gAudioPlayer, curdate + 12*tmpSampleRate));
    
    sleep(1);
     
    StartSound(gAudioPlayer, MakeRegionSound(FILENAME1, 5*tmpSampleRate, 7*tmpSampleRate), GenRealDate(gAudioPlayer, curdate + 6*tmpSampleRate));
    StartSound(gAudioPlayer, MakeRegionSound(FILENAME2, 5*tmpSampleRate, 7*tmpSampleRate), GenRealDate(gAudioPlayer, curdate + 6*tmpSampleRate));
    
    sleep(1);
     
    StartSound(gAudioPlayer, MakeRegionSound(FILENAME1, 5*tmpSampleRate, 7*tmpSampleRate), GenRealDate(gAudioPlayer, curdate + 3*tmpSampleRate));
    StartSound(gAudioPlayer, MakeRegionSound(FILENAME2, 5*tmpSampleRate, 7*tmpSampleRate), GenRealDate(gAudioPlayer, curdate + 3*tmpSampleRate));
}

static void test16()
{
    audio_frames_t curdate = getCurDate();
    printf("info.Frames %lld\n", curdate);
    
    StartSound(gAudioPlayer, MakeRegionSound(FILENAME1, 5*tmpSampleRate, 17*tmpSampleRate), GenRealDate(gAudioPlayer, curdate + tmpSampleRate*2));
    StartSound(gAudioPlayer, MakeRegionSound(FILENAME1, 5*tmpSampleRate, 17*tmpSampleRate), GenRealDate(gAudioPlayer, curdate + tmpSampleRate*2 + 4000));
    StartSound(gAudioPlayer, MakeRegionSound(FILENAME1, 5*tmpSampleRate, 17*tmpSampleRate), GenRealDate(gAudioPlayer, curdate + tmpSampleRate*2 + 6000));
    StartSound(gAudioPlayer, MakeRegionSound(FILENAME1, 5*tmpSampleRate, 17*tmpSampleRate), GenRealDate(gAudioPlayer, curdate + tmpSampleRate*2 + 8000));
    StartSound(gAudioPlayer, MakeRegionSound(FILENAME1, 5*tmpSampleRate, 17*tmpSampleRate), GenRealDate(gAudioPlayer, curdate + tmpSampleRate*2 + 10000));
}

SymbolicDate symb1 = GenSymbolicDate(gAudioPlayer);

static void test17()
{
    AudioStream stream1 = MakeRegionSound(FILENAME1, 5 * tmpSampleRate, tmpSampleRate * 20);
    StartSound(gAudioPlayer, stream1, symb1);
}

SymbolicDate symb2 = GenSymbolicDate(gAudioPlayer);

static void test18()
{
    AudioStream stream1 = MakeRegionSound(FILENAME1, 5 * tmpSampleRate, tmpSampleRate * 20);
    AudioStream stream2 = MakeRegionSound(FILENAME2, 5 * tmpSampleRate, tmpSampleRate * 20);
    StartSound(gAudioPlayer, stream1, GenRealDate(gAudioPlayer, 0));
    StopSound(gAudioPlayer, stream1, symb2);
    StartSound(gAudioPlayer, stream2, symb2);
}

SymbolicDate symb3 = GenSymbolicDate(gAudioPlayer);
SymbolicDate symb4 = GenSymbolicDate(gAudioPlayer);

static void test19()
{
    AudioStream stream1 = MakeRegionSound(FILENAME1, 5 * tmpSampleRate, tmpSampleRate * 20);
    AudioStream stream2 = MakeRegionSound(FILENAME2, 5 * tmpSampleRate, tmpSampleRate * 20);
    StartSound(gAudioPlayer, stream1, symb3);
    StopSound(gAudioPlayer, stream1, symb4);
    StartSound(gAudioPlayer, stream2, symb4);
}

SymbolicDate symb5 = GenSymbolicDate(gAudioPlayer);
SymbolicDate symb6 = GenSymbolicDate(gAudioPlayer);

static SymbolicDate SeqWithEvent(AudioStream s1, AudioStream s2)
{
    SymbolicDate symb = GenSymbolicDate(gAudioPlayer);
    StartSound(gAudioPlayer, s1, GenRealDate(gAudioPlayer, 0));
    StopSound(gAudioPlayer, s1, symb);
    StartSound(gAudioPlayer, s2, symb);
    return symb;
}

static void test20()
{
    AudioStream stream1 = MakeRegionSound(FILENAME1, 5 * tmpSampleRate, tmpSampleRate * 20);
    AudioStream stream2 = MakeRegionSound(FILENAME2, 5 * tmpSampleRate, tmpSampleRate * 20);
    
    StartSound(gAudioPlayer, stream1, symb5);
    StartSound(gAudioPlayer, stream2, symb5);
    
    StopSound(gAudioPlayer, stream1, symb6);
    StopSound(gAudioPlayer, stream2, symb6);
}

static void test21()
{
    audio_frames_t curdate = getCurDate();
    printf("info.Frames %lld\n", curdate);
    
    //StopAudioPlayer(gAudioPlayer);
    
    // Clear effect
    ClearAudioPlayer(gAudioPlayer);
                        
    for (int i = 0; i < 10; i++) {
         StartSound(gAudioPlayer, MakeRegionSound(FILENAME1, 5 * tmpSampleRate, tmpSampleRate * 10 + i), GenRealDate(gAudioPlayer, curdate + i*tmpSampleRate));
    }
    
    for (int i = 0; i < 10; i++) {
         StartSound(gAudioPlayer, MakeRegionSound(FILENAME2, 2 * tmpSampleRate, tmpSampleRate * 10 + i), GenRealDate(gAudioPlayer, curdate + i*tmpSampleRate));
    }
    
    for (int i = 0; i < 10; i++) {
         StartSound(gAudioPlayer, 
            MakeEffectSound(MakeRegionSound(FILENAME2, 2 * tmpSampleRate, tmpSampleRate * 11 + i), MakeFaustAudioEffect(LLVM_EFFECT1, "", ""), 100, 100),
            GenRealDate(gAudioPlayer, curdate + i*tmpSampleRate));
    }
    
    for (int i = 100; i > 0; i--) {
        SetTimedControlValueEffect(gAudioPlayer, "freeverb", "/Freeverb/Wet", float(i)*0.01f, GenRealDate(gAudioPlayer, curdate + tmpSampleRate*5+i*4410));
    }
}

static void test22()
{
    audio_frames_t curdate = getCurDate();
    printf("info.Frames %lld\n", curdate);
    
     // Clear effect
    ClearAudioPlayer(gAudioPlayer);
    
    AudioStream stream1 = MakeEffectSound(MakeRegionSound(FILENAME1, 3 * tmpSampleRate, tmpSampleRate * 200), MakeFaustAudioEffect(LLVM_EFFECT1, "", ""), 100, 100);
    StartSound(gAudioPlayer, stream1, GenRealDate(gAudioPlayer, curdate + tmpSampleRate*2));
    //StopSound(gAudioPlayer, stream1, GenRealDate(gAudioPlayer, tmpSampleRate * 4));
    
    printf("faust_effect1 NAME %s\n", GetNameEffect(faust_effect1));
    
    for (int i = 0; i < 2000; i++) {
        SetTimedControlValueEffect(gAudioPlayer, "freeverb", "/Freeverb/Wet", float(i)*0.0005f, GenRealDate(gAudioPlayer, curdate + tmpSampleRate*3+i*2*44));
        SetTimedControlValueEffect(gAudioPlayer, "freeverb", "/Freeverb/RoomSize", float(i)*0.0005f, GenRealDate(gAudioPlayer, curdate + tmpSampleRate*3+i*2*44));
    }
    
    for (int i = 0; i < 2000; i++) {
        SetTimedControlValueEffect(gAudioPlayer, "freeverb", "/Freeverb/Wet", 1.f-float(i)*0.0005f, GenRealDate(gAudioPlayer, curdate + tmpSampleRate*13+i*2*44));
        SetTimedControlValueEffect(gAudioPlayer, "freeverb", "/Freeverb/RoomSize", 1.f-float(i)*0.0005f, GenRealDate(gAudioPlayer, curdate + tmpSampleRate*13+i*2*44));
    }
    
    for (int i = 0; i < 2000; i++) {
        SetTimedControlValueEffect(gAudioPlayer, "freeverb", "/Freeverb/Wet", float(i)*0.0005f, GenRealDate(gAudioPlayer, curdate + tmpSampleRate*23+i*2*44));
        SetTimedControlValueEffect(gAudioPlayer, "freeverb", "/Freeverb/RoomSize", float(i)*0.0005f, GenRealDate(gAudioPlayer, curdate + tmpSampleRate*23+i*2*44));
    }
    
    for (int i = 0; i < 2000; i++) {
        SetTimedControlValueEffect(gAudioPlayer, "freeverb", "/Freeverb/Wet", 1.f-float(i)*0.0005f, GenRealDate(gAudioPlayer, curdate + tmpSampleRate*33+i*2*44));
        SetTimedControlValueEffect(gAudioPlayer, "freeverb", "/Freeverb/RoomSize", 1.f-float(i)*0.0005f, GenRealDate(gAudioPlayer, curdate + tmpSampleRate*33+i*2*44));
    }

}

static void test23()
{
    audio_frames_t curdate = getCurDate();
    printf("info.Frames %lld\n", curdate);
    
    AudioStream stream1 = MakeCutSound(MakeEffectSound(MakeRegionSound(FILENAME2, 2 * tmpSampleRate, tmpSampleRate * 20), faust_effect1, 100, 100), 0, tmpSampleRate * 21);
    StartSound(gAudioPlayer, stream1, GenRealDate(gAudioPlayer, 0));
    //StopSound(gAudioPlayer, stream1, GenRealDate(gAudioPlayer, tmpSampleRate * 4));
    
    printf("faust_effect1 NAME %s\n", GetNameEffect(faust_effect1));
    
    for (int i = 0; i < 100; i++) {
        long res = SetTimedControlValueEffect(gAudioPlayer, "freeverb", "/Freeverb/Wet", float(i)*0.01f, GenRealDate(gAudioPlayer, curdate + tmpSampleRate*5+i*4410));
    }
}

static void test24()
{
    audio_frames_t curdate = getCurDate();
    printf("info.Frames %lld\n", curdate);
    
    AudioStream stream1 = MakeEffectSound(MakeRegionSound(FILENAME1, 5 * tmpSampleRate, tmpSampleRate * 70), MakeFaustAudioEffect(LLVM_EFFECT1, "", ""), 100, 100);
    AudioStream stream2 = MakeEffectSound(MakeRegionSound(FILENAME2, 0, tmpSampleRate * 70), MakeFaustAudioEffect(LLVM_EFFECT1, "", ""), 100, 100);
    AudioStream stream3 = MakeParSound(stream1, stream2);
    
    StartSound(gAudioPlayer, stream3, GenRealDate(gAudioPlayer, 0));
    
    printf("faust_effect1 NAME %s\n", GetNameEffect(faust_effect1));
    printf("faust_effect2 NAME %s\n", GetNameEffect(faust_effect2));
    
    for (int i = 0; i < 100; i++) {
        long res = SetTimedControlValueEffect(gAudioPlayer, "freeverb", "/Freeverb/Wet", float(i)*0.01f, GenRealDate(gAudioPlayer, curdate + tmpSampleRate*5+i*4410));
    }
}

static void test25()
{
    audio_frames_t curdate = getCurDate();
    printf("info.Frames %lld\n", curdate);
    
    AudioStream stream1 = MakeRegionSound(FILENAME1, 5 * tmpSampleRate, tmpSampleRate * 70);
    AudioStream stream2 = MakeRegionSound(FILENAME2, 0, tmpSampleRate * 70);
    AudioStream stream3 = MakeEffectSound(MakeParSound(stream1, stream2), MakeFaustAudioEffect(LLVM_EFFECT1, "", ""), 100, 100);
    
    StartSound(gAudioPlayer, stream3, GenRealDate(gAudioPlayer, 0));
    
    printf("faust_effect1 NAME %s\n", GetNameEffect(faust_effect1));
    printf("faust_effect2 NAME %s\n", GetNameEffect(faust_effect2));
    
    for (int i = 0; i < 100; i++) {
        long res = SetTimedControlValueEffect(gAudioPlayer, "freeverb", "/Freeverb/Wet", float(i)*0.01f, GenRealDate(gAudioPlayer, curdate + tmpSampleRate*5+i*4410));
    }
}

SymbolicDate symb7 = GenSymbolicDate(gAudioPlayer);

static void test26()
{
    audio_frames_t curdate = getCurDate();
    printf("info.Frames %lld\n", curdate);
    
    AudioStream stream1 = MakeRegionSound(FILENAME1, 5 * tmpSampleRate, tmpSampleRate * 200);
    
    StartSound(gAudioPlayer, stream1, GenRealDate(gAudioPlayer, curdate));
    StopSound(gAudioPlayer, stream1, symb7);
    
}

int main(int argc, char* argv[])
{
    gAudioPlayer = OpenAudioPlayer(tmpInChan, tmpOutChan, tmpSampleRate, tmpBufferSize, 65536 * 4, tmpSampleRate * 60 * 10, kJackRenderer, 1);
    //gAudioPlayer = OpenAudioPlayer(tmpInChan, tmpOutChanmake build ARCH=x86-64, tmpSampleRate, tmpBufferSize, 65536 * 4, tmpSampleRate * 60 * 10, kCoreAudioRenderer, 1);
    if (!gAudioPlayer) {
        printf("Cannot OpenAudioPlayer\n");
        return 0;
    } 
    
    StartAudioPlayer(gAudioPlayer);
    
    sleep(1);
     
    AudioRendererPtr renderer = GetAudioPlayerRenderer(gAudioPlayer);
    
    RendererInfo info;
    GetAudioRendererInfo(renderer, &info);
    
    TAudioClient* audio_client = new TAudioLASClient();
    AddAudioClient(renderer, audio_client); 

    //SetControlValueEffect(faust_effect1, 0, 0.99);
    //SetControlValueEffect(faust_effect1, 1, 0.99);
    //SetControlValueEffect(faust_effect1, 2, 0.99);
    
    printControls(faust_effect1);
    printControls(faust_effect2);
    
    //test0();
    
    //test1();
    
    //test2();
    //test3();
    //test4();
    //test5();
    //test6();
    //test7();
    
    //test8();
    //test9();
    
    //test10();
    //test11();
    //test12();
    //test13();
    
    //test14();
    
    //test16();
    
    //test17();
    //test18();
    //test19();
    //test20();
    //test21();
    test22();
    //test23();
    //test24();
    //test25();
    //test26();
       
    //StartAudioPlayer(gAudioPlayer);
    /*
    SetControlValueEffect(faust_effect1, 0, 0.9);
    SetControlValueEffect(faust_effect1, 1, 0.9);
    SetControlValueEffect(faust_effect1, 2, 0.9);
    
    SetControlValueEffect(faust_effect2, 9, 0.9);
    SetControlValueEffect(faust_effect2, 10, 0.9);
    
    SetControlValueEffect(faust_effect4, 0, 0.9);
    SetControlValueEffect(faust_effect4, 1, 0.9);
    SetControlValueEffect(faust_effect4, 2, 0.9);
    
    SetControlValueEffect(faust_effect5, 0, 0.9);
    */
    
    printf("faust_effect1 %s\n", GetNameEffect(faust_effect1));
    printf("faust_effect2 %s\n", GetNameEffect(faust_effect2));
    printf("faust_effect4 %s\n", GetNameEffect(faust_effect4));
    printf("faust_effect5 %s\n", GetNameEffect(faust_effect5));
    
     
    char c;
    printf("Type 'q' to quit\n");

    while ((c = getchar()) && c != 'q') {
        switch (c) {
        
            case 'p':
                StartSound(gAudioPlayer, MakeEffectSound(MakeRegionSound(FILENAME1, 5 * tmpSampleRate, tmpSampleRate * 13), MakeFaustAudioEffect(LLVM_EFFECT1, "", ""), 100, 100), 0);
                break;
                
             case 'o':
                StartSound(gAudioPlayer, MakeEffectSound(MakeRegionSound(FILENAME2, 5 * tmpSampleRate, tmpSampleRate * 13), MakeFaustAudioEffect(LLVM_EFFECT1, "", ""), 100, 100), 0);
                break;
                
            case 'n': {
                 
                audio_frames_t curdate = getCurDate();
                printf("info.Frames %lld\n", curdate);
                
                StartSound(gAudioPlayer, MakeRegionSound(FILENAME1, 5*tmpSampleRate, 7*tmpSampleRate), GenRealDate(gAudioPlayer, curdate + 1*tmpSampleRate));
                StartSound(gAudioPlayer, MakeRegionSound(FILENAME2, 5*tmpSampleRate, 7*tmpSampleRate), GenRealDate(gAudioPlayer, curdate + 2*tmpSampleRate));
                break;
            }
                
            case 's': {
                  
                audio_frames_t curdate = getCurDate();
                printf("info.Frames %lld\n", curdate);
                
                SetSymbolicDate(gAudioPlayer, symb1, curdate + 1*tmpSampleRate);
                SetSymbolicDate(gAudioPlayer, symb2, curdate);
                SetSymbolicDate(gAudioPlayer, symb3, curdate);
                SetSymbolicDate(gAudioPlayer, symb4, curdate + 10*tmpSampleRate);
                
                SetSymbolicDate(gAudioPlayer, symb5, curdate);
                SetSymbolicDate(gAudioPlayer, symb6, curdate + 5*tmpSampleRate);
                 
                break;
            }
            
            case 'a': {
                test21();
                break;
            }
            
            case 'b': {
                audio_frames_t curdate = getCurDate();
                printf("info.Frames %lld\n", curdate);
                SetSymbolicDate(gAudioPlayer, symb7, curdate);
                StartSound(gAudioPlayer, MakeCutSound(MakeRegionSound(FILENAME1, 5 * tmpSampleRate, tmpSampleRate * 200), 0, GetSymbolicDate(gAudioPlayer, symb7)), symb7);
                break;
            }
        
        }
        sleep(0.01);
    }
    
    RemoveAudioClient(renderer, audio_client);
    CloseAudioPlayer(gAudioPlayer);
    
}