
#include <stdio.h>
#include "LibAudioStreamMC++.h"

#define LLVM_EFFECT1 "/Documents/faust-sf/examples/freeverb.dsp"
#define LLVM_EFFECT2 "/Documents/faust-sf/examples/zita_rev1.dsp"

#define CHANNELS 4

#define FILENAME1 "/Users/letz/Music/Sounds/levot.wav"
#define FILENAME2 "/Users/letz/Music/Sounds/tango.wav"
#define FILENAME3 "/Users/letz/son1.wav"
#define FILENAME4 "/Users/letz/Music/Sounds/levot-mono.aiff"

// Global context
static long gSampleRate = 0;
static long gBufferSize = 0;
static AudioRendererPtr gAudioRenderer = 0;
static AudioPlayerPtr gAudioPlayer = 0;

static void printControls(AudioEffect faust_effect)
{
    printf("Faust effect: param num %ld\n", GetControlCountEffect(faust_effect));
    for (int i = 0; i < GetControlCountEffect(faust_effect); i++) {
        float min, max, init;
        char label[32];
        GetControlParamEffect(faust_effect, i, label, &min, &max, &init); 
        printf("Faust effect: param label = %s min = %f max = %f init = %f value = %f\n", label, min, max, init, GetControlValueEffect(faust_effect, i));
    }
}

int main(int argc, char* argv[])
{
    long tmpInChan = 4;
    long tmpOutChan = 4;
    long tmpBufferSize = 512;
    long tmpSampleRate = 44100;
    
    gAudioPlayer = OpenAudioPlayer(tmpInChan, tmpOutChan, CHANNELS, tmpSampleRate, tmpBufferSize, 65536 * 4, tmpSampleRate * 60 * 10, kJackRenderer, 1);
    if (!gAudioPlayer) {
        printf("Cannot OpenAudioPlayer\n");
        return 0;
    } 

    //AudioEffectList list_effect = MakeAudioEffectList();
	AudioEffect faust_effect1 = MakeFaustAudioEffect(LLVM_EFFECT1, "", "");
    AudioEffect faust_effect2 = MakeFaustAudioEffect(LLVM_EFFECT2, "", "");
    //list_effect = AddAudioEffect(list_effect, faust_effect);
    
    SetControlValueEffect(faust_effect1, 0, 0.99);
    SetControlValueEffect(faust_effect1, 1, 0.99);
    SetControlValueEffect(faust_effect1, 2, 0.99);
    
    printControls(faust_effect1);
    printControls(faust_effect2);
    
    /*
    AudioStream stream1 = MakeRegionSound(FILENAME1, 5 * tmpSampleRate, tmpSampleRate * 15);
    AddSound(gAudioPlayer, stream1);

    AudioStream stream2 = MakeRegionSound(FILENAME2, 0, tmpSampleRate * 25);
    AddSound(gAudioPlayer, stream2);
    */
    
    /*
    AudioStream stream1 = MakeRegionSound(FILENAME1, 5 * tmpSampleRate, tmpSampleRate * 15);
    AudioStream stream2 = MakeRegionSound(FILENAME2, 0, tmpSampleRate * 25);
    AudioStream stream3 = MakeParSound(stream1, stream2);
    AddSound(gAudioPlayer, stream3);
    */
    
    /*
    AudioStream stream1 = MakeRegionSound(FILENAME4, 5 * tmpSampleRate, tmpSampleRate * 15);
    AudioStream stream2 = MakeRegionSound(FILENAME4, 7 * tmpSampleRate, tmpSampleRate * 15);
    
    AudioStream stream3 = MakeRegionSound(FILENAME2, 0, tmpSampleRate * 25);
    AudioStream stream4 = MakeParSound(stream1, MakeParSound(stream2, stream3));
    AddSound(gAudioPlayer, stream4);
    */
   
    /*
    AudioStream stream1 = MakeEffectSound(MakeRegionSound(FILENAME1, 5 * tmpSampleRate, tmpSampleRate * 50), faust_effect1, 100, 100);
    AddSound(gAudioPlayer, stream1);
    
    AudioStream stream2 = MakeEffectSound(MakeRegionSound(FILENAME2, 5 * tmpSampleRate, tmpSampleRate * 50), faust_effect2, 100, 100);
    AddSound(gAudioPlayer, stream2);
    */
    
    AudioStream stream1 = MakeEffectSound(MakeRegionSound(FILENAME1, 5 * tmpSampleRate, tmpSampleRate * 50), faust_effect1, 100, 100);
    AudioStream stream2 = MakeEffectSound(MakeRegionSound(FILENAME2, 5 * tmpSampleRate, tmpSampleRate * 50), faust_effect2, 100, 100);
    AudioStream stream3 = MakeParSound(stream1, stream2);
    AddSound(gAudioPlayer, stream3);
    //AddSound(gAudioPlayer, stream1);
    //RemoveSound(gAudioPlayer, stream3);
    //RemoveSound(gAudioPlayer, stream1);
    
    StartAudioPlayer(gAudioPlayer);
    
    SetControlValueEffect(faust_effect1, 0, 0.9);
    SetControlValueEffect(faust_effect1, 1, 0.9);
    SetControlValueEffect(faust_effect1, 2, 0.9);
    
    SetControlValueEffect(faust_effect2, 9, 0.9);
    SetControlValueEffect(faust_effect2, 10, 0.9);
     
    char c;
    printf("Type 'q' to quit\n");

    while ((c = getchar()) && c != 'q') {
        sleep(1);
    }
    
}