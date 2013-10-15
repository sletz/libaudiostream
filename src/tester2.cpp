
#include <stdio.h>
#include "LibAudioStreamMC++.h"

#define LLVM_EFFECT1 "/Documents/faust-sf/examples/freeverb.dsp"

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
	AudioEffect faust_effect = MakeFaustAudioEffect(LLVM_EFFECT1, "", "");
    //list_effect = AddAudioEffect(list_effect, faust_effect);
    
    SetControlValueEffect(faust_effect, 0, 0.9);
    SetControlValueEffect(faust_effect, 1, 0.9);
    SetControlValueEffect(faust_effect, 2, 0.9);
    
    printControls(faust_effect);
    
    AudioStream stream = MakeRegionSound(FILENAME1, 0, tmpSampleRate * 15);
    
    AddSound(gAudioPlayer, stream);
    
    StartAudioPlayer(gAudioPlayer);
     
    char c;
    printf("Type 'q' to quit\n");

    while ((c = getchar()) && c != 'q') {
        sleep(1);
    }
    
}