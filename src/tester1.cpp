
#include <stdio.h>
#include "LibAudioStream++.h"

#define LLVM_EFFECT1 "/Documents/faust-sf/examples/freeverb.dsp"

class TAudioClient
{

    public:

        TAudioClient()
        {}
        virtual ~TAudioClient()
        {}

		/*!
		\brief Audio callback called by the AudioManager.
		\param inputBuffer The input buffer as a array of interleaved float samples (stereo here).
		\param outputBuffer The output buffer as a array of interleaved float samples (stereo here).
		\param frames The input/output buffer number of frames.
		\return true if success, false otherwise.
		*/
        virtual bool AudioCallback(float** inputBuffer, float** outputBuffer, long frames) = 0;
};

typedef TAudioClient * TAudioClientPtr;

class TAudioLASClient : public TAudioClient {


    public:

        TAudioLASClient()
        {}
        virtual ~TAudioLASClient()
        {}

	    virtual bool AudioCallback(float** inputBuffer, float** outputBuffer, long frames)
        {
            printf("AudioCallback frames = %d\n", frames);
        }
};

#define MAX_CHAN 2

// Global context
static AudioRendererPtr gAudioRenderer = 0;
static AudioPlayerPtr gAudioPlayer = 0;
static long gSampleRate = 0;
static long gBufferSize = 0;
static TAudioLASClient* gAudioClient = 0;

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
    long tmpInChan = 2;
    long tmpOutChan = 2;
    long tmpBufferSize = 512;
    long tmpSampleRate = 44100;
    
    long inputDevice = 2;
    long ouputDevice = 2;

    AudioGlobalsInit(tmpInChan, tmpOutChan, MAX_CHAN, tmpSampleRate, tmpBufferSize, 65536, 65536, 1);

    /*
    // Try access Jack server
    gAudioRenderer = MakeAudioRenderer(kJackRenderer);
    
    if (gAudioRenderer && (OpenAudioRenderer(gAudioRenderer, inputDevice, ouputDevice, tmpInChan, tmpOutChan, tmpBufferSize, tmpSampleRate) == NO_ERR)) {
        gSampleRate = tmpSampleRate;
        gBufferSize = tmpBufferSize;
        goto player;
    } else {
        if (gAudioRenderer) {
            CloseAudioRenderer(gAudioRenderer);
            DeleteAudioRenderer(gAudioRenderer);
        }
    }
    */

    // Otherwise use PortAudio API
    gAudioRenderer = MakeAudioRenderer(kCoreAudioRenderer);
    if (gAudioRenderer && (OpenAudioRenderer(gAudioRenderer, inputDevice, ouputDevice, tmpInChan, tmpOutChan, tmpBufferSize, tmpSampleRate) == NO_ERR)) {
        gSampleRate = tmpSampleRate;
        gBufferSize = tmpBufferSize;
         goto player;
    } else {
        if (gAudioRenderer) {
            CloseAudioRenderer(gAudioRenderer);
            DeleteAudioRenderer(gAudioRenderer);
        }
    }

player:

    // Open mixer client
    //gAudioPlayer = OpenAudioClient(gAudioRenderer);
    
    AudioEffectList list_effect = MakeAudioEffectList();
	AudioEffect faust_effect = MakeFaustAudioEffect(LLVM_EFFECT1, "", "");
    list_effect = AddAudioEffect(list_effect, faust_effect);
    
    SetControlValueEffect(faust_effect, 0, 0.9);
    SetControlValueEffect(faust_effect, 1, 0.9);
    SetControlValueEffect(faust_effect, 2, 0.9);
  	 
    // Add in/out client
    gAudioClient = new TAudioLASClient();
    AddAudioClient(gAudioRenderer, gAudioClient); 
    
    int devices = GetDeviceCount(gAudioRenderer);
    
    printf("devices = %d\n", devices);

    // Start audio rendering...
    StartAudioRenderer(gAudioRenderer); 
    
    char c;

    while ((c = getchar()) && c != 'q') {
        sleep(1);
    }
    
    // Stop audio rendering...
    StopAudioRenderer(gAudioRenderer); 
    
    // Remove in/out client
    RemoveAudioClient(gAudioRenderer, gAudioClient); 
    
    // Close mixer client
    //CloseAudioClient(gAudioPlayer);

    AudioGlobalsDestroy();
}