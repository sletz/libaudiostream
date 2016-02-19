/*

Copyright (C) Grame 2002-2014

This library is free software; you can redistribute it and modify it under
the terms of the GNU Library General Public License as published by the
Free Software Foundation version 2 of the License, or any later version.

This library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public License
for more details.

You should have received a copy of the GNU Library General Public License
along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

Grame Research Laboratory, 9, rue du Garet 69001 Lyon - France
research@grame.fr

*/

#include "LibAudioStream++.h"
#include <sndfile.h>
#include <stdio.h>
#include <errno.h>

#ifdef WIN32
	//#define FILENAME1 "C:\\Documents and Settings\\letz\\Mes documents\\Ma Musique\\s1.wav"
	#define FILENAME1 "C:\\Documents and Settings\\letz\\Mes documents\\Ma Musique\\����.wav"
	#define FILENAME2 "C:\\Documents and Settings\\letz\\Mes documents\\Ma Musique\\s2.wav"
	#define FILENAME3 "D:\\acl62-trial\\BjornenSover.wav"
	#define EFFECT1 "D:\\acl62-trial\\freeverb.dll"
#else
	//#define FILENAME1 "/Users/letz/Music/Sounds/levot-882.wav"
    #define FILENAME1 "/Users/letz/Music/Sounds/levot.wav"
	#define FILENAME2 "/Users/letz/Music/Sounds/tango.wav"
	#define FILENAME3 "/Users/letz/son1.wav"
	#define FILENAME4 "/Users/letz/Music/Sounds/levot-mono.aiff"
	#define EFFECT1 ""
    //#define LLVM_EFFECT1 "process = component(\"effect.lib\").zita_rev1;"
    #define LLVM_EFFECT1 "/Documents/faust-sf/examples/freeverb.dsp"
    //#define LLVM_EFFECT1 "process = _,_;"
#endif


#define IN_CHANNELS 2	// stereo player
//#define IN_CHANNELS 0	// stereo player
#define OUT_CHANNELS 2	// stereo player
#define CHANNELS 8

#define SAMPLE_RATE 44100

AudioEffect faust_effect = 0;

AudioPlayerPtr player;

static void TestCallback(void* context)
{
	printf("Channel stopped\n");
}

double pitch_shift = 1.0;
double time_strech = 1.0;

void printControls(AudioEffect faust_effect)
{
    printf("Faust effect: param num %ld\n", GetControlCountEffect(faust_effect));
    for (int i = 0; i < GetControlCountEffect(faust_effect); i++) {
        float min, max, init;
        char label[32];
        GetControlParamEffect(faust_effect, i, label, &min, &max, &init); 
        printf("Faust effect: param label = %s min = %f max = %f init = %f value = %f\n", label, min, max, init, GetControlValueEffect(faust_effect, i));
    }
}

AudioStream test0()
{
    printf("-------------- \n");
    printf("Build a region \n");
    printf("-------------- \n\n");
    AudioStream s1;
    //s1 = MakeReadSound(FILENAME1);
    s1 = MakeReadSound(FILENAME4);
    return s1;
}

AudioStream test1()
{
    printf("--------------------------- \n");
    printf("Build a region with a fade  \n");
    printf("--------------------------- \n\n");
    AudioStream s1;
    s1 = MakeRegionSound(FILENAME1, 200000, 500000);
    return MakeFadeSound(s1, 44100, 44100);
}

AudioStream test2()
{
    printf("------------------------------------- \n");
    printf("Build a mix of 2 regions with a fade \n");
    printf("------------------------------------- \n\n");
    AudioStream s1, s2;
    s1 = MakeRegionSound(FILENAME1, 400000, 1700000);
    s2 = MakeRegionSound(FILENAME2, 400000, 1800000);
    return MakeFadeSound(MakeMixSound(s2, s1), 44100, 44100);
}

AudioStream test3()
{
    printf("-----------------------------------------------\n");
    printf("Build a sequence of 2 regions with a crossfade \n");
    printf("-----------------------------------------------\n\n");
    AudioStream s1, s2;
    s1 = MakeRegionSound(FILENAME1, 200000, 500000);
    s2 = MakeRegionSound(FILENAME2, 200000, 700000);
    return MakeSeqSound(s1, s2, 88200);
}

AudioStream test4()
{
    printf("----------------------\n");
    printf("Build a looped region \n");
    printf("----------------------\n\n");
    AudioStream s1;
    s1 = MakeRegionSound(FILENAME1, 200000, 400000);
    s1 = MakeFadeSound(s1, 500, 500);
    s1 = MakeLoopSound(s1, 5);
    return s1;
}

AudioStream test5()
{
    printf("------------------------------------\n");
    printf("Build a input/output thru stream \n");
    printf("------------------------------------\n\n");
    return MakeInputSound();
}

/*
AudioStream test5bis()
{
    printf("-----------------------------------------------\n");
    printf("Build a buffered input/output thru stream \n");
    printf("-----------------------------------------------\n\n");
    return MakeBufferedInputSound(10 * SAMPLE_RATE);
}

AudioStream test5ter()
{
    printf("----------------------------------------------------------------\n");
    printf("Build seq of a buffered input/output thru stream and a region\n");
    printf("----------------------------------------------------------------\n\n");
    return MakeSeqSound(MakeBufferedInputSound(10 * SAMPLE_RATE), MakeRegionSound(FILENAME1, 200000, 500000), 88200);
}


AudioStream test5ter1()
{
    printf("-----------------------------------------------\n");
    printf("Build a buffered input/output thru stream   \n");
    printf("-----------------------------------------------\n\n");
    return MakeMixSound(MakeSeqSound(MakeNullSound(SAMPLE_RATE * 3), MakeSharedBufferedInputSound(SAMPLE_RATE), 10000), MakeBufferedInputSound(20 * SAMPLE_RATE));
}
*/

AudioStream test5ter2()
{
    AudioEffectList list_effect = MakeAudioEffectList();
	faust_effect = MakeFaustAudioEffect(LLVM_EFFECT1, "", "");
    list_effect = AddAudioEffect(list_effect, faust_effect);
	
    printControls(faust_effect);
    
    SetControlValueEffect(faust_effect, 0, 0.9);
    SetControlValueEffect(faust_effect, 1, 0.9);
    SetControlValueEffect(faust_effect, 2, 0.9);
    
    printControls(faust_effect);
   
    printf("---------------------------------------------------\n");
    printf("Build delayed + effect version of the input stream \n");
    printf("---------------------------------------------------\n\n");
    return MakeSeqSound(MakeNullSound(SAMPLE_RATE * 1), MakeTransformSound(MakeSharedInputSound(), list_effect, 0, 0), 0);
    //return MakeSeqSound(MakeNullSound(SAMPLE_RATE * 1), MakeSharedBufferedInputSound(0), 0);
    //return MakeTransformSound(MakeSharedBufferedInputSound(0), list_effect, 100, 100);
    //return MakeSharedBufferedInputSound(0);
}

AudioStream test5ter3()
{
    AudioEffectList list_effect1 = MakeAudioEffectList();
	faust_effect = MakeFaustAudioEffect(LLVM_EFFECT1, "", "");
    list_effect1 = AddAudioEffect(list_effect1, faust_effect);
 	
    printControls(faust_effect);
    
    SetControlValueEffect(faust_effect, 0, 0.9);
    SetControlValueEffect(faust_effect, 1, 0.9);
    SetControlValueEffect(faust_effect, 2, 0.9);
    
    printControls(faust_effect);
   
    printf("-------------------------------------------------\n");
    printf("Build sequence of parts of the input stream      \n");
    printf("-------------------------------------------------\n\n");
    
    /*
    return MakeSeqSound(MakeCutSound(MakeSharedInputSound(), 0, 5*SAMPLE_RATE),
                        MakeCutSound(MakeTransformSound(MakeSharedInputSound(), list_effect1, 0, 0), SAMPLE_RATE, 5*SAMPLE_RATE), 0);
    
    */
    /*
    return MakeSeqSound(MakeCutSound(MakeSharedInputSound(), 0, 5*SAMPLE_RATE),
                        MakeTransformSound(MakeCutSound(MakeSharedInputSound(), 2*SAMPLE_RATE, 5*SAMPLE_RATE), list_effect1, 0, 0), 0);
                        
    */
    /*
    return MakeSeqSound(MakeCutSound(MakeSharedInputSound(), 0, 5*SAMPLE_RATE),
                        MakeCutSound(MakeSharedInputSound(), 2*SAMPLE_RATE, 5*SAMPLE_RATE), 0);
    */
    
    return MakeSeqSound(MakeCutSound(MakeSharedInputSound(), 0, 5*SAMPLE_RATE),
                        MakeCutSound(MakeSharedInputSound(), 0*SAMPLE_RATE, 5*SAMPLE_RATE), 0);
   
}

AudioStream test5ter4()
{
    
    AudioEffectList list_effect1 = MakeAudioEffectList();
	faust_effect = MakeFaustAudioEffect(LLVM_EFFECT1, "", "");
    list_effect1 = AddAudioEffect(list_effect1, faust_effect);
 	
    printControls(faust_effect);
    
    SetControlValueEffect(faust_effect, 0, 0.9);
    SetControlValueEffect(faust_effect, 1, 0.9);
    SetControlValueEffect(faust_effect, 2, 0.9);
    
    printControls(faust_effect);
   
    printf("-----------------------------------------------\n");
    printf("Takes current portion of the input stream      \n");
    printf("-----------------------------------------------\n\n");
    
    // Remove portion of the input stream from the beginning
    RendererInfo info;
    AudioRendererPtr renderer = GetAudioPlayerRenderer(player);
    GetAudioRendererInfo(renderer, &info);
    
    printf("info.fCurFrame %d\n", info.fCurFrame);
 
    return MakeTransformSound(MakeCutSound(MakeSharedInputSound(), 0, info.fCurFrame + 1), list_effect1, 0, 0);
}

AudioStream test6()
{
    printf("---------------------------------------------------------\n");
    printf("Build a input/output thru and record the output stream\n");
    printf("---------------------------------------------------------\n\n");
    // "Wav" format can be read while being written...
    return MakeWriteSound("input.wav", MakeInputSound(), SF_FORMAT_WAV | SF_FORMAT_PCM_16);
}

AudioStream test7()
{
    printf("-------------------------\n");
    printf("Build a echo on a region \n");
    printf("-------------------------\n\n");
    AudioStream sound, mix = MakeRegionSound(FILENAME2, 20000, 60000);
    for (int i = 0; i < 10 ; i++) {
        sound = MakeSeqSound(MakeNullSound(i * 4410), MakeRegionSound(FILENAME2, 20000, 60000), 0);
		if (sound == 0) {
			printf("Error1 %i= \n", i);
        }
        mix = MakeMixSound(sound, mix);
		if (mix == 0) {
			printf("Error2 %i= \n", i);
        }
    }
    return mix;
}

AudioStream test8()
{
    printf("------------------------------------------------------------------\n");
    printf("Cut a region into a complex stream and write the result to a file\n");
    printf("------------------------------------------------------------------\n\n");
    AudioStream sound, mix = MakeRegionSound(FILENAME2, 400000, 500000);
    for (int i = 0; i < 8 ; i++) {
        sound = MakeSeqSound(MakeNullSound(i * 44000), MakeRegionSound(FILENAME2, 400000, 500000), 0);
        mix = MakeMixSound(sound, mix);
    }
    return MakeWriteSound("output.aif", MakeCutSound(mix, 100000, 200000), SF_FORMAT_AIFF | SF_FORMAT_PCM_16);
}

AudioStream test9()
{
    printf("--------------------------------------------------------------\n");
    printf("Sequence of a region with a transformed region (volume effect)\n");
    printf("--------------------------------------------------------------\n\n");
    AudioStream sound1 = MakeRegionSound(FILENAME1, 400000, 600000);
    AudioStream sound2 = MakeRegionSound(FILENAME1, 44100, 150000);
    AudioEffectList list_effect = MakeAudioEffectList();
	ClearAudioEffectList(list_effect);
	list_effect = AddAudioEffect(list_effect, MakeVolAudioEffect(0.1));
	return MakeSeqSound(sound1, MakeTransformSound(sound2, list_effect, 100, 100), 44100);
}

AudioStream test9bis()
{
    printf("--------------------------------------------------------------\n");
    printf("Sequence of a region with a pitched region (pitchshift effect)\n");
    printf("--------------------------------------------------------------\n\n");
    AudioStream sound1 = MakeRegionSound(FILENAME1, 400000, 700000);
    AudioStream sound2 = MakeRegionSound(FILENAME1, 400000, 700000);
    AudioEffectList list_effect = MakeAudioEffectList();
	ClearAudioEffectList(list_effect);
	list_effect = AddAudioEffect(list_effect, MakePitchShiftAudioEffect(0.8));
	return MakeSeqSound(sound1, MakeTransformSound(sound2, list_effect, 100, 100), 44100);
}

AudioStream test10()
{
    printf("-------------------------------------------------------------------\n");
    printf("Sequence of a region with a Faust freeverb region                  \n");
    printf("-------------------------------------------------------------------\n\n");
    AudioStream sound1 = MakeRegionSound(FILENAME1, 400000, 1000000);
    AudioStream sound2 = MakeRegionSound(FILENAME1, 400000, 1000000);
	AudioEffectList list_effect = MakeAudioEffectList();
    faust_effect = MakeFaustAudioEffect(EFFECT1, "", "");
    
    printControls(faust_effect);
   	
	list_effect = AddAudioEffect(list_effect, faust_effect);
	list_effect = AddAudioEffect(list_effect, MakeVolAudioEffect(0.5));
    return MakeSeqSound(sound1, MakeTransformSound(sound2, list_effect, 100, 100), 44100);
}

AudioStream test10bis()
{
    printf("-------------------------------------------------------------------\n");
    printf("Sequence of a region with a Faust LLVM reverb region                  \n");
    printf("-------------------------------------------------------------------\n\n");
    AudioStream sound1 = MakeRegionSound(FILENAME1, 400000, 1000000);
    AudioStream sound2 = MakeRegionSound(FILENAME1, 400000, 1000000);
    AudioEffectList list_effect = MakeAudioEffectList();
    faust_effect = MakeFaustAudioEffect(LLVM_EFFECT1, "", "");
    list_effect = AddAudioEffect(list_effect, faust_effect);
    
    printControls(faust_effect);
        
    SetControlValueEffect(faust_effect, 0, 1.0);
    SetControlValueEffect(faust_effect, 1, 1.0);
    SetControlValueEffect(faust_effect, 2, 1.0);
    
    printControls(faust_effect);
	
	//list_effect = AddAudioEffect(list_effect, MakeVolAudioEffect(0.99));
    return MakeSeqSound(sound1, MakeTransformSound(sound2, list_effect, 100, 100), 44100);
}

AudioStream test11()
{
    printf("-------------------------------------------------------------------\n");
    printf("Input stream + Faust freeverb effect                               \n");
    printf("-------------------------------------------------------------------\n\n");
    AudioEffectList list_effect = MakeAudioEffectList();
	faust_effect = MakeFaustAudioEffect(EFFECT1, "", "");
    list_effect = AddAudioEffect(list_effect, faust_effect);
	
    printControls(faust_effect);
    
    SetControlValueEffect(faust_effect, 0, 1.0);
    SetControlValueEffect(faust_effect, 1, 1.0);
    SetControlValueEffect(faust_effect, 2, 1.0);
    
    printControls(faust_effect);
	
    return MakeWriteSound("reverb_input.wav", MakeTransformSound(MakeInputSound(), list_effect, 100, 100), SF_FORMAT_WAV | SF_FORMAT_PCM_16);
}

AudioStream test11bis()
{
    printf("-------------------------------------------------------------------\n");
    printf("Input stream + Faust LLVM freeverb effect                               \n");
    printf("-------------------------------------------------------------------\n\n");
    AudioEffectList list_effect = MakeAudioEffectList();
	faust_effect = MakeFaustAudioEffect(LLVM_EFFECT1, "", "");
    list_effect = AddAudioEffect(list_effect, faust_effect);
	
    printControls(faust_effect);
    
    SetControlValueEffect(faust_effect, 0, 0.9);
    SetControlValueEffect(faust_effect, 1, 0.9);
    SetControlValueEffect(faust_effect, 2, 0.9);
    
    printControls(faust_effect);
  
    return MakeWriteSound("reverb_input.wav", MakeTransformSound(MakeInputSound(), list_effect, 100, 100), SF_FORMAT_WAV | SF_FORMAT_PCM_16);
}

AudioStream test11ter()
{
    printf("-------------------------------------------------------------------\n");
    printf("Input stream + Faust LLVM freeverb effect                               \n");
    printf("-------------------------------------------------------------------\n\n");
    AudioEffectList list_effect = MakeAudioEffectList();
	faust_effect = MakeFaustAudioEffect(LLVM_EFFECT1, "", "/Users/letz/SVG");
    list_effect = AddAudioEffect(list_effect, faust_effect);
	
    printControls(faust_effect);
    
    SetControlValueEffect(faust_effect, 0, 0.9);
    SetControlValueEffect(faust_effect, 1, 0.9);
    SetControlValueEffect(faust_effect, 2, 0.9);
    
    printControls(faust_effect);
  
    return MakeMixSound(
        MakeWriteSound("reverb_input.wav", MakeTransformSound(MakeInputSound(), list_effect, 100, 100), SF_FORMAT_WAV | SF_FORMAT_PCM_16), 
        MakeSeqSound(MakeNullSound(3 * 44100), MakeReadSound("reverb_input.wav"), 100));
}

AudioStream test11quad(const char* faust_code)
{
    printf("-------------------------------------------------------------------\n");
    printf("Input stream + Faust LLVM freeverb effect                               \n");
    printf("-------------------------------------------------------------------\n\n");
    AudioEffectList list_effect = MakeAudioEffectList();
	faust_effect = MakeFaustAudioEffect(faust_code, "", "/Users/letz/SVG");
    list_effect = AddAudioEffect(list_effect, faust_effect);
	
    printControls(faust_effect);
    
    SetControlValueEffect(faust_effect, 0, 0.9);
    SetControlValueEffect(faust_effect, 1, 0.9);
    SetControlValueEffect(faust_effect, 2, 0.9);
    
    printControls(faust_effect);
  
    return MakeMixSound(
        MakeWriteSound("reverb_input.wav", MakeTransformSound(MakeInputSound(), list_effect, 100, 100), SF_FORMAT_WAV | SF_FORMAT_PCM_16), 
        MakeSeqSound(MakeNullSound(3 * 44100), MakeReadSound("reverb_input.wav"), 100));
}

AudioStream test12()
{
    printf("-------------------------------------------------------------------\n");
    printf("RubberBand library (1)											   \n");
    printf("-------------------------------------------------------------------\n\n");
    
	AudioStream s1 = MakeReadSound(FILENAME1);
	return MakePitchSchiftTimeStretchSound(s1, &pitch_shift, &time_strech);
}

AudioStream test13()
{
    printf("-------------------------------------------------------------------\n");
    printf("RubberBand library (2)											   \n");
    printf("-------------------------------------------------------------------\n\n");
	
	AudioStream s1 = MakeRegionSound(FILENAME1, 200000, 500000);
    AudioStream s2 = MakeRegionSound(FILENAME2, 200000, 1700000);
    return MakeSeqSound(MakePitchSchiftTimeStretchSound(s1, &pitch_shift, &time_strech), MakePitchSchiftTimeStretchSound(s2, &pitch_shift, &time_strech), 88200);
}

void test20()
{
    printf("-----------------------------------------------------------\n");
    printf("Non real-time rendering : use the MakeRendererSound wrapper\n");
    printf("-----------------------------------------------------------\n\n");
    //AudioStream sound = MakeRendererSound(MakeRegionSound(FILENAME2, 400000, 500000));
	AudioStream sound = MakeRendererSound(MakeRegionSound(FILENAME2, 0, 1000));
    float buffer[512 * OUT_CHANNELS];
    long res;
    do {
        res = ReadSound(sound, buffer, 512, OUT_CHANNELS);
        printf("Simulate non real-time rendering : use buffer here %ld\n", res);
    } while (res == 512);
    printf("Simulate non real-time rendering : use last buffer here %ld\n", res);
}

void test21()
{
    printf("-----------------------------------------------------------\n");
    printf("Non real-time rendering : use the MakeRendererSound wrapper\n");
    printf("-----------------------------------------------------------\n\n");
    AudioStream sound = MakeRendererSound(MakeWriteSound("output.aif", MakeReadSound(FILENAME3), SF_FORMAT_AIFF | SF_FORMAT_PCM_16));
    float buffer[512 * OUT_CHANNELS];
    long res;
	do {
        res = ReadSound(sound, buffer, 512, OUT_CHANNELS);
        printf("Simulate non real-time rendering : use buffer here %ld\n", res);
    } while (res == 512);
    printf("Simulate non real-time rendering : use last buffer here %ld\n", res);	
}

void TestPlay(AudioPlayerPtr player)
{
    float vol = 1.0f;
    float panLeft = 1.0f;
	float panRight = 1.0f;
    char c;
    
    AudioRendererPtr renderer = GetAudioPlayerRenderer(player);
    
    // To reset real-time input
    //StopAudioPlayer(player);
    //StartAudioPlayer(player);
	
    while ((c = getchar()) && (c != 'n')) {

        switch (c) {

            case 'b':
                StopAudioPlayer(player);
                
                StartChannel(player, 1);
                StartAudioPlayer(player);
            break;

            case 'p':
                StopAudioPlayer(player);
               
                ContChannel(player, 1);
                StartAudioPlayer(player);
                break;

            case 's':
                StopChannel(player, 1);
                
                StopAudioPlayer(player);
                StartAudioPlayer(player);
                break;

            case '+':
                vol += 0.05f;
                //SetVolAudioPlayer(player, vol);
				time_strech += 0.1;
				printf("time_strech %f\n", time_strech);
                break;

            case '-':
                vol -= 0.05f;
                //SetVolAudioPlayer(player, vol);
				time_strech -= 0.1;
				printf("time_strech %f\n", time_strech);
                break;
				
			case '1':
                vol += 0.05f;
                //SetVolAudioPlayer(player, vol);
				pitch_shift += 0.1;
				printf("pitch_shift %f\n", pitch_shift);
                break;

            case '2':
                vol -= 0.05f;
                //SetVolAudioPlayer(player, vol);
				pitch_shift -= 0.1;
				printf("pitch_shift %f\n", pitch_shift);
                break;
                
            case 'w':
                RendererInfo info;
                GetAudioRendererInfo(renderer, &info);
                printf("info.fCurFrame %lld\n", info.fCurFrame);
                printf("info.fCurUsec %lld %f\n", info.fCurUsec, float(info.fCurUsec/1000000));
				break;

            /*
            case '1':
                panLeft += 0.05f;
				panRight -= 0.05f;
                SetPanAudioPlayer(player, panLeft, panRight);
                break;

            case '2':
				panLeft += 0.05f;
				panRight -= 0.05f;
                SetPanAudioPlayer(player, panLeft, panRight);
                break;
            */
			 case 'c': // To be used only when faust effects are running....
				SetControlValueEffect(faust_effect, 1, 0.95f);
				SetControlValueEffect(faust_effect, 2, 0.9f);
	            break;
        }
    }
}

/*
void SaveSound(AudioStream sound, char* name)
{
	AudioStream writesound = MakeRendererSound(MakeWriteSound(name, sound, SF_FORMAT_AIFF | SF_FORMAT_PCM_16));
	printf("GetChannelsSound %ld\n", GetChannelsSound(writesound));
	float buffer[512 * 2];
    long res;
    do {
        res = ReadSound(writesound, buffer, 512, GetChannelsSound(writesound));
        printf("Simulate non real-time rendering : use buffer here %ld\n", res);
    } while (res == 512);
}
*/

static void printError(int err)
{
    switch (err) {
        case 0:
            printf("NO_ERR\n");
            break;
        case -1:
            printf("OPEN_ERR\n");
            break;
        case -2:
            printf("CLOSE_ERR\n");
            break;
        case -3:
            printf("LOAD_ERR\n");
            break;
        case -4:
            printf("FILE_NOT_FOUND_ERR\n");
            break;
    
    }
}

static void ExecTest(AudioPlayerPtr player, AudioStream sound)
{
	printf("ExecTest channels = %ld \n", GetChannelsSound(sound));
    int err = LoadChannel(player, sound, 1, 1.0f, 1.0f, 0.0f);
	SetStopCallbackChannel(player, 1, TestCallback, NULL);
    if (err == NO_ERR) {
        TestPlay(player);
    } else {
        printf("LoadChannel error %d \n", err);
        printError(err);
    }
    StopChannel(player, 1);
}

int main(int argc, char* argv[])
{
    printf("----------------------------\n");
    printf("LibAudioStream based Player \n");
    printf("----------------------------\n\n");

	int res = LibVersion();
    
	// Try to open Jack version
    player = OpenAudioPlayer(IN_CHANNELS, OUT_CHANNELS, CHANNELS, SAMPLE_RATE, 512, 65536 * 8, SAMPLE_RATE * 60 * 10, kJackRenderer, 1);
    // If failure opens PortAudio version
    if (!player) {
        player = OpenAudioPlayer(IN_CHANNELS, OUT_CHANNELS, CHANNELS, SAMPLE_RATE, 1024, 65536 * 8, SAMPLE_RATE * 60 * 10, kPortAudioRenderer, 1);
    }
    // If failure opens CoreAudio version
    if (!player) {
        player = OpenAudioPlayer(IN_CHANNELS, OUT_CHANNELS, CHANNELS, SAMPLE_RATE, 1024, 65536 * 8, SAMPLE_RATE * 60 * 10, kCoreAudioRenderer, 1);
    }
    
    if (!player) {
        printf("Cannot open AudioPlayer...\n");
        return -1;
    } 
    
    //StartAudioPlayer(player);
	
    printf("Type 'b' to start playing from the begining\n");
    printf("Type 's' to stop playing\n");
    printf("Type 'p' to play from the current position\n");
    printf("Type '+' to raise volume\n");
    printf("Type '-' to lower volume\n");
    printf("Type '1' to pan left\n");
    printf("Type '2' to pan right\n");
    printf("Type 'n' to go to next test\n");
    
   
	/*
    ExecTest(player, test0());
	ExecTest(player, test0());
	ExecTest(player, test0());
	ExecTest(player, test0());
	ExecTest(player, test0());
	ExecTest(player, test0());
	ExecTest(player, test0());
    */
    

    /*
    ExecTest(player, test0());
    ExecTest(player, test1());
	ExecTest(player, test1());
    ExecTest(player, test2());
    ExecTest(player, test3());
    ExecTest(player, test4());
    ExecTest(player, test5());
    ExecTest(player, test6());
    */
    
    /*
    //ExecTest(player, test5bis());
    //ExecTest(player, test5ter());
    ExecTest(player, test5ter1());
    */

    //ExecTest(player, test5ter2());
    //ExecTest(player, test5ter3());
    
    ExecTest(player, test5ter4());
	
    
    /*
	ExecTest(player, test7());
    ExecTest(player, test8());
    ExecTest(player, test9());
	ExecTest(player, test9bis());
	//ExecTest(player, test10());
    ExecTest(player, test10bis());
	//ExecTest(player, test11());
    ExecTest(player, test11bis());
    */
    
    /*
    ExecTest(player, test10bis());
    
    ExecTest(player, test11ter());
    ExecTest(player, test11quad("process = _,_;"));
    ExecTest(player, test11quad("process = _,_;"));
    	
	ExecTest(player, test12());
	ExecTest(player, test13());
    */

    /*
	test20();
	test21();
    */
    
    //ExecTest(player, test0());
	
    StopAudioPlayer(player);
    CloseAudioPlayer(player);
    printf("Quit\n");
    return 0;
}

//int WinMain(int argc, char * argv[]) {}
