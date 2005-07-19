/*

Copyright © Grame 2002

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
grame@rd.grame.fr

*/


#include "LibAudioStream.h"
#include </opt/local/include/sndfile.h>
#include <stdio.h>

#define FILENAME1 "/Users/letz/levot.wav"
#define FILENAME2 "/Users/letz/tango.wav"
#define FILENAME3 "/Users/letz/son1.wav"

#define EFFECT1 "/Users/letz/freeverb.so"

#define IN_CHANNELS 2 // stereo player
#define OUT_CHANNELS 2 // stereo player
#define CHANNELS 8

AudioEffectPtr faust_effect = 0;

static void TestCallback(void* context)
{
	printf("Channel stopped\n");
}

AudioStreamPtr test0()
{
    printf("-------------- \n");
    printf("Build a region \n");
    printf("-------------- \n\n");
    AudioStreamPtr s1;
	s1 = MakeRegionSoundPtr(FILENAME1, 200000, 500000);
    return s1;
}

AudioStreamPtr test1()
{
    printf("--------------------------- \n");
    printf("Build a region with a fade \n");
    printf("--------------------------- \n\n");
    AudioStreamPtr s1;
    s1 = MakeRegionSoundPtr(FILENAME1, 200000, 500000);
    return MakeFadeSoundPtr(s1, 44100, 44100);
}

AudioStreamPtr test2()
{
    printf("------------------------------------- \n");
    printf("Build a mix of 2 regions with a fade \n");
    printf("------------------------------------- \n\n");
    AudioStreamPtr s1, s2;
    s1 = MakeRegionSoundPtr(FILENAME1, 400000, 1700000);
    s2 = MakeRegionSoundPtr(FILENAME2, 400000, 1800000);
    return MakeFadeSoundPtr(MakeMixSoundPtr(s2, s1), 44100, 44100);
}

AudioStreamPtr test3()
{
    printf("-----------------------------------------------\n");
    printf("Build a sequence of 2 regions with a crossfade \n");
    printf("-----------------------------------------------\n\n");
    AudioStreamPtr s1, s2;
    s1 = MakeRegionSoundPtr(FILENAME1, 200000, 500000);
    s2 = MakeRegionSoundPtr(FILENAME2, 200000, 700000);
    return MakeSeqSoundPtr(s1, s2, 88200);
}

AudioStreamPtr test4()
{
    printf("----------------------\n");
    printf("Build a looped region \n");
    printf("----------------------\n\n");
    AudioStreamPtr s1;
    s1 = MakeRegionSoundPtr(FILENAME1, 200000, 400000);
    s1 = MakeFadeSoundPtr(s1, 500, 500);
    s1 = MakeLoopSoundPtr(s1, 5);
    return s1;
}

AudioStreamPtr test5()
{
    printf("------------------------------------\n");
    printf("Build a input/output through stream \n");
    printf("------------------------------------\n\n");
    return MakeInputSoundPtr();
}

AudioStreamPtr test6()
{
    printf("---------------------------------------------------------\n");
    printf("Build a input/output through and record the ouput stream\n");
    printf("---------------------------------------------------------\n\n");
    return MakeWriteSoundPtr("input.aif", MakeInputSoundPtr(), SF_FORMAT_AIFF | SF_FORMAT_PCM_16);
}

AudioStreamPtr test7()
{
    printf("-------------------------\n");
    printf("Build a echo on a region \n");
    printf("-------------------------\n\n");
    AudioStreamPtr sound, mix = MakeRegionSoundPtr(FILENAME2, 400000, 1000000);
    for (int i = 0; i < 32 ; i++) {
        sound = MakeSeqSoundPtr(MakeNullSoundPtr(i * 4410), MakeRegionSoundPtr(FILENAME2, 400000, 1000000), 0);
        mix = MakeMixSoundPtr(sound, mix);
    }
    return mix;
}

AudioStreamPtr test8()
{
    printf("------------------------------------------------------------------\n");
    printf("Cut a region into a complex stream and write the result to a file\n");
    printf("------------------------------------------------------------------\n\n");
    AudioStreamPtr sound, mix = MakeRegionSoundPtr(FILENAME2, 400000, 500000);
    for (int i = 0; i < 8 ; i++) {
        sound = MakeSeqSoundPtr(MakeNullSoundPtr(i * 44000), MakeRegionSoundPtr(FILENAME2, 400000, 500000), 0);
        mix = MakeMixSoundPtr(sound, mix);
    }
    return MakeWriteSoundPtr("output.aif", MakeCutSoundPtr(mix, 100000, 200000), SF_FORMAT_AIFF | SF_FORMAT_PCM_16);
}

AudioStreamPtr test9()
{
    printf("--------------------------------------------------------------\n");
    printf("Sequence of a region with a transformed region (volume effect)\n");
    printf("--------------------------------------------------------------\n\n");
    AudioStreamPtr sound1 = MakeRegionSoundPtr(FILENAME1, 400000, 600000);
    AudioStreamPtr sound2 = MakeRegionSoundPtr(FILENAME1, 400000, 600000);
    AudioEffectListPtr list_effect = MakeAudioEffectListPtr();
    list_effect = AddAudioEffectPtr(list_effect, MakeVolAudioEffectPtr(0.25));
    return MakeSeqSoundPtr(sound1, MakeTransformSoundPtr(sound2, list_effect, 100, 100), 44100);
}

AudioStreamPtr test10()
{
    printf("-------------------------------------------------------------------\n");
    printf("Sequence of a region with a Faust freeverb region                  \n");
    printf("-------------------------------------------------------------------\n\n");
    AudioStreamPtr sound1 = MakeRegionSoundPtr(FILENAME1, 400000, 1000000);
    AudioStreamPtr sound2 = MakeRegionSoundPtr(FILENAME1, 400000, 1000000);
	AudioEffectListPtr list_effect = MakeAudioEffectListPtr();
    faust_effect = MakeFaustAudioEffectPtr(EFFECT1);
    
	printf("Faust effect: param num %ld\n", GetControlCountPtr(faust_effect));
	for (int i = 0; i < GetControlCountPtr(faust_effect); i++) {
		float min, max, init;
		char label[32];
		GetControlParamPtr(faust_effect, i, label, &min, &max, &init); 
		printf("Faust effect: param %s %f %f %f\n", label, min, max, init);
	}
	
	list_effect = AddAudioEffectPtr(list_effect, faust_effect);
	list_effect = AddAudioEffectPtr(list_effect, MakeVolAudioEffectPtr(0.5));
    return MakeSeqSoundPtr(sound1, MakeTransformSoundPtr(sound2, list_effect, 100, 100), 44100);
}

AudioStreamPtr test11()
{
    printf("-------------------------------------------------------------------\n");
    printf("Input stream + Faust freeverb effect                               \n");
    printf("-------------------------------------------------------------------\n\n");
    AudioEffectListPtr list_effect = MakeAudioEffectListPtr();
	faust_effect = MakeFaustAudioEffectPtr(EFFECT1);
	
    printf("Faust effect: param num %ld\n", GetControlCountPtr(faust_effect));
	for (int i = 0; i < GetControlCountPtr(faust_effect); i++) {
		float min, max, init;
		char label[32];
		GetControlParamPtr(faust_effect, i, label, &min, &max, &init); 
		printf("Faust effect: param %s %f %f %f\n", label, min, max, init);
	}
	
	list_effect = AddAudioEffectPtr(list_effect, faust_effect);
    return MakeTransformSoundPtr(MakeInputSoundPtr(), list_effect, 100, 100);
}

void test20()
{
    printf("-----------------------------------------------------------\n");
    printf("Non real-time rendering : use the MakeRendererSound wrapper\n");
    printf("-----------------------------------------------------------\n\n");
    AudioStreamPtr sound = MakeRendererSoundPtr(MakeRegionSoundPtr(FILENAME2, 400000, 500000));
    float buffer[512 * OUT_CHANNELS];
    long res;
    do {
        res = ReadSoundPtr(sound, buffer, 512, OUT_CHANNELS);
        printf("Simulate non real-time rendering : use buffer here %ld\n", res);
    } while (res == 512);
    printf("Simulate non real-time rendering : use last buffer here %ld\n", res);
	DeleteSoundPtr(sound);
}

void test21()
{
    printf("-----------------------------------------------------------\n");
    printf("Non real-time rendering : use the MakeRendererSound wrapper\n");
    printf("-----------------------------------------------------------\n\n");
    AudioStreamPtr sound = MakeRendererSoundPtr(MakeWriteSoundPtr("output.aif", MakeReadSoundPtr(FILENAME3),SF_FORMAT_AIFF | SF_FORMAT_PCM_16));
    float buffer[512 * OUT_CHANNELS];
    long res;
	do {
        res = ReadSoundPtr(sound, buffer, 512, OUT_CHANNELS);
        printf("Simulate non real-time rendering : use buffer here %ld\n", res);
    } while (res == 512);
    printf("Simulate non real-time rendering : use last buffer here %ld\n", res);
	DeleteSoundPtr(sound);
}

void TestPlay(AudioPlayerPtr player)
{
    float vol = 1.0f;
    float pan = 0.5f;
    char c;
	
    while ((c = getchar()) && (c != 'n')) {

        switch (c) {

            case 'b':
                StartChannel(player, 1);
				break;

            case 'p':
                ContChannel(player, 1);
                break;

            case 's':
                StopChannel(player, 1);
                break;

            case '+':
                vol += 0.05f;
                SetVolAudioPlayer(player, vol);
                break;

            case '-':
                vol -= 0.05f;
                SetVolAudioPlayer(player, vol);
                break;

            case '1':
                pan += 0.05f;
                SetPanAudioPlayer(player, pan);
                break;

            case '2':
                pan -= 0.05f;
                SetPanAudioPlayer(player, pan);
                break;
				
			 case 'c': // To be used only when faust effects are running....
				SetControlValuePtr(faust_effect, 1, 0.95f);
				SetControlValuePtr(faust_effect, 2, 0.9f);
	            break;
        }
    }
}

void SaveSound(AudioStreamPtr sound, char* name)
{
	AudioStreamPtr writesound = MakeRendererSoundPtr(MakeWriteSoundPtr(name, sound,SF_FORMAT_AIFF | SF_FORMAT_PCM_16));
	printf("GetChannelsSoundPtr %ld\n", GetChannelsSoundPtr(writesound));
	float buffer[512 * GetChannelsSoundPtr(writesound)];
    long res;
    do {
        res = ReadSoundPtr(writesound, buffer, 512, GetChannelsSoundPtr(writesound));
        printf("Simulate non real-time rendering : use buffer here %ld\n", res);
    } while (res == 512);
}

void ExecTest(AudioPlayerPtr player, AudioStreamPtr sound)
{
    int res = LoadChannelPtr(player, sound, 1, 1.0f, 0.5f);
	SetStopCallbackChannel(player, 1, TestCallback, NULL);
    if (res == NO_ERR) {
        TestPlay(player);
    } else {
        printf("LoadChannel error %d \n", res);
    }
    StopChannel(player, 1);
    DeleteSoundPtr(sound);
}

int main(int argc, char* argv[])
{
    printf("----------------------------\n");
    printf("LibAudioStream based Player \n");
    printf("----------------------------\n\n");
	
	// Try to open Jack version
    AudioPlayerPtr player = OpenAudioPlayer(IN_CHANNELS, OUT_CHANNELS, CHANNELS, 44100, 512, 65536 * 4, 131072 * 4, kJackRenderer, 1);
    // Is failure opens PortAudio version
    if (!player)
        player = OpenAudioPlayer(IN_CHANNELS, OUT_CHANNELS, CHANNELS, 44100, 512, 65536 * 4, 131072 * 4, kPortAudioRenderer, 1);
    StartAudioPlayer(player);
	
	/*
	AudioStreamPtr sound = MakeTransformSoundPtr(MakeReadSoundPtr ("/Users/letz/son1.wav"),
							AddAudioEffectPtr(MakeAudioEffectListPtr(), MakeVolAudioEffectPtr(0.9)), 1, 0);
							
	SaveSound(sound,"/Users/letz/out1.aiff");
	
	sound = MakeTransformSoundPtr(MakeReadSoundPtr ("/Users/letz/levotmono.wav"),
							AddAudioEffectPtr(MakeAudioEffectListPtr(), MakeVolAudioEffectPtr(0.9)), 1, 0);
							
	SaveSound(sound,"/Users/letz/out2.aiff");
	
	sound = MakeTransformSoundPtr(MakeReadSoundPtr ("/Users/letz/levotmono.wav"),
							AddAudioEffectPtr(MakeAudioEffectListPtr(), MakeFaustAudioEffectPtr(EFFECT1)), 1, 0);
							
	SaveSound(sound,"/Users/letz/out3.aiff");
	*/
			
    printf("Type 'b' to start playing from the begining\n");
    printf("Type 's' to stop playing\n");
    printf("Type 'p' to play from the current position\n");
    printf("Type '+' to raise volume\n");
    printf("Type '-' to lower volume\n");
    printf("Type '1' to pan left\n");
    printf("Type '2' to pan right\n");
    printf("Type 'n' to go to next test\n");
	
    ExecTest(player, test0());
	ExecTest(player, test1());
    ExecTest(player, test2());
    ExecTest(player, test3());
    ExecTest(player, test4());
    ExecTest(player, test5());
    ExecTest(player, test6());
    ExecTest(player, test7());
    ExecTest(player, test8());
    ExecTest(player, test9());
	ExecTest(player, test10());
	ExecTest(player, test11());

	test20();
	test21();
	
    StopAudioPlayer(player);
    CloseAudioPlayer(player);
    printf("Quit\n");
    return 0;
}


