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

#define IN_CHANNELS 2 // stereo player
#define OUT_CHANNELS 2 // stereo player
#define CHANNELS 8

AudioStreamPtr test0()
{
    printf("-------------- \n");
    printf("Build a region \n");
    printf("-------------- \n\n");
    AudioStreamPtr s1;
    s1 = MakeRegionSound(FILENAME1, 200000, 500000);
    return s1;
}

AudioStreamPtr test1()
{
    printf("--------------------------- \n");
    printf("Build a region with a fade \n");
    printf("--------------------------- \n\n");
    AudioStreamPtr s1;
    s1 = MakeRegionSound(FILENAME1, 200000, 500000);
    return MakeFadeSound(s1, 44100, 44100);
}

AudioStreamPtr test2()
{
    printf("------------------------------------- \n");
    printf("Build a mix of 2 regions with a fade \n");
    printf("------------------------------------- \n\n");
    AudioStreamPtr s1, s2;
    s1 = MakeRegionSound(FILENAME1, 400000, 1700000);
    s2 = MakeRegionSound(FILENAME2, 400000, 1800000);
    return MakeFadeSound(MakeMixSound(s2, s1), 44100, 44100);
}

AudioStreamPtr test3()
{
    printf("-----------------------------------------------\n");
    printf("Build a sequence of 2 regions with a crossfade \n");
    printf("-----------------------------------------------\n\n");
    AudioStreamPtr s1, s2;
    s1 = MakeRegionSound(FILENAME1, 200000, 500000);
    s2 = MakeRegionSound(FILENAME2, 200000, 700000);
    return MakeSeqSound(s1, s2, 88200);
}

AudioStreamPtr test4()
{
    printf("----------------------\n");
    printf("Build a looped region \n");
    printf("----------------------\n\n");
    AudioStreamPtr s1;
    s1 = MakeRegionSound(FILENAME1, 200000, 400000);
    s1 = MakeFadeSound(s1, 500, 500);
    s1 = MakeLoopSound(s1, 5);
    return s1;
}

AudioStreamPtr test5()
{
    printf("------------------------------------\n");
    printf("Build a input/output through stream \n");
    printf("------------------------------------\n\n");
    return MakeInputSound();
}

AudioStreamPtr test6()
{
    printf("---------------------------------------------------------\n");
    printf("Build a input/output through and record the ouput stream\n");
    printf("---------------------------------------------------------\n\n");
    return MakeWriteSound("input.aif", MakeInputSound(), SF_FORMAT_AIFF | SF_FORMAT_PCM_16);
}

AudioStreamPtr test7()
{
    printf("-------------------------\n");
    printf("Build a echo on a region \n");
    printf("-------------------------\n\n");
    AudioStreamPtr sound, mix = MakeRegionSound(FILENAME2, 400000, 1000000);
    for (int i = 0; i < 32 ; i++) {
        sound = MakeSeqSound(MakeNullSound(i * 4410), MakeRegionSound(FILENAME2, 400000, 1000000), 0);
        mix = MakeMixSound(sound, mix);
    }
    return mix;
}

AudioStreamPtr test8()
{
    printf("------------------------------------------------------------------\n");
    printf("Cut a region into a complex stream and write the result to a file\n");
    printf("------------------------------------------------------------------\n\n");
    AudioStreamPtr sound, mix = MakeRegionSound(FILENAME2, 400000, 500000);
    for (int i = 0; i < 8 ; i++) {
        sound = MakeSeqSound(MakeNullSound(i * 44000), MakeRegionSound(FILENAME2, 400000, 500000), 0);
        mix = MakeMixSound(sound, mix);
    }
    return MakeWriteSound("output.aif", MakeCutSound(mix, 100000, 200000), SF_FORMAT_AIFF | SF_FORMAT_PCM_16);
}

AudioStreamPtr test9()
{
    printf("--------------------------------------------------------------\n");
    printf("Sequence of a region with a transformed region (volume effect)\n");
    printf("--------------------------------------------------------------\n\n");
    AudioStreamPtr sound1 = MakeRegionSound(FILENAME1, 400000, 600000);
    AudioStreamPtr sound2 = MakeRegionSound(FILENAME1, 400000, 600000);
    AudioEffectListPtr list_effect = MakeAudioEffectList();
    list_effect = AddAudioEffect(list_effect, MakeVolAudioEffect(0.25));
    return MakeSeqSound(sound1, MakeTransformSound(sound2, list_effect, 100, 100), 44100);
}

AudioStreamPtr test10()
{
    printf("-------------------------------------------------------------------\n");
    printf("Sequence of a region with a Faust freeverb region                  \n");
    printf("-------------------------------------------------------------------\n\n");
    AudioStreamPtr sound1 = MakeRegionSound(FILENAME1, 400000, 1000000);
    AudioStreamPtr sound2 = MakeRegionSound(FILENAME1, 400000, 1000000);
    AudioEffectListPtr list_effect = MakeAudioEffectList();
    list_effect = AddAudioEffect(list_effect, MakeFaustAudioEffect("freeverb.so"));
    return MakeSeqSound(sound1, MakeTransformSound(sound2, list_effect, 100, 100), 44100);
}

AudioStreamPtr test11()
{
    printf("-------------------------------------------------------------------\n");
    printf("Input stream + Faust freeverb effect                               \n");
    printf("-------------------------------------------------------------------\n\n");
    AudioEffectListPtr list_effect = MakeAudioEffectList();
    list_effect = AddAudioEffect(list_effect, MakeFaustAudioEffect("freeverb.so"));
    return MakeTransformSound(MakeInputSound(), list_effect, 100, 100);
}

void test12()
{
    printf("-----------------------------------------------------------\n");
    printf("Non real-time rendering : use the MakeRendererSound wrapper\n");
    printf("-----------------------------------------------------------\n\n");
    AudioStreamPtr sound = MakeRendererSound(MakeRegionSound(FILENAME2, 400000, 500000));
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
    int vol = 127;
    int pan = 64;
    char c;

    while ((c = getchar()) && (c != 'n')) {

        switch (c) {

            case 'b':
                StartSound(player, 1);
                break;

            case 'p':
                ContSound(player, 1);
                break;

            case 's':
                StopSound(player, 1);
                break;

            case '+':
                vol += 5;
                SetVolAudioPlayer(player, vol);
                break;

            case '-':
                vol -= 5;
                SetVolAudioPlayer(player, vol);
                break;

            case '1':
                pan += 5;
                SetPanAudioPlayer(player, pan);
                break;

            case '2':
                pan -= 5;
                SetPanAudioPlayer(player, pan);
                break;
        }
    }
}

void ExecTest(AudioPlayerPtr player, AudioStreamPtr sound)
{
    int res = LoadChannel(player, sound, 1, 120, 64);
    if (res == NO_ERR) {
        TestPlay(player);
    } else {
        printf("LoadChannel error %ld \n", res);
    }
    StopSound(player, 1);
    DeleteSound(sound);
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
    test12();

    StopAudioPlayer(player);
    CloseAudioPlayer(player);
    printf("Quit\n");
    return 0;
}


