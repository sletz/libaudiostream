/*
  Copyright (C) 2005  Grame
  Grame Research Dpt, 9 rue du Garet, 69001 Lyon - France
  research@grame.fr

  This file is provided as an example of the AudioStream Library use.
*/

#include <iostream>
#include "LibAudioStream++.h"
using namespace std;

class FilePlayer {
	private:
		enum { kIn=2, kOut=2, kChans=8, kSRate=44100, kAudioBuff=512, kFileBuff=65536 * 4 };
		AudioPlayerPtr fPlayer;

	public:
		FilePlayer()	{ 
			fPlayer = OpenAudioPlayer(kIn, kOut, kChans, kSRate, kAudioBuff, kFileBuff, kFileBuff, kPortAudioRenderer, 1);
			StartAudioPlayer(fPlayer);
		}

		virtual ~FilePlayer()	{
			StopChannel (fPlayer, 1);
    		StopAudioPlayer (fPlayer);
    		CloseAudioPlayer(fPlayer);
		}
		
		void Play (char *file, long beginFrame, long endFrame) {
			AudioStream stream = MakeRegionSound (file, beginFrame, endFrame);
			LoadChannel (fPlayer, stream, 1, 1.0f, 0.5f);
			StartChannel(fPlayer, 1);
		} 
		
		long Status () {
			ChannelInfo info;
			GetInfoChannel (fPlayer, 1, &info);
			return info.fStatus;
		} 

};

int main (int argc, char *argv[]) {
	if (argc != 4) {
		cerr << "usage: fileplay 'file' 'start' 'end'\n" << endl;
		return 1;
	}
	else {
		FilePlayer player;
		int start = atoi(argv[2]);
		int end   = atoi(argv[3]);
		cout << "playing file " << argv[1] << " from " << start << " to " << end << endl;	
		player.Play(argv[1], start * 44100, end * 44100);
		while (player.Status() != kIdleChannel)
			;
		cout << "done" << endl;	
	}
	return 0;
}
