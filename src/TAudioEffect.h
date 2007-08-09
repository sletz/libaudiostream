/*
Copyright © Grame 2002-2007

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

#ifndef __TAudioEffect__
#define __TAudioEffect__

#include "TAudioEffectInterface.h"
#include "TAudioGlobals.h"
#include "TAudioConstants.h"
#include "Envelope.h"
#include <list>

using namespace std;

//--------------------
// Class TAudioEffect
//--------------------

class TAudioEffectList;

typedef SMARTP<TAudioEffectList> TAudioEffectListPtr;

// Using smartable1 cause crash when desallocating the object: desactivated for now
// class TAudioEffectList : public list<TAudioEffectInterfacePtr>,  public la_smartable1

/*!
\brief  Effect list management for subclasses of TAudioEffectInterface.
*/

class TAudioEffectList : public list<TAudioEffectInterfacePtr>,  public la_smartable
{

	private:
	
		float* fTemp1[MAX_PLUG_CHANNELS];
		float* fTemp2[MAX_PLUG_CHANNELS];
		long fStatus;			// Playing state
		Envelope fFadeIn;    	// FadeIn object
        Envelope fFadeOut;   	// FadeOut object
		long fFadeInFrames;		// Number of frames for FadeIn
        long fFadeOutFrames;	// Number of frames for FadeOut
		
		void Init(float v1, float f1, float v2, float f2);
		
    public:
		enum {kIdle = 0, kFadeIn, kPlaying, kFadeOut};

        TAudioEffectList():fFadeInFrames(0),fFadeOutFrames(0)
        {
			int i;
			for (i = 0; i < MAX_PLUG_CHANNELS; i++) {
				fTemp1[i] = (float*)calloc(TAudioGlobals::fBuffer_Size, sizeof(float));
			}
			for (i = 0; i < MAX_PLUG_CHANNELS; i++) {
				fTemp2[i] = (float*)calloc(TAudioGlobals::fBuffer_Size, sizeof(float));
			}
			fStatus = kIdle;
		}
        virtual ~TAudioEffectList();

        virtual void Process(float* buffer, long framesNum, long channels);
        TAudioEffectListPtr Copy();
        void Reset();
		
		void FadeIn(long fadeIn, long fadeOut);
		void FadeOut();
		long GetStatus() 
		{
			return fStatus;
		}
};

//-------------------------------
// Class TAudioEffectListManager
//-------------------------------

/*!
\brief  Effect list management: handle crossfade between effects.
*/

class TAudioEffectListManager {

	  private: 
	  
		float*	fTempBuffer; 					// Used for crossfade
		TAudioEffectListPtr	fCurEffectList;		// Current Effect list
		TAudioEffectListPtr	fNextEffectList;	// Next Effect list
		bool fSwitchEffect;
	
	public:
	
		TAudioEffectListManager():fCurEffectList(new TAudioEffectList()),fNextEffectList(0),fSwitchEffect(false)
		{
			fTempBuffer = new float[TAudioGlobals::fBuffer_Size * TAudioGlobals::fOutput]; // A revoir
		}
		
		virtual ~TAudioEffectListManager()
		{
			delete[] fTempBuffer;
		}

		// TO IMPROVE : this will fail if an effect is still in switch mode...
		void SetEffectList(TAudioEffectListPtr effect_list, long fadeIn, long fadeOut)
		{
			if (!fSwitchEffect) { 
				fNextEffectList = effect_list;
				fCurEffectList->FadeOut();
				fNextEffectList->FadeIn(fadeIn, fadeOut);
	 			fSwitchEffect = true;
			} else if (fadeIn == -1 && fadeOut == -1) {  // Used to indicate immediate switch
				effect_list->FadeIn(100, 100);
				fCurEffectList = effect_list;
			}
		}
	
		TAudioEffectListPtr GetEffectList() // Called in RT
		{
			return fCurEffectList;
		}
		
		void Process(float* buffer, long framesNum, long channels);
};

#endif
