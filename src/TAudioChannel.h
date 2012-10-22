/*

Copyright (C) Grame 2002-2012

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

#ifndef __TAudioChannel__
#define __TAudioChannel__

#include "TFadeAudioStream.h"
#include "TRendererAudioStream.h"
#include "TAudioEffect.h"
#include "TPanTable.h"

#ifdef WIN32
	#if defined(_AFXDLL) || defined(_WINDLL)	// using mfc
	#include <afx.h>
	#else
	#include <windows.h>// without mfc
	#endif
	#define AudioSleep(val) Sleep(val)
#else
 #define AudioSleep(val) usleep(val*1000)
#endif

typedef struct ChannelInfo * ChannelInfoPtr;
typedef struct ChannelInfo
{
    long fStatus;
    long fCurFrame;
    float fVol;
    float fPanLeft;
	float fPanRight;
    long fLeftOut;
    long fRightOut;
}
ChannelInfo;

typedef void (*StopCallback)(void* context);

//-----------------
// Class TCallback
//-----------------

/*!
\brief A generic callback.
*/

class TCallback
{

    protected:

        void* fContext;
        bool fStatus;

    public:

        TCallback(): fContext(0), fStatus(false)
        {}
        virtual ~TCallback()
        {}

        void Activate()
        {
			fStatus = true;
        }
        void Desactivate()
        {
			fStatus = false;
        }
};

//---------------------
// Class TStopCallback
//---------------------

/*!
\brief A callback to be called when the stream stops.
*/

class TStopCallback : public TCallback
{

    private:

        StopCallback fCallback;

    public:

        TStopCallback(): TCallback(), fCallback(0)
        {}
        virtual ~TStopCallback()
        {}

        void Execute()
        {
			if (fStatus && fCallback) {
                fStatus = false;
                fCallback(fContext);
			}
		}

        void SetCallback(StopCallback fun, void* context)
        {
            fCallback = fun;
            fContext = context;
        }
        StopCallback GetCallback()
        {
            return fCallback;
        }
};

//---------------------
// Class TAudioChannel
//---------------------

/*!
\brief Contains a stream object. Allows to Start and Stop a stream. Does Volume and Panning control.
*/

class TAudioChannel
{

    private:

        TChannelFadeAudioStream fFadeStream; 		// Fade stream
        TRTRendererAudioStream	fRendererStream;	// Renderer stream (set a real-time command manager for file stream)
        TAudioStreamPtr			fStream;			// Audio stream
        FLOAT_BUFFER	fMixBuffer; 		// Used for mixing
		TStopCallback			fStopCallback;		// Stop callback called when the stream is finished
		TAudioEffectListManager	fEffectList;

        float fVol;			// Volume
        float fPanLeft;		// Pan for left signal
		float fPanRight;	// Pan for right signal
		float fLLVol;		// Volume for left output for left channel
		float fLRVol;		// Volume for right output for left channel
		float fRLVol;		// Volume for left output for right channel
		float fRRVol;		// Volume for right output for right channel
        long fLeftOut;		// Audio left out
        long fRightOut;		// Audio right out
        bool fInserted;		// Insertion state

    public:

        TAudioChannel();
        virtual ~TAudioChannel();

        // Control
        void SoundOn();
        void SoundOff(bool sync);
	    void Reset();

        // Mixing
        bool Mix(FLOAT_BUFFER out, long framesNum, long channels);
		
		// To know if the channel is inserted in the active channel list
        void SetState(bool state)
        {
            fInserted = state;
        }
        // Status
        int GetState()
        {
            return fInserted;
        } 	
		
        void GetInfo(ChannelInfoPtr info);

        void SetStream(TAudioStreamPtr stream);
        TAudioStreamPtr GetStream();

		void SetLeft(long left)
        {
            fLeftOut = left;
        }
        long GetLeft()
        {
            return fLeftOut;
        }
		
        void SetRight(long right)
        {
            fRightOut = right;
        }
		long GetRight()
        {
            return fRightOut;
        }

		void SetVol(float vol)
        {
            fVol = vol;
			TPanTable::GetLR(fVol, fPanLeft, &fLLVol, &fLRVol);
			TPanTable::GetLR(fVol, fPanRight, &fRLVol, &fRRVol);
        }
        float GetVol()
        {
            return fVol;
        }
        float GetPanLeft()
        {
            return fPanLeft;
        }
		float GetPanRight()
        {
            return fPanRight;
        }
		void SetPan(float panLeft, float panRight)
        {
		    fPanLeft = panLeft;
			fPanRight = panRight;
			TPanTable::GetLR(fVol, fPanLeft, &fLLVol, &fLRVol);
			TPanTable::GetLR(fVol, fPanRight, &fRLVol, &fRRVol);
		}
		
		void SetStopCallback(StopCallback callback, void* context)
        {
            fStopCallback.SetCallback(callback, context);
        }

        StopCallback GetStopCallback()
        {
            return fStopCallback.GetCallback();
        }
		
		void SetEffectList(TAudioEffectListPtr effect_list, long fadeIn, long fadeOut)
        {
			fEffectList.SetEffectList(effect_list, fadeIn, fadeOut);
        }

        TAudioEffectListPtr GetEffectList()
        {
            return fEffectList.GetEffectList();
        }
};

typedef TAudioChannel * TAudioChannelPtr;

#endif


