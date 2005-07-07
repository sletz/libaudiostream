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

#ifndef __TAudioChannel__
#define __TAudioChannel__

#include "TFadeAudioStream.h"
#include "TRendererAudioStream.h"

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
    long fVol;
    long fPan;
    long fLeftOut;
    long fRightOut;
}
ChannelInfo;

typedef void (*StopCallback)(void* context);

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
        TAudioBuffer<float>*	fMixBuffer; 		// Used for mixing
		TStopCallback			fStopCallback;

        long fVol;	// Master vol
        long fPan;	// Master pan
        long fLeftOut;  // Audio left out
        long fRightOut;	// Audio right out
        bool fInserted;	// Insertion state

    public:

        TAudioChannel();
        virtual ~TAudioChannel();

        // Control
        void SoundOn();
        void SoundOff();
        void Reset();

        // Mixing
        bool Mix(TAudioBuffer<float>* out, long framesNum, long channels);

        // Status
        int GetState()
        {
            return fInserted;
        } 	// To know if the channel is inserted in the active channel list
        void SetState(bool state)
        {
            fInserted = state;
        }
        void GetInfo(ChannelInfoPtr info);

        // Warning : SetStream replaces the internal stream with a new one, desallocation has to be done externally
        void SetStream(TAudioStreamPtr stream);
        TAudioStreamPtr GetStream();

        long GetLeft()
        {
            return fLeftOut;
        }
        long GetRight()
        {
            return fRightOut;
        }

        void SetLeft(long left)
        {
            fLeftOut = left;
        }
        void SetRight(long right)
        {
            fRightOut = right;
        }

        long GetVol()
        {
            return fVol;
        }
        long GetPan()
        {
            return fPan;
        }

        void SetVol(long vol)
        {
            fVol = vol;
        }
        void SetPan(long pan)
        {
            fPan = pan;
        }
		
		void SetStopCallback(StopCallback callback, void* context)
        {
            fStopCallback.SetCallback(callback, context);
        }

        StopCallback GetStopCallback()
        {
            return fStopCallback.GetCallback();
        }
};

typedef TAudioChannel * TAudioChannelPtr;

#endif


