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

#ifndef __TEventAudioStream__
#define __TEventAudioStream__

#include "TSeqAudioStream.h"
#include <list>

//---------------------
// Class TEventHandler
//---------------------
/*!
\brief  A TEventHandler handle external events.
*/

class TEventHandler
{

    protected:

        long fNum;      // event number
        bool fFired;    // Fired state

    public:

        TEventHandler(long num): fNum(num), fFired(false)
        {}
        virtual ~TEventHandler()
        {}

        virtual bool HandleEvent(long num) = 0;
};

typedef TEventHandler * TEventHandlerPtr;

//----------------------------
// Class TEventSeqAudioStream
//----------------------------
/*!
\brief A TEventSeqAudioStream plays two streams in sequence, and switch to the second stream if an event is received.
*/

/*
class TEventSeqAudioStream : public TSeqAudioStream, public TEventHandler
{
    
    public:

        TEventSeqAudioStream(TAudioStreamPtr s1, TAudioStreamPtr s2, long crossFade, long num): TSeqAudioStream(s1, s2, crossFade), TEventHandler(num)
        {}
        virtual ~TEventSeqAudioStream()
        {}

        TAudioStreamPtr Copy()
        {
            return new TEventSeqAudioStream(fStream1->Copy(), fStream2->Copy(), fCrossFade, fNum);
        }

        bool HandleEvent(long num)
        {
            fFired = (fNum == num);
            return fFired;
        }
        
        long Read(FLOAT_BUFFER buffer, long framesNum, long framePos);
        void Reset();
};

typedef TEventSeqAudioStream * TEventSeqAudioStreamPtr;
*/

//-------------------------
// Class TEventHandlerList
//-------------------------
/*!
\brief A STL list which contains the TEventHandler nodes of a stream expression.
*/

class TEventHandlerList : public std::list<TEventHandlerPtr>
{

    private:

        // Build a list of TCmdHandler leaves
        void MakeList(TAudioStreamPtr stream)
        {
            if (TBinaryAudioStreamPtr binary = dynamic_cast<TBinaryAudioStreamPtr>(stream.getPointer())) {
                MakeList(binary->GetBranch1());
                MakeList(binary->GetBranch2());
            } else if (TUnaryAudioStreamPtr unary = dynamic_cast<TUnaryAudioStreamPtr>(stream.getPointer())) {
                MakeList(unary->GetBranch1());
            } else if (TEventHandlerPtr handler = dynamic_cast<TEventHandlerPtr>(stream.getPointer())) {
                push_front(handler);
            }
        }

    public:

        TEventHandlerList(TAudioStreamPtr stream)
        {
            MakeList(stream);
        }
        virtual ~TEventHandlerList()
        {}
};

typedef TEventHandlerList * TEventHandlerListPtr;

//-------------------------
// Class TEventAudioStream
//-------------------------
/*!
\brief A TEventAudioStream gives the received event to all TEventHandler objects in the decorated stream.
*/

class TEventAudioStream : public TDecoratedAudioStream, public TEventHandler
{

    private:

        TEventHandlerList fHandlerList;

    public:

        TEventAudioStream(TAudioStreamPtr stream)
                : TDecoratedAudioStream(stream), TEventHandler(0), fHandlerList(stream)
        {}
        virtual ~TEventAudioStream()
        {}

        TAudioStreamPtr Copy()
        {
            return new TEventAudioStream(fStream);
        }

        bool HandleEvent(long num)
        {
            bool res = false;
            // Iterate through list
            for (std::list<TEventHandlerPtr>::iterator iter = fHandlerList.begin(); iter != fHandlerList.end(); iter++) {
                TEventHandlerPtr handler = *iter;
                res |= handler->HandleEvent(num);
            }
            return res;
        }

};

typedef TEventAudioStream * TEventAudioStreamPtr;

#endif
