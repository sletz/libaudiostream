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

#ifndef __TRendererAudioStream__
#define __TRendererAudioStream__

#include "TCmdHandler.h"
#include "TBinaryAudioStream.h"

//----------------------------
// Class TRendererAudioStream
//----------------------------
/*!
\brief The base class for stream renderers.
*/

class TRendererAudioStream : public TDecoratedAudioStream
{

    protected:

        void SetManager(TAudioStreamPtr stream, TCmdManagerPtr manager);
        virtual TCmdManagerPtr GetManager() = 0;

    public:

        TRendererAudioStream(): TDecoratedAudioStream(0)
        {}
        TRendererAudioStream(TAudioStreamPtr stream, TCmdManagerPtr manager)
                : TDecoratedAudioStream(stream)
        {
            SetManager(stream, manager);
        }
        virtual ~TRendererAudioStream()
        {}

        void SetStream(TAudioStreamPtr stream);
        void ClearStream()
        {
            fStream = 0;
        }
};

typedef TRendererAudioStream * TRendererAudioStreamPtr;

//------------------------------
// Class TDTRendererAudioStream
//------------------------------
/*!
\brief A stream renderer that uses a direct command manager.
*/

class TDTRendererAudioStream : public TRendererAudioStream
{

    private:

        static TCmdManagerPtr fManager;
        TCmdManagerPtr GetManager()
        {
            return fManager;
        }

    public:

        TDTRendererAudioStream(): TRendererAudioStream()
        {}
        TDTRendererAudioStream(TAudioStreamPtr stream): TRendererAudioStream(stream, fManager)
        {}
        virtual ~TDTRendererAudioStream()
        {}

        static void Init();
        static void Destroy();
        static void Flush();
};

typedef TDTRendererAudioStream * TDTRendererAudioStreamPtr;

//------------------------------
// Class TRTRendererAudioStream
//------------------------------
/*!
\brief  A stream renderer that uses a low-priority thread command manager.
*/

class TRTRendererAudioStream : public TRendererAudioStream
{

    private:

        static TCmdManagerPtr fManager;
        TCmdManagerPtr GetManager()
        {
            return fManager;
        }

    public:

        TRTRendererAudioStream(): TRendererAudioStream()
        {}
        TRTRendererAudioStream(TAudioStreamPtr stream): TRendererAudioStream(stream, fManager)
        {}
        virtual ~TRTRendererAudioStream()
        {}

        static void Init(long thread_num);
        static void Destroy();
        static void Flush();
};

//typedef TRTRendererAudioStream * TRTRendererAudioStreamPtr;
typedef LA_SMARTP<TRTRendererAudioStream>  TRTRendererAudioStreamPtr;

//----------------------
// Class TGCAudioStream
//----------------------
/*!
\brief  A stream decorator that uses the Boehm Garbage collector thus providing automatic memory management for streams.
*/

/*
#include <gc_cpp.h>
class TGCAudioStream : public TDecoratedAudioStream, public gc_cleanup{
 
       	public:
        
            TGCAudioStream(TAudioStreamPtr stream):TDecoratedAudioStream(stream){} 
            virtual ~TGCAudioStream(){}
  };
 
typedef TGCAudioStream * TGCAudioStreamPtr;
*/

#endif
