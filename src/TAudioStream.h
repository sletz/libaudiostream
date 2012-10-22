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

#ifndef __TAudioStream__
#define __TAudioStream__

#include "TAudioConstants.h"
#include "UAudioTools.h"
#include "TAudioBuffer.h"
#include "la_smartpointer.h"
#include <stdio.h>

//--------------------
// Class TAudioStream
//--------------------

class TAudioStream;

typedef LA_SMARTP<TAudioStream>  TAudioStreamPtr;

/*!
\brief The base class for all streams.
*/ 

class TAudioStream : public la_smartable
{

    public:

        TAudioStream()
        {}
        virtual ~TAudioStream()
        {}

        virtual long Write(FLOAT_BUFFER buffer, long framesNum, long framePos, long channels)
        {
            return 0;
        }
        virtual long Read(FLOAT_BUFFER buffer, long framesNum, long framePos, long channels)
        {
            return 0;
        }

        // Reset the stream to it's beginning
        virtual void Reset()
        {}

        // Cut the beginning of the stream
        virtual TAudioStreamPtr CutBegin(long frames)
        {
            return 0;
        }

        // Length in frames
        virtual long Length()
        {
            return 0;
        }

        // Number of channels
        virtual long Channels()
        {
            return 0;
        }

        // Copy the structure
        virtual TAudioStreamPtr Copy()
        {
            return 0;
        }
};

//  typedef TAudioStream * TAudioStreamPtr;

//-------------------------
// Class TUnaryAudioStream
//-------------------------
/*!
\brief The base class for all unary streams.
*/

class TUnaryAudioStream
{

    protected:

        TAudioStreamPtr fStream; // Decorated stream

    public:
	
		virtual ~TUnaryAudioStream()
        {}
	
        TAudioStreamPtr GetBranch1()
        {
            return fStream;
        }
};

typedef TUnaryAudioStream * TUnaryAudioStreamPtr;


//-----------------------------
// Class TDecoratedAudioStream
//-----------------------------
/*!
\brief  A TDecoratedAudioStream object decorates the contained stream.
*/

class TDecoratedAudioStream : public TAudioStream, public TUnaryAudioStream
{

    public:

        TDecoratedAudioStream(): TAudioStream()
        {
            fStream = 0;
        }
        TDecoratedAudioStream(TAudioStreamPtr stream): TAudioStream()
        {
            fStream = stream;
        }

		virtual long Write(FLOAT_BUFFER buffer, long framesNum, long framePos, long channels)
        {
            assert(fStream);
            return fStream->Write(buffer, framesNum, framePos, channels);
        }

        virtual long Read(FLOAT_BUFFER buffer, long framesNum, long framePos, long channels)
        {
            assert(fStream);
            return fStream->Read(buffer, framesNum, framePos, channels);
        }

        virtual void Reset()
        {
            assert(fStream);
            fStream->Reset();
        }

        virtual TAudioStreamPtr CutBegin(long frames)
        {
            assert(fStream);
            return fStream->CutBegin(frames);
        }

        virtual long Length()
        {
            assert(fStream);
            return fStream->Length();
        }

        virtual long Channels()
        {
            assert(fStream);
            return fStream->Channels();
        }

        virtual TAudioStreamPtr Copy()
        {
            return new TDecoratedAudioStream(fStream->Copy());
        }
};

typedef TDecoratedAudioStream * TDecoratedAudioStreamPtr;

#endif
