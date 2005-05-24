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

#ifndef __TAudioBuffer__
#define __TAudioBuffer__

#include <assert.h>

//--------------------
// Class TAudioBuffer
//--------------------
/*!
\brief Template based class for buffers.
*/

template <class T>
class TAudioBuffer
{

    protected:

        T* fBuffer;
        long fFrames;
        long fChannels;
        long fPos;

    public:

        TAudioBuffer(): fBuffer(0), fFrames(0), fChannels(0), fPos(0)
        {}
        virtual ~TAudioBuffer()
        {}

        long GetSize()
        {
            return fFrames;
        }
        long GetChannels()
        {
            return fChannels;
        }

        T* GetFrame(long frame)
        {
            assert(frame <= fFrames);
            return &fBuffer[frame * fChannels];
        }

        T* GetPos()
        {
            assert(fPos <= fFrames);
            return &fBuffer[fPos * fChannels];
        }

        static void Copy(TAudioBuffer* b1, long f1, TAudioBuffer* b2, long f2, long frames)
        {
            assert(frames + f1 <= b1->GetSize());
            assert(frames + f2 <= b2->GetSize());
            assert(b1->GetChannels() == b2->GetChannels());
            memcpy(b1->GetFrame(f1), b2->GetFrame(f2), frames*sizeof(T)*b1->GetChannels());
        }
};

/*!
\brief Template based class for local buffers.
*/

template <class T>
class TLocalAudioBuffer : public TAudioBuffer<T>
{

    public:

        TLocalAudioBuffer(long frames, long channels)
        {
            this->fBuffer = new T[frames * channels];
            this->fFrames = frames;
            this->fChannels = channels;
            this->fPos = 0;
        }
        virtual ~TLocalAudioBuffer()
        {
            delete []this->fBuffer;
        }
};

/*!
\brief Template based class for shared buffers.
*/

template <class T>
class TSharedAudioBuffer : public TAudioBuffer<T>
{

    public:

        TSharedAudioBuffer(T* buffer, long frames, long channels)
        {
            this->fBuffer = buffer;
            this->fFrames = frames;
            this->fChannels = channels;
            this->fPos = 0;
        }
        virtual ~TSharedAudioBuffer()
        {}
};

#endif
