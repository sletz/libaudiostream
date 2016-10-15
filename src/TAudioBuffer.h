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

#ifndef __TAudioBuffer__
#define __TAudioBuffer__

#include <assert.h>
#include <stdio.h>
#include <sys/types.h>
#include <iostream>
#include <algorithm>

#ifdef WIN32
#include <windows.h>
    inline bool CHECK_MLOCK(void* ptr, size_t size)
    {
        if (!VirtualLock((ptr), (size))) {
            SIZE_T minWSS, maxWSS;
            HANDLE hProc = GetCurrentProcess();
            if (GetProcessWorkingSetSize(hProc, &minWSS, &maxWSS)) {
                const size_t increase = size + (10 * 4096);
                maxWSS += increase;
                minWSS += increase;
                if (!SetProcessWorkingSetSize(hProc, minWSS, maxWSS)) {
                    return false;
                } else if (!VirtualLock((ptr), (size))) {
                    return false;
                } else {
                    return true;
                }
            } else {
                return false;
            }
        } else {
            return true;
        }
    }
    #define MUNLOCK(ptr, size) VirtualUnlock((ptr), (size))
#else
#include <sys/mman.h>
    #define CHECK_MLOCK(ptr, size) (mlock((ptr), (size)) == 0)
    #define MUNLOCK(ptr, size) munlock((ptr), (size))
#endif

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

        long fFrames;
        long fChannels;

    public:

        TAudioBuffer():fFrames(0), fChannels(0)
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

        virtual TAudioBuffer<T>* clone() const = 0;

};

//-------------------------------
// Class TInterleavedAudioBuffer
//-------------------------------
/*!
\brief Template based class for interleaved buffers.
*/

template <class T>
class TInterleavedAudioBuffer : public TAudioBuffer<T>
{

    protected:

        T* fBuffer;

        void TouchAndLock(bool touch)
        {
            if (touch) {
                // Force pages loading (4k)
                for (int i = 0; i < this->fFrames * this->fChannels; i+= 4096 / sizeof(T)) {
                    fBuffer[i] = 0.f;
                }
                if (!CHECK_MLOCK(fBuffer, this->fFrames * this->fChannels * sizeof(T))) {
                    printf("Cannot lock TInterleavedAudioBuffer !\n");
                }
            }
        }

    public:

        TInterleavedAudioBuffer():TAudioBuffer<T>(), fBuffer(0)
        {}
        virtual ~TInterleavedAudioBuffer()
        {
            if(fBuffer)
            {
                MUNLOCK(fBuffer, this->fFrames * this->fChannels * sizeof(T));
            }
        }

        T* GetFrame(long frame)
        {
            assert(frame <= this->fFrames);
            return &fBuffer[frame * this->fChannels];
        }

        static void Copy(TInterleavedAudioBuffer* b1, long f1, TInterleavedAudioBuffer* b2, long f2, long frames)
        {
            assert(frames + f1 <= b1->GetSize());
            assert(frames + f2 <= b2->GetSize());
            assert(b1->GetChannels() == b2->GetChannels());
            memcpy(b1->GetFrame(f1), b2->GetFrame(f2), frames * sizeof(T) * b1->GetChannels());
        }

        T* GetBuffer() { return fBuffer; }
};

//----------------------------------
// Class TNonInterleavedAudioBuffer
//----------------------------------
/*!
\brief Template based class for non-interleaved buffers.
*/

template <class T>
class TNonInterleavedAudioBuffer : public TAudioBuffer<T>
{

    protected:

        T** fBuffer;

        void TouchAndLock(bool touch)
        {
            if (touch) {
                // Force pages loading (4k)
                for (int i = 0; i < this->fChannels; i++) {
                    for (int j = 0; j < this->fFrames; j++) {
                        this->fBuffer[i][j] = 0.f;
                    }
                    if (!CHECK_MLOCK(this->fBuffer[i], this->fFrames * sizeof(T))) {
                        printf("Cannot lock TNonInterleavedAudioBuffer !\n");
                    }
                }
            }
        }

    public:

        TNonInterleavedAudioBuffer():TAudioBuffer<T>(), fBuffer(0)
        {}
        virtual ~TNonInterleavedAudioBuffer()
        {
            if(fBuffer)
            {
                for (int i = 0; i < this->fChannels; i++) {
                    MUNLOCK(fBuffer[i], this->fFrames * sizeof(T));
                }
            }
        }

        T** GetFrame(long frame, T** res)
        {
            if (!(frame <= this->fFrames)) {
                printf("GetFrame frame %ld this->fFrames %ld this->fChannels %ld\n", frame, this->fFrames, this->fChannels);
            }
            assert(frame <= this->fFrames);
            for (int i = 0; i < this->fChannels; i++) {
                res[i] = &fBuffer[i][frame];
            }
            return res;
        }

        static void Copy(TNonInterleavedAudioBuffer* b1, long f1, TNonInterleavedAudioBuffer* b2, long f2, long frames)
        {
            assert(frames + f1 <= b1->GetSize());
            assert(frames + f2 <= b2->GetSize());
            assert(b1->GetChannels() == b2->GetChannels());

            T** tmp1 = (T**)alloca(b1->GetChannels()*sizeof(T*));
            T** tmp2 = (T**)alloca(b2->GetChannels()*sizeof(T*));

            T** dst = b1->GetFrame(f1, tmp1);
            T** src = b2->GetFrame(f2, tmp2);

            for (int i = 0; i < b1->GetChannels(); i++) {
                memcpy(dst[i], src[i], frames * sizeof(T));
            }
        }

        T** GetBuffer() { return fBuffer; }

        virtual TNonInterleavedAudioBuffer<T>* clone() const = 0;
};


/*!
\brief Template based class for local interleaved buffers.
*/

template <class T>
class TLocalInterleavedAudioBuffer : public TInterleavedAudioBuffer<T>
{

    public:

        TLocalInterleavedAudioBuffer(long frames, long channels, bool touch = false)
        {
            this->fBuffer = new T[frames * channels];
            this->fFrames = frames;
            this->fChannels = channels;
            this->TouchAndLock(touch);
        }
        virtual ~TLocalInterleavedAudioBuffer()
        {
            delete []this->fBuffer;
            this->fBuffer = nullptr;
        }


        TLocalInterleavedAudioBuffer<T>* clone() const
        {
            auto ptr = new TLocalInterleavedAudioBuffer<T>{this->fFrames, this->fChannels, false};
            std::copy_n(this->fBuffer, this->fFrames * this->fChannels, ptr->fBuffer);
            return ptr;
        }
};

/*!
\brief Template based class for local non-interleaved buffers.
*/

template <class T>
class TLocalNonInterleavedAudioBuffer : public TNonInterleavedAudioBuffer<T>
{

    public:

        TLocalNonInterleavedAudioBuffer(long frames, long channels, bool touch = false)
        {
            this->fBuffer = new T*[channels];
            for (int i = 0; i < channels; i++) {
                this->fBuffer[i] = new T[frames];
            }
            this->fFrames = frames;
            this->fChannels = channels;
            this->TouchAndLock(touch);
        }
        virtual ~TLocalNonInterleavedAudioBuffer()
        {
            for (int i = 0; i < this->fChannels; i++) {
                delete []this->fBuffer[i];
            }
            delete []this->fBuffer;
            this->fBuffer = nullptr;
        }

        TLocalNonInterleavedAudioBuffer<T>* clone() const
        {
            auto ptr = new TLocalNonInterleavedAudioBuffer<T>{this->fFrames, this->fChannels, false};

            for (int i = 0; i < this->fChannels; i++)
                std::copy_n(this->fBuffer[i], this->fFrames, ptr->fBuffer[i]);

            return ptr;
        }
};

/*!
\brief Template based class for shared interleaved buffers.
*/

template <class T>
class TSharedInterleavedAudioBuffer : public TInterleavedAudioBuffer<T>
{
    public:
        TSharedInterleavedAudioBuffer(T* buffer, long frames, long channels)
        {
            this->fBuffer = buffer;
            this->fFrames = frames;
            this->fChannels = channels;
        }

        virtual ~TSharedInterleavedAudioBuffer()
        {}

        TSharedInterleavedAudioBuffer<T>* clone() const
        {
            return new TSharedInterleavedAudioBuffer<T>{this->fBuffer, this->fFrames, this->fChannels};
        }
};

/*!
\brief Template based class for shared non-interleaved buffers.
*/

template <class T>
class TSharedNonInterleavedAudioBuffer : public TNonInterleavedAudioBuffer<T>
{

    public:

        TSharedNonInterleavedAudioBuffer(T** buffer, long frames, long channels)
        {
            this->fBuffer = buffer;
            this->fFrames = frames;
            this->fChannels = channels;
        }
        virtual ~TSharedNonInterleavedAudioBuffer()
        {}

        TSharedNonInterleavedAudioBuffer<T>* clone() const
        {
            return new TSharedNonInterleavedAudioBuffer<T>{this->fBuffer, this->fFrames, this->fChannels};
        }
};

typedef TNonInterleavedAudioBuffer<float>* FLOAT_BUFFER;

#endif
