/*

Copyright © Grame 2002

This library is free software; you can redistribute it and modify it under 
the terms of the GNU Library General Public License as published by the 
Free Software Foundation version 2 of the License, or any later version.framesNum * channelsOut

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

#ifndef __UAudioTools__
#define __UAudioTools__

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <assert.h>
#include <limits.h>

#include "TAudioConstants.h"

//--------------------
// Class UAudioTools
//--------------------
/*!
\brief An utility calls for common mixing functions.
*/

class UAudioTools
{

    private:

        static const float fGain;

    public:

        static inline void MonoToStereo(short* dst, short* src, long nbsamples)
        {
            do {
                int x = *src++;
                *dst += x;
                dst++;
                *dst += x;
                dst++;
            } while (--nbsamples);
        }

        static inline void MixMonoToStereoBlk(long* dst, short* src, long nbsamples, short leftamp, short rightamp)
        {
            long x, y, i, j;

            for ( i = 0, j = 0 ; i < nbsamples; i += 2, j += 4) {
                x = src[i];
                y = src[i + 1];
                dst[j] += (x * leftamp);
                dst[j + 1] += (x * rightamp);
                dst[j + 2] += (y * leftamp);
                dst[j + 3] += (y * rightamp);
            }
        }

        static inline void MixMonoToStereoBlk(MY_FLOAT* dst, MY_FLOAT* src, long nbsamples, MY_FLOAT leftamp, MY_FLOAT rightamp)
        {
            MY_FLOAT x, y;
            long i, j;

            for ( i = 0, j = 0 ; i < nbsamples; i += 2, j += 4) {
                x = src[i];
                y = src[i + 1];
                dst[j] += (x * leftamp);
                dst[j + 1] += (x * rightamp);
                dst[j + 2] += (y * leftamp);
                dst[j + 3] += (y * rightamp);
            }
        }


        static inline void MixStereoToStereoBlk(long* dst, short* src, long nbsamples, short leftamp, short rightamp)
        {
            long x, y ;

            for (long i = 0 ; i < nbsamples; i += 2) {
                x = src[i];
                y = src[i + 1];
                dst [i] += (x * leftamp);
                dst [i + 1] += (y * rightamp);
            }
        }

        static inline void MixStereoToStereoBlk(MY_FLOAT* dst, MY_FLOAT* src, long nbsamples, MY_FLOAT leftamp, MY_FLOAT rightamp)
        {
            MY_FLOAT x, y ;

            for (long i = 0 ; i < nbsamples; i += 2) {
                x = src[i];
                y = src[i + 1];
                dst [i] += (x * leftamp);
                dst [i + 1] += (y * rightamp);
            }
        }

        static inline void MixFrameToFrameBlk(MY_FLOAT* dst, MY_FLOAT* src, long framesNum, long channels, MY_FLOAT leftamp, MY_FLOAT rightamp)
        {
            for (int i = 0 ; i < framesNum; i++) {
                for (int j = 0 ; j < channels; j += 2) { // A REVOIR
                    int index1 = i * channels + j;
                    int index2 = index1 + 1;
                    dst[index1] += (src[index1] * leftamp);
                    dst[index2] += (src[index2] * rightamp);
                }
            }
        }

        static inline void MixFrameToFrameBlk1(MY_FLOAT* dst, MY_FLOAT* src, long framesNum, long channels)
        {
            for (int i = 0 ; i < framesNum; i++) {
                for (int j = 0 ; j < channels; j += 2) { // A REVOIR
                    int index1 = i * channels + j;
                    int index2 = index1 + 1;
                    dst[index1] += src[index1];
                    dst[index2] += src[index2];
                }
            }
        }


        static inline void ReplaceFrameToFrameBlk(MY_FLOAT* dst, MY_FLOAT* src, long framesNum, long channels, MY_FLOAT leftamp, MY_FLOAT rightamp)
        {
            for (int i = 0 ; i < framesNum; i++) {
                for (int j = 0 ; j < channels; j += 2) { // A REVOIR
                    int index1 = i * channels + j;
                    int index2 = index1 + 1;
                    dst[index1] = (src[index1] * leftamp);
                    dst[index2] = (src[index2] * rightamp);
                }
            }
        }

        static inline void ReplaceFrameToFrameBlk1(MY_FLOAT* dst, MY_FLOAT* src, long framesNum, long channels)
        {
            for (int i = 0 ; i < framesNum; i++) {
                for (int j = 0 ; j < channels; j += 2) { // A REVOIR
                    int index1 = i * channels + j;
                    int index2 = index1 + 1;
                    dst[index1] = src[index1];
                    dst[index2] = src[index2];
                }
            }
        }

        static inline void ZeroStereoBlk(long* dst, long nbsamples)
        {
            memset(dst, 0, sizeof(long)*nbsamples);
        }

        static inline void ZeroStereoBlk(short* dst, long nbsamples)
        {
            memset(dst, 0, sizeof(short)*nbsamples);
        }

        static inline void ZeroStereoBlk(MY_FLOAT* dst, long nbsamples)
        {
            memset(dst, 0, sizeof(MY_FLOAT)*nbsamples);
        }

        static inline void ZeroFloatBlk(MY_FLOAT* dst, long framesNum, long channels)
        {
            memset(dst, 0, sizeof(MY_FLOAT)*framesNum*channels);
        }

        static inline void ZeroShortBlk(short* dst, long framesNum, long channels)
        {
            memset(dst, 0, sizeof(short)*framesNum*channels);
        }

        static inline void Float2ShortStereo(MY_FLOAT* in, short* out, long nbsamples)
        {
            for (long i = 0;i < nbsamples; i++) {
                out[i] = (in[i] >= 1.0) ? SHRT_MAX : (in[i] <= -1.0) ? SHRT_MIN : short(in[i] * float(SHRT_MAX));
            }
        }

        static inline void Float2LongMulti(MY_FLOAT* in, long * out, long framesNum, long nbchan, long chan1, long chan2)
        {
            for (long i = 0;i < framesNum; i++) {
                out[(i*nbchan) + chan1]
                = (in[i * 2] >= 1.0) ? LONG_MAX : (in[i * 2] <= -1.0) ? LONG_MIN : long(in[i * 2] * float(LONG_MAX));
                out[(i*nbchan) + chan2]
                = (in[i * 2 + 1] >= 1.0) ? LONG_MAX : (in[i * 2 + 1] <= -1.0) ? LONG_MIN : long(in[i * 2 + 1] * float(LONG_MAX));
            }
        }

        static inline void Short2FloatStereo(short* in, MY_FLOAT* out, long nbsamples)
        {
            float fGain = 1.0f / float(SHRT_MAX);
            for (long i = 0; i < nbsamples; i++) {
                out [i] = float(in[i]) * fGain;
            }
        }

        static inline void Short2FloatMono(short* in, MY_FLOAT* out, long nbsamples)
        {
            float sample;
            float fGain = 1.0f / float(SHRT_MAX);

            for (long i = 0; i < nbsamples; i++) {
                sample = float(in[i]) * fGain;
                out [i*2] = sample;
                out [i*2 + 1] = sample;
            }
        }

        static inline float ClipFloat (float sample)
        {
            return (sample < -1.0f) ? -1.0f : (sample > 1.0f) ? 1.0f : sample;
        }

        static inline void Short2Float(short* in, MY_FLOAT* out, long framesNum, long channelsIn, long channelsOut)
        {
            float sample;
            float fGain = 1.0f / float(SHRT_MAX);

            if (channelsIn < channelsOut) { // distribute channels
                for (long i = 0; i < framesNum; i++) {
                    long indexRead = i * channelsIn;
                    long indexWrite = i * channelsOut;
                    for (long j = 0; j < channelsOut; j++) {
                        sample = float(in[indexRead + j % channelsIn]) * fGain;
                        out[indexWrite + j] = sample;
                    }
                }
            } else {
                for (long i = 0; i < framesNum * channelsOut; i += 4) {
                    out[i] = float(in[i]) * fGain;
                    out[i + 1] = float(in[i + 1]) * fGain;
                    out[i + 2] = float(in[i + 2]) * fGain;
                    out[i + 3] = float(in[i + 3]) * fGain;
                }
            }
        }

        static inline void Short2FloatMix(short* in, MY_FLOAT* out, long framesNum, long channelsIn, long channelsOut)
        {
            float sample;
            float fGain = 1.0f / float(SHRT_MAX);

            if (channelsIn < channelsOut) { // distribute channels
                for (long i = 0; i < framesNum; i++) {
                    long indexRead = i * channelsIn;
                    long indexWrite = i * channelsOut;
                    for (long j = 0; j < channelsOut; j++) {
                        sample = float(in[indexRead + j % channelsIn]) * fGain;
                        out[indexWrite + j] += sample;
                    }
                }
            } else {
                for (long i = 0; i < framesNum * channelsOut; i += 4) {
                    out[i] += float(in[i]) * fGain;
                    out[i + 1] += float(in[i + 1]) * fGain;
                    out[i + 2] += float(in[i + 2]) * fGain;
                    out[i + 3] += float(in[i + 3]) * fGain;
                }
            }
        }

        static inline void Float2Short(MY_FLOAT* in, short* out, long framesNum, long channelsIn, long channelsOut)
        {
            float fGain = float(SHRT_MAX);

            if (channelsIn < channelsOut) { // distribute channels
                for (long i = 0; i < framesNum; i++) {
                    for (long j = 0; j < channelsOut; j++) {
                        out[i*channelsOut + j] = (short)(ClipFloat(in[i * channelsIn + j % channelsIn]) * fGain);
                    }
                }
            } else {
                for (long i = 0; i < framesNum; i++) {
                    for (long j = 0; j < channelsOut; j++) {
                        out[i*channelsOut + j] = (short)(ClipFloat(in[i * channelsIn + j]) * fGain);
                    }
                }
            }
        }

        static inline void Float2ShortMix(MY_FLOAT* in, short* out, long framesNum, long channelsIn, long channelsOut)
        {
            float fGain = float(SHRT_MAX);

            if (channelsIn < channelsOut) {  // distribute channels
                for (long i = 0; i < framesNum; i++) {
                    for (long j = 0; j < channelsOut; j++) {
                        out[i*channelsOut + j] += (short)(ClipFloat(in[i * channelsIn + j % channelsIn]) * fGain);
                    }
                }
            } else {
                for (long i = 0; i < framesNum * channelsOut; i += 4) {
                    out[i] += (short)(ClipFloat(in[i]) * fGain);
                    out[i + 1] += (short)(ClipFloat(in[i + 1]) * fGain);
                    out[i + 2] += (short)(ClipFloat(in[i + 2]) * fGain);
                    out[i + 3] += (short)(ClipFloat(in[i + 3]) * fGain);
                }
            }
        }

        static inline void Float2FloatMix(MY_FLOAT* in, MY_FLOAT* out, long framesNum, long channelsIn, long channelsOut)
        {
            if (channelsIn < channelsOut) {  // distribute channels
                for (long i = 0; i < framesNum; i++) {
                    for (long j = 0; j < channelsOut; j++) {
                        out [i*channelsOut + j] += in[i * channelsIn + j % channelsIn];
                    }
                }
            } else {
                for (long i = 0; i < framesNum * channelsOut; i += 4) {
                    out[i] = (short)(ClipFloat(in[i]) * fGain);
                    out[i + 1] = (short)(ClipFloat(in[i + 1]) * fGain);
                    out[i + 2] = (short)(ClipFloat(in[i + 2]) * fGain);
                    out[i + 3] = (short)(ClipFloat(in[i + 3]) * fGain);
                }
            }
        }

        static inline void MultFrame(float* frame, float val, long channels)
        {
            // A optimiser
            for (int i = 0 ; i < channels; i++) {
                frame[i] *= val;
            }
        }

        static void cTocCopy(char *dest, const char* src)
        {
            register short i = 0;

            while (src[i] != 0) {
                dest[i] = src[i];
                i++;
            }
            dest[i] = 0;
        }

        static inline int FourCC(const char *ChunkName)
        {
            int retbuf = 0x20202020;   // four spaces (padding)
            char *p = ((char *) & retbuf);

            // Remember, this is Intel format!
            // The first character goes in the LSB

            for (long i = 0; i < 4 && ChunkName[i]; i++) {
                *p++ = ChunkName[i];
            }

            return retbuf;
        }

        static inline float ConvertMsToFrame(float ms)
        {
            return ((ms * SAMPLE_RATE) / 1000.0f);
        }
        static inline float ConvertFrameToMs(float frame)
        {
            return ((frame / SAMPLE_RATE) * 1000.0f);
        }

        static inline float ConvertSecToFrame(float ms)
        {
            return (ms * SAMPLE_RATE);
        }
        static inline float ConvertFrameToSec(float frame)
        {
            return (frame / SAMPLE_RATE);
        }
};

#endif








