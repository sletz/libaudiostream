/*

Copyright (C) Grame 2002-2013

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
research@grame.fr

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

#ifdef __APPLE__
#include <Accelerate/Accelerate.h>
#endif

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
    
        static inline void CopyChannelsTo(float** dst, float** src, long framesNum, long shift_channel, long channels)
        {
            for (int i = 0; i < channels; i++) {
                memcpy(dst[shift_channel + i], src[i], framesNum * sizeof(float));
            }
        }
        
        static inline void MixChannelsTo(float** dst, float** src, long framesNum, long shift_channel, long channels)
        {
            
            for (int i = 0; i < channels; i++) {
                for (int j = 0; j < framesNum; j++) {
                    dst[shift_channel + i][j] += src[i][j];
                }
            }
        }

        static inline void MonoToStereo(short* dst, short* src, long framesNum)
        {
            do {
                int x = *src++;
                *dst += x;
                dst++;
                *dst += x;
                dst++;
            } while (--framesNum);
        }

        static inline void MixMonoToStereoBlk(long* dst, short* src, long framesNum, short leftamp, short rightamp)
        {
            long x, y, i, j;

            for (i = 0, j = 0 ; i < framesNum; i += 2, j += 4) {
                x = src[i];
                y = src[i + 1];
                dst[j] += (x * leftamp);
                dst[j + 1] += (x * rightamp);
                dst[j + 2] += (y * leftamp);
                dst[j + 3] += (y * rightamp);
            }
        }

        static inline void MixMonoToStereoBlk(float* dst, float* src, long framesNum, float leftamp, float rightamp)
        {
            float x, y;
            long i, j;

            for ( i = 0, j = 0 ; i < framesNum; i += 2, j += 4) {
                x = src[i];
                y = src[i + 1];
                dst[j] += (x * leftamp);
                dst[j + 1] += (x * rightamp);
                dst[j + 2] += (y * leftamp);
                dst[j + 3] += (y * rightamp);
            }
        }

        static inline void MixStereoToStereoBlk(long* dst, short* src, long framesNum, short leftamp, short rightamp)
        {
            long x, y ;

            for (long i = 0 ; i < framesNum; i += 2) {
                x = src[i];
                y = src[i + 1];
                dst [i] += (x * leftamp);
                dst [i + 1] += (y * rightamp);
            }
        }

        static inline void MixStereoToStereoBlk(float* dst, float* src, long framesNum, float leftamp, float rightamp)
        {
            float x, y ;

            for (long i = 0 ; i < framesNum; i += 2) {
                x = src[i];
                y = src[i + 1];
                dst [i] += (x * leftamp);
                dst [i + 1] += (y * rightamp);
            }
        }
		
		static inline void MixFrameToFrameBlk(float* dst, float* src, long framesNum, long channels)
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
        
        static inline void MixFrameToFrameBlk(float** dst, float** src, long framesNum, long channels)
        {
     		for (int i = 0; i < channels; i++) {
                for (int j = 0; j < framesNum; j++) { 
                    dst[i][j] += src[i][j];
                }
            }
    	}

        static inline void MixFrameToFrameBlk(float* dst, float* src, long framesNum, long channels, float leftamp, float rightamp)
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
		
		static inline void MixFrameToFrameBlk(float* dst, float* src, long framesNum, long channels, float leftamp_L, float rightamp_L, float leftamp_R, float rightamp_R)
        {
			for (int i = 0 ; i < framesNum; i++) {
                for (int j = 0 ; j < channels; j += 2) { // A REVOIR
                    int index1 = i * channels + j;
                    int index2 = index1 + 1;
					dst[index1] += (src[index1] * leftamp_L) + (src[index2] * leftamp_R);
                    dst[index2] += (src[index1] * rightamp_L) + (src[index2] * rightamp_R);
                }
            }
		}
		
		static inline void MixFrameToFrameBlk1(float* dst, float* src, long framesNum, long channels)
        {
            for (int i = 0 ; i < framesNum; i++) {
                for (int j = 0 ; j < channels; j++) { 
                    long index1 = i * channels + j;
                    dst[index1] += src[index1];
                }
            }
        }
        
        static inline void MixFrameToFrameBlk1(float** dst, float** src, long framesNum, long channels)
        {
            for (int i = 0 ; i < channels; i++) {
                for (int j = 0 ; j < framesNum; j++) { 
                    dst[i][j] += src[i][j];
                }
            }
        }

        static inline void ReplaceFrameToFrameBlk(float* dst, float* src, long framesNum, long channels, float leftamp, float rightamp)
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

        static inline void ReplaceFrameToFrameBlk1(float* dst, float* src, long framesNum, long channels)
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
            memset(dst, 0, sizeof(long) * nbsamples);
        }

        static inline void ZeroStereoBlk(short* dst, long nbsamples)
        {
            memset(dst, 0, sizeof(short) * nbsamples);
        }

        static inline void ZeroStereoBlk(float* dst, long nbsamples)
        {
            memset(dst, 0, sizeof(float) * nbsamples);
        }

        static inline void ZeroFloatBlk(float* dst, long framesNum, long channels)
        {
            memset(dst, 0, sizeof(float) * framesNum * channels);
        }
        
        static inline void ZeroFloatBlk(float** dst, long framesNum, long channels)
        {
            for (int i = 0; i < channels; i++) {
                memset(dst[i], 0, sizeof(float) * framesNum);
            }
        }

        static inline void ZeroShortBlk(short* dst, long framesNum, long channels)
        {
            memset(dst, 0, sizeof(short) * framesNum * channels);
        }

        static inline void Float2ShortStereo(float* in, short* out, long nbsamples)
        {
            for (long i = 0;i < nbsamples; i++) {
                out[i] = (in[i] >= 1.0) ? SHRT_MAX : (in[i] <= -1.0) ? SHRT_MIN : short(in[i] * float(SHRT_MAX));
            }
        }

        static inline void Float2LongMulti(float* in, long * out, long framesNum, long nbchan, long chan1, long chan2)
        {
            for (long i = 0;i < framesNum; i++) {
                out[(i*nbchan) + chan1]
                = (in[i * 2] >= 1.0) ? LONG_MAX : (in[i * 2] <= -1.0) ? LONG_MIN : long(in[i * 2] * float(LONG_MAX));
                out[(i*nbchan) + chan2]
                = (in[i * 2 + 1] >= 1.0) ? LONG_MAX : (in[i * 2 + 1] <= -1.0) ? LONG_MIN : long(in[i * 2 + 1] * float(LONG_MAX));
            }
        }

        static inline void Short2FloatStereo(short* in, float* out, long nbsamples)
        {
            float fGain = 1.0f / float(SHRT_MAX);
            for (long i = 0; i < nbsamples; i++) {
                out [i] = float(in[i]) * fGain;
            }
        }

        static inline void Short2FloatMono(short* in, float* out, long nbsamples)
        {
            float sample;
            float fGain = 1.0f / float(SHRT_MAX);

            for (long i = 0; i < nbsamples; i++) {
                sample = float(in[i]) * fGain;
                out [i*2] = sample;
                out [i*2 + 1] = sample;
            }
        }
		
		static inline void Interleave(float* dst, float** src, long framesNum, long channels)
        {
            printf("Interleave %d %d\n", framesNum, channels);
			int i, j;
			for (i = 0; i < framesNum; i++) {
				for (j = 0; j < channels; j++) {
					dst[i * channels + j] = src[j][i]; 
				}
			}
        }
		
		static inline void Deinterleave(float** dst, float* src, long framesNum, long channels)
        {
            printf("Deinterleave %d %d\n", framesNum, channels);
         	int i, j;
			for (i = 0; i < framesNum; i++) {
				for (j = 0; j < channels; j++) {
					dst[j][i] = src[i * channels + j];
				}
			}
        }
        
        static inline void Adapt(float** dst, float** src, long framesNum, long channelsIn, long channelsOut)
        {
			if (channelsIn < channelsOut) {
                for (int split = 0; split < channelsOut/channelsIn; split++) {
                    // TODO
                    memcpy(dst[split], src[split], framesNum * sizeof(float));
                }
            } else if (channelsIn == channelsOut) {
                for (int split = 0; split = channelsOut; split++) {
                    memcpy(dst[split], src[split], framesNum * sizeof(float));
                }
            } else {
                for (int split = 0; split < channelsIn/channelsOut; split++) {
                    // TODO
                    memcpy(dst[split], src[split], framesNum * sizeof(float));
                }
            }
        }

        static inline float ClipFloat (float sample)
        {
            return (sample < -1.0f) ? -1.0f : (sample > 1.0f) ? 1.0f : sample;
        }

        static inline void Short2Float(short* in, float* out, long framesNum, long channelsIn, long channelsOut)
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

		static inline void Short2FloatMix(short* in, float* out, long framesNum, long channelsIn, long channelsOut)
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
			
			/*
			for (long i = 0; i < framesNum * channelsOut; i += 4) {
				out[i] += float(in[i]) * fGain;
				out[i + 1] += float(in[i + 1]) * fGain;
				out[i + 2] += float(in[i + 2]) * fGain;
				out[i + 3] += float(in[i + 3]) * fGain;
			}
			*/
		
			// Works only on Tiger... removed for now
			#ifdef __APPLE__
				float buffer[framesNum * channelsOut];
				vDSP_vflt16(in, 1, buffer, 1, framesNum * channelsOut);
				vDSP_vsma(buffer, 1, &fGain, out, 1, out, 1, framesNum * channelsOut);
			#else
				for (long i = 0; i < framesNum * channelsOut; i += 4) {
                    out[i] += float(in[i]) * fGain;
                    out[i + 1] += float(in[i + 1]) * fGain;
                    out[i + 2] += float(in[i + 2]) * fGain;
                    out[i + 3] += float(in[i + 3]) * fGain;
                }
			#endif
			
			}
        }
	  		
        static inline void Float2Short(float* in, short* out, long framesNum, long channelsIn, long channelsOut)
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

        static inline void Float2ShortMix(float* in, short* out, long framesNum, long channelsIn, long channelsOut)
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

        static inline void Float2FloatMix(float* in, float* out, long framesNum, long channelsIn, long channelsOut)
        {
            if (channelsIn < channelsOut) {  // distribute channels
                for (long i = 0; i < framesNum; i++) {
                    for (long j = 0; j < channelsOut; j++) {
                        out [i*channelsOut + j] += in[i * channelsIn + j % channelsIn];
                    }
                }
            } else {
                for (long i = 0; i < framesNum * channelsOut; i += 4) {
                    out[i] += in[i];
                    out[i + 1] += in[i + 1];
                    out[i + 2] += in[i + 2];
                    out[i + 3] += in[i + 3];
                }
            }
        }
        
        static inline void Float2FloatMix(float** in, float** out, long framesNum, long channels)
        {
            for (long i = 0; i < channels; i ++) {
                for (long j = 0; j < framesNum; j += 4) {
                    out[i][j] += in[i][j];
                    out[i][j + 1] += in[i][j + 1];
                    out[i][j + 2] += in[i][j + 2];
                    out[i][j + 3] += in[i][j + 3];
                }
            }
        }
        
        static inline void Float2Float(float* in, float* out, long framesNum, long channelsIn, long channelsOut)
        {
            if (channelsIn < channelsOut) {  // distribute channels
                for (long i = 0; i < framesNum; i++) {
                    for (long j = 0; j < channelsOut; j++) {
                        out [i*channelsOut + j] = in[i * channelsIn + j % channelsIn];
                    }
                }
            } else {
                for (long i = 0; i < framesNum * channelsOut; i += 4) {
                    out[i] = in[i];
                    out[i + 1] = in[i + 1];
                    out[i + 2] = in[i + 2];
                    out[i + 3] = in[i + 3];
                }
            }
        }
        
        static inline void Float2Float(float** in, float** out, long framesNum, long channels)
        {
            for (long i = 0; i < channels; i ++) {
                for (long j = 0; j < framesNum; j += 4) {
                    out[i][j] += in[i][j];
                    out[i][j + 1] += in[i][j + 1];
                    out[i][j + 2] += in[i][j + 2];
                    out[i][j + 3] += in[i][j + 3];
                }
            }        
        }

        static inline void MultFrame(float* frame, float val, long size)
        {
            // A optimiser
            for (int i = 0 ; i < size; i++) {
                frame[i] *= val;
            }
        }
        
        static inline void MultFrame(float** frame, float val, long channels)
        {
            // A optimiser
            for (int i = 0 ; i < channels; i++) {
                frame[i][0] *= val;
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

