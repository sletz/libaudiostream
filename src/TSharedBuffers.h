/*

Copyright (C) Grame 2002-2013

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

#ifndef __TSharedBuffers_
#define __TSharedBuffers_

// Global input/output buffers

//----------------------
// Class TSharedBuffers
//----------------------
/*!
\brief Shared audio buffers.
*/

class TSharedBuffers
{
  
    public:

        static float** fInBuffer;
        static float** fOutBuffer;
        
        static long fInputOffset;
        static long fOutputOffset;

        static float** GetInBuffer()
        {
            return fInBuffer;
        }
        
        static float** GetInBuffer(long framesNum, long channels, float** res)
        {
            for (int i = 0; i < channels; i++) {
                res[i] = &fInBuffer[i][fInputOffset];
            }
            fInputOffset += framesNum;
            return res;
        }
        
        static float** GetOutBuffer()
        {
            return fOutBuffer;
        }
        
        static float** GetOutBuffer(long framesNum, long channels, float** res)
        {
            for (int i = 0; i < channels; i++) {
                res[i] = &fOutBuffer[i][fOutputOffset];
            }
            fOutputOffset += framesNum;
            return res;
        }

        static void SetInBuffer(float** input)
        {
            fInBuffer = input;
            fInputOffset = 0;
        }
        
        static void SetOutBuffer(float** output)
        {
            fOutBuffer = output;
            fOutputOffset = 0;
        }
};

#endif
