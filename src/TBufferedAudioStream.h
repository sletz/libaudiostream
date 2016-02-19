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

#ifndef __TBufferedAudioStream__
#define __TBufferedAudioStream__

#include "TAudioStream.h"
#include "TAudioBuffer.h"
#include "UTools.h"

/*!
\brief A TBufferedAudioStream object contains the common double-buffering buffer access code.
*/

/*
A TBufferedAudioStream object contains the common double-buffering buffer access code.
 
The Read method calls the ReadBuffer method when the end of a "big" buffer is reached.
 
Subclasses will possibly implements ReadBuffer for their special need. For example a sound file subclass
will have a ReadBuffer method that calls the real disk read function inside a low-prority thread.
 
    - A TFileAudioStream object is the LibSndFile stream
 
    - A TReadFileAudioStream object is the LibSndFile read stream
 
    - A TWriteFileAudioStream object is the LibSndFile write stream
 
  
                                        //----------------------------//
                                        //							  //  
                                        //     TBufferedAudioStream   // 
                                        //							  //
                                        //----------------------------//
                                                        !
                                                        !     						
                                                        !							
                                        //------------------------------// 		    
                                        //								//		   
                                        //      TFileAudioStream      	//		      
                                        //								//		    
                                        //------------------------------//
                                                        !
                                                        !
                                                        !     						
                                ---------------------------------------------                         
                                !                        			        !
				//------------------------------//	  	    //------------------------------// 					
				//								//		    //								//
                //    TReadFileAudioStream      //		    //    TWriteFileAudioStream     //      
                //			        			//		    //								//
                //------------------------------//		    //------------------------------//   
 
*/

//----------------------------
// Class TBufferedAudioStream
//----------------------------

class TBufferedAudioStream : public TAudioStream
{

    protected:

        FLOAT_BUFFER fMemoryBuffer;

        long fChannels;     // Number of channels
        long fCurFrame;     // Position inside the buffer
        long fFramesNum;    // Total buffer frames number
        long fTotalFrames;  // Total frames already handled

        volatile bool fReady; // For disk access error detection

        virtual long WriteImp(FLOAT_BUFFER buffer, long framesNum, long framePos)
        {
            return 0;
        }
        virtual long ReadImp(FLOAT_BUFFER buffer, long framesNum, long framePos)
        {
            return 0;
        }

        virtual void ReadBuffer(FLOAT_BUFFER buffer, long framesNum, long framePos);
        virtual void WriteBuffer(FLOAT_BUFFER buffer, long framesNum, long framePos);

        long HandleBuffer(FLOAT_BUFFER buffer, long framesNum, long framePos, bool read_or_write);
  
    public:

        TBufferedAudioStream();
        virtual ~TBufferedAudioStream()
        {}

        virtual long Write(FLOAT_BUFFER buffer, long framesNum, long framePos);
        virtual long Read(FLOAT_BUFFER buffer, long framesNum, long framePos);

        virtual void Reset();
        
        virtual TAudioStreamPtr CutBegin(long frames)
        {
            assert(false);
            return 0;
        }
        
        virtual long Length()
        {
            assert(false);
            return 0;
        }
        
        virtual long Channels()
        {
            return fChannels;
        }
        
        virtual TAudioStreamPtr Copy()
        {
            assert(false);
            return 0;
        } 
        
        FLOAT_BUFFER GetMemoryBuffer() { return fMemoryBuffer; }
};

typedef TBufferedAudioStream * TBufferedAudioStreamPtr;

//----------------------------------
// Class TSharedBufferedAudioStream
//----------------------------------

class TSharedBufferedAudioStream : public TBufferedAudioStream
{
    
    protected:
    
        long fBeginFrame;  // First frame to be read in the shared buffer

    public:
    
        TSharedBufferedAudioStream(long beginFrame, FLOAT_BUFFER buffer): TBufferedAudioStream()
        {
            // Keep the shared buffer
            fMemoryBuffer = buffer;

            fChannels = buffer->GetChannels();
            fFramesNum = fMemoryBuffer->GetSize() - fBeginFrame;
            
            // Start from fBeginFrame
            fBeginFrame = beginFrame;
            Reset();
            
        }
        virtual ~TSharedBufferedAudioStream()
        {}
        
        virtual TAudioStreamPtr CutBegin(long frames)
        {
            return new TSharedBufferedAudioStream(fBeginFrame + frames, fMemoryBuffer);
        }
    
        virtual long Length()
        {
            return fFramesNum;
        }
        
        virtual TAudioStreamPtr Copy()
        {
            return new TSharedBufferedAudioStream(fBeginFrame, fMemoryBuffer);
        } 
        
        void Reset()
        {
            fCurFrame = fBeginFrame;
            fTotalFrames = 0;
        }
        
        long GetPos()
        {
            return fCurFrame;
        }

};

typedef TSharedBufferedAudioStream * TSharedBufferedAudioStreamPtr;

//----------------------------------
// Class TMemoryBufferedAudioStream
//----------------------------------
// Wraps an externally given audio buffer

class TMemoryBufferedAudioStream : public TBufferedAudioStream
{
    
    protected:
        
        long fBeginFrame;       // First frame to be read in the memory buffer
        long fCurWriteFrame;    // Write position inside the buffer
        
        bool fClear;
        
    public:
        
        TMemoryBufferedAudioStream(long beginFrame, FLOAT_BUFFER buffer, bool clear = true):TBufferedAudioStream()
        {
            // Keep the shared buffer
            fMemoryBuffer = buffer;
            
            fChannels = fMemoryBuffer->GetChannels();
            fFramesNum = fMemoryBuffer->GetSize() - fBeginFrame;
            
            // Start from fBeginFrame
            fBeginFrame = beginFrame;
            Reset();
            
            fClear = clear;
        }
        
        virtual ~TMemoryBufferedAudioStream()
        {
            // Will not delete the 'wrapped' buffer
            delete fMemoryBuffer;
        }
        
        long Read(FLOAT_BUFFER buffer, long framesNum, long framePos)
        {
            long cur_pos = fCurFrame;
            long res = TBufferedAudioStream::Read(buffer, framesNum, framePos);
            
            // Clear just read buffer
            if (fClear) {
                float** temp = (float**)alloca(fChannels*sizeof(float*));
                UAudioTools::ZeroFloatBlk(fMemoryBuffer->GetFrame(cur_pos, temp), res, fChannels);
            }
            return res;
        }
        
        virtual long Write(FLOAT_BUFFER buffer, long framesNum, long framePos)
        {
            assert(framePos == 0);  // Entire buffer for now...
            framesNum = UTools::Min(framesNum, fMemoryBuffer->GetSize() - fCurWriteFrame);
            
            float** temp1 = (float**)alloca(fMemoryBuffer->GetChannels()*sizeof(float*));
            float** temp2 = (float**)alloca(buffer->GetChannels()*sizeof(float*));
  
            UAudioTools::Float2Float(buffer->GetFrame(framePos, temp2), fMemoryBuffer->GetFrame(fCurWriteFrame, temp1), framesNum, fChannels);
            
            fCurWriteFrame += framesNum;
            return framesNum;
        }
    
        virtual TAudioStreamPtr CutBegin(long frames)
        {
            return new TMemoryBufferedAudioStream(fBeginFrame + frames, fMemoryBuffer);
        }
        
        virtual long Length()
        {
            return fFramesNum;
        }
        
        virtual TAudioStreamPtr Copy()
        {
            return new TMemoryBufferedAudioStream(fBeginFrame, fMemoryBuffer);
        } 
        
        void Reset()
        {
            fCurFrame = fBeginFrame;
            fCurWriteFrame = fBeginFrame;
            fTotalFrames = 0;
        }
        
        virtual long SetPos(long frames)
        {
            assert(frames <= fMemoryBuffer->GetSize());
            fBeginFrame = frames;
            Reset();
            return NO_ERR;
        }
	      
        long GetPos()
        {
            return fCurFrame;
        }
        
};

typedef TMemoryBufferedAudioStream * TMemoryBufferedAudioStreamPtr;

#endif
