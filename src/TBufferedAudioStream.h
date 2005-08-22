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
research@grame.fr

*/

#ifndef __TBufferedAudioStream__
#define __TBufferedAudioStream__

#include "TAudioStream.h"
#include "TAudioBuffer.h"

/*!
\brief A TBufferedAudioStream object will contains the common double-buffering buffer access code.
*/

/*
A TBufferedAudioStream object will contains the common double-buffering buffer access code.
 
The Read method calls the ReadBuffer method when the end of a "big" buffer is reached.
 
Subclasses will possibly implements ReadBuffer for their special need. For example a sound file subclass
will have a ReadBuffer method that calls the real disk read function inside a low-prority thread.
 
    - A TFileAudioStream object is the LibSndFile read stream
 
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
                                --------------------------------------------------                          
                                !                        			 !
				//------------------------------//	  	//------------------------------// 					
				//								//		//								//
                //    TReadFileAudioStream      //		//    TWriteFileAudioStream     //      
                //			        			//		//								//
                //------------------------------//		//------------------------------//   
 
*/

//----------------------------
// Class TBufferedAudioStream
//----------------------------

class TBufferedAudioStream : public TAudioStream
{

    protected:

        TAudioBuffer<short>* fBuffer;

        long fChannels;		// Number of channels
        long fCurFrame;		// Position inside a buffer
        long fFramesNum;	// Total file frames number
        long fTotalFrames;	// Total frames already handled
     
        bool fReady;		// For disk access error detection

        virtual long Write(TAudioBuffer<short>* buffer, long framesNum, long framePos)
        {
            return 0;
        }
        virtual long Read(TAudioBuffer<short>* buffer, long framesNum, long framePos)
        {
            return 0;
        }

        virtual void ReadBuffer(TAudioBuffer<short>* buffer, long framesNum, long framePos);
        virtual void WriteBuffer(TAudioBuffer<short>* buffer, long framesNum, long framePos);

        long HandleBuffer(TAudioBuffer<float>* buffer, long framesNum, long framePos, long channels, bool read_or_write);
        void Init(TAudioBuffer<short>* buffer);

    public:

        TBufferedAudioStream();
        virtual ~TBufferedAudioStream()
        {}

        virtual long Write(TAudioBuffer<float>* buffer, long framesNum, long framePos, long channels);
        virtual long Read( TAudioBuffer<float>* buffer, long framesNum, long framePos, long channels);

        virtual void Reset();
		
		/*
        // Stop the stream
        virtual void Stop()
        {}
		*/
		
        virtual TAudioStreamPtr CutBegin(long frames)
        {
            return 0;
        }
        virtual long Length()
        {
            return 0;
        }
        virtual long Channels()
        {
            return fChannels;
        }
        virtual TAudioStreamPtr Copy()
        {
            return new TBufferedAudioStream();
        } // Le buffer interne est partagé??
};

typedef TBufferedAudioStream * TBufferedAudioStreamPtr;

#endif
