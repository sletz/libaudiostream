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

#ifndef __TSetScoreAudioMapping__
#define __TSetScoreAudioMapping__

#include "TScoreAudioMapping.h"
#include "TMapping.h"

//------------------------------
// Class TSetScoreAudioMapping
//------------------------------
/*!
\brief A TSetScoreAudioMapping object convert a date in samples in a date in musical time and the contrary.
*/

class TSetScoreAudioMapping : public TScoreAudioMapping
{

    private:

        TSegmentation fSegmentation1;
        TSegmentation fSegmentation2;
        TMapping fMapping;
        TTranslator* fTranslator;
        long fDateMs, fMusicPos;
        long fTPQ;

    public:

        TSetScoreAudioMapping(long sr, long tpq);
        virtual ~TSetScoreAudioMapping();

        void addPos(TPos& pos);
        void addPos(long ms, long musicpos);

        long sample2MusicPosition(long samplepos);
        long music2SamplePosition(long musicpos);

        long ms2MusicPosition(long mspos);
        long music2MsPosition(long musicpos);

        void Print();
        long Size();
};

typedef TSetScoreAudioMapping * TSetScoreAudioMappingPtr;

#endif



