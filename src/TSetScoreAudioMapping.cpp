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

#ifdef WIN32
#pragma warning (disable : 4786)		// inhibit warnings for empty instructions
#endif


#include "TSetScoreAudioMapping.h"

TSetScoreAudioMapping::TSetScoreAudioMapping(long sr, long tpq): TScoreAudioMapping(sr)
{
    fDateMs = fMusicPos = 0;
    fTranslator = NULL;
    fTPQ = tpq;
}

TSetScoreAudioMapping::~TSetScoreAudioMapping()
{
    delete fTranslator;
}

void TSetScoreAudioMapping::addPos(TPos& pos)
{
}

void TSetScoreAudioMapping::addPos(long mspos, long musicpos)
{
    if (mspos == 0x7FFFFFF && musicpos == 0x7FFFFFF) { // last pos
        fTranslator = new TTranslator(fSegmentation1, fSegmentation2, fMapping, fTPQ);
    } else {
        mspos = ConvertMs2Sample(mspos);

        TSegment s1(fDateMs, mspos);
        TSegment s2(fMusicPos, musicpos);

        fSegmentation1.insert(s1);
        fSegmentation2.insert(s2);

        fMapping.insert(TLink(s1, s2));

        fDateMs = mspos;
        fMusicPos = musicpos;
    }
}

long TSetScoreAudioMapping::sample2MusicPosition(long samplepos)
{
    return fTranslator->directFirst(samplepos);
}

long TSetScoreAudioMapping::music2SamplePosition(long musicpos)
{
    return fTranslator->inverseLast(musicpos);
}

void TSetScoreAudioMapping::Print()
{
}

long TSetScoreAudioMapping::Size()
{
    return fMapping.size();
}

long TSetScoreAudioMapping::ms2MusicPosition(long mspos)
{
    return sample2MusicPosition(ConvertMs2Sample(mspos));
}
long TSetScoreAudioMapping::music2MsPosition(long musicpos)
{
    return ConvertSample2Ms(music2SamplePosition(musicpos));
}
