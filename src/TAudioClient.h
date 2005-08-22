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

#ifndef __TAudioClient__
#define __TAudioClient__

#include <assert.h>

//--------------------
// Class TAudioClient
//--------------------
/*!
\brief The base class for Audio clients
*/

class TAudioClient
{

    public:

        TAudioClient()
        {}
        virtual ~TAudioClient()
        {}

        virtual bool AudioCallback(float* inputBuffer, float* outputBuffer, long frames) = 0;
};

typedef TAudioClient * TAudioClientPtr;

#endif



