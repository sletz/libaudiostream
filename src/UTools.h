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


// ===========================================================================
//	 UTools.h
// ===========================================================================
//
//	Utilities functions
//


#ifndef __UTools__
#define __UTools__


//--------------
// Class UTools
//--------------
/*!
\brief Utility methods
*/

class UTools
{

    public :

        static void SendVal(long val);
        static void MidiPrintText ( char * s);
        inline static long Min (long a, long b)
        {
            return (a < b) ? a : b;
        }
        inline static long Max (long a, long b)
        {
            return (a >= b) ? a : b;
        }

};



#endif




