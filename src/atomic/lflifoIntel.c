/*

  Copyright © Grame 2001-2005

  This library is free software; you can redistribute it and modify it under 
  the terms of the GNU Library General Public License as published by the 
  Free Software Foundation version 2 of the License, or any later version.

  This library is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public 
  License for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

  Grame Research Laboratory, 9, rue du Garet 69001 Lyon - France
  research@grame.fr

*/

#include "lflifo.h"

void lfinit(lifo* lf)
{
	lf->top = 0;
	lf->count.value = 0;
	lf->oc = 0;
}

void lfpush (lifo * lf, lifocell * cl)
{
	lifocell * volatile  top;
	do {
		top = lf->top;
		cl->link = top;
	} while (!CAS (&lf->top, top, cl));
	msAtomicInc (&lf->count);
}

/* 
   on intel architecture, the ABA problem is catched
   using oc (out count) and a CAS2
*/
lifocell* lfpop (lifo * lf)
{
	volatile  long oc;
	lifocell * volatile top;
	do {
		oc =  lf->oc;
		top = lf->top;
		if (!top) return 0;
	} while (!CAS2 (&lf->top, top, oc, top->link, oc+1));
	msAtomicDec (&lf->count);
	return top;
}
