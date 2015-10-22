/*
  MidiShare Project
  Copyright (C) Grame 1999-2005

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr

*/

#include "lflifo.h"

void lfinit(lifo* lf)
{
	lf->top = 0;
	lf->count.value = 0;
}

void lfpush (lifo * lf, lifocell * cl)
{
	lifocell * volatile top;
	do {
		top = lf->top;
		cl->link = top;
	} while (!CAS (&lf->top, top, cl));
	msAtomicInc (&lf->count);
}

/* 
   on ppc architecture, the ABA problem is catched
   by the memory reservation system
*/
lifocell* lfpop (lifo * lf)
{
	lifocell * volatile top;
	do {
		LWARX(&lf->top);
		top = lf->top;
		if (!top) return 0;
	} while (!STWCX (&lf->top, top, top->link));
	msAtomicDec (&lf->count);
	return top;
}

