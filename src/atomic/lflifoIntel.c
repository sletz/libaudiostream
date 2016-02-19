/*
  MidiShare Project
  Copyright (C) Grame 1999-2005

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr

*/

#include "stdio.h"
#include "lflifo.h"

//#define __trace__

void lfinit(lifo* lf)
{
#ifdef __trace__
printf("lfinit %p\n", lf);
#endif
	lf->top = 0;
	lf->count.value = 0;
	lf->oc = 0;
}

void lfpush (lifo * lf, lifocell * cl)
{
#ifdef __trace__
printf("lfpush %p %p\n", lf, cl);
#endif
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
#ifdef __trace__
printf("lfpop %p ", lf);
#endif
	volatile  atomic_long oc;
	lifocell * volatile top;
	do {
		oc =  lf->oc;
		top = lf->top;
		if (!top) return 0;
	} while (!CAS2 (&lf->top, top, oc, top->link, oc+1));
	msAtomicDec (&lf->count);
#ifdef __trace__
printf("%p\n", top);
#endif
	return top;
}
