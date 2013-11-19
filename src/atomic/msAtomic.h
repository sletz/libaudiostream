/*
  MidiShare Project
  Copyright (C) Grame 1999-2005

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr

*/

#ifndef __msAtomic__
#define __msAtomic__

/* by default, compiles for SMP architecture */
#define __SMP__

#ifdef __x86_64__
typedef long long	atomic_long;
#else
typedef long atomic_long;
#endif

#if defined(powerpc) || defined(__ppc__) || defined(__MWERKS__)
	typedef struct {
		long value;
		long reserved[7];
	} TAtomic;
#else
	typedef struct {
		atomic_long value;
	} TAtomic;
#endif


#if defined(__GNUC__)
# if defined(powerpc) || defined(__ppc__)
#  include "msAtomicPPC.h"
# else
#  include "msAtomicIntel.h"
# endif

#elif defined(WIN32)
# include "msAtomicWin32.h"

#elif defined(__MWERKS__)  /* this is for CodeWarrior compiler on Macintosh */
# include "msAtomicPPC_CW.h"
#else
# error "msAtomic.h : target compiler and processor undefined"
#endif

static inline atomic_long msAtomicInc (volatile TAtomic * val)
{
    atomic_long actual;
    do {
        actual = val->value;
    } while (!CAS(val, (void *)actual, (void *)(actual+1)));
	return actual;
}

static inline atomic_long msAtomicDec (volatile TAtomic * val)
{
    atomic_long actual;
    do {
        actual = val->value;
    } while (!CAS(val, (void *)actual, (void *)(actual-1)));
	return actual;
}

#endif
