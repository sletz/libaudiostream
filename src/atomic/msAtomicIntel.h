/*
  MidiShare Project
  Copyright (C) Grame 1999-2005

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr

*/


#ifndef __msAtomicIntel__
#define __msAtomicIntel__

#ifdef __SMP__
#	define LOCK "lock ; "
#else
#	define LOCK ""
#endif

//----------------------------------------------------------------
// CAS functions
//----------------------------------------------------------------
static inline char CAS (volatile void * addr, volatile void * value, void * newvalue) 
{
	register char ret;
	__asm__ __volatile__ (
		"# CAS \n\t"
		LOCK "cmpxchg %2, (%1) \n\t"
		"sete %0               \n\t"
		:"=a" (ret)
		:"c" (addr), "d" (newvalue), "a" (value)
	);
	return ret;
}

#if (defined(__APPLE__) && !defined(__x86_64__))

/*
On MacIntel, version 4.0.1 of the gcc compiler gives the following error:
can't find a register in class 'BREG' while reloading 'asm'
To solve that, %%ebx register has to be saved and restored.
*/

static inline char CAS2 (volatile void * addr, volatile void * v1, volatile long v2, void * n1, long n2)
{
	register char ret;
	__asm__ __volatile__ (
		"# CAS2 \n\t"
		"xchgl %%esi, %%ebx \n\t"
		LOCK "cmpxchg8b (%1) \n\t"
		"sete %0             \n\t"
		"xchgl %%ebx, %%esi \n\t"  /* Restore %ebx.  */
		:"=a" (ret)
		:"D" (addr), "d" (v2), "a" (v1), "S" (n1), "c" (n2)
	);
	return ret;
}

#else

static inline char CAS2 (volatile void * addr, volatile void * v1, volatile long v2, void * n1, long n2)
{
	register char ret;

	__asm__ __volatile__ (
		"# CAS2 \n\t"
#ifdef __x86_64__
		LOCK "cmpxchg16b (%1) \n\t"
#else
		LOCK "cmpxchg8b (%1) \n\t"
#endif
		"sete %0               \n\t"
		:"=a" (ret)
		:"D" (addr), "d" (v2), "a" (v1), "b" (n1), "c" (n2)
	);
	return ret;
}

#endif

#endif
