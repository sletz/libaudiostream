/*
  MidiShare Project
  Copyright (C) Grame 1999-2005

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr

*/


#ifndef __msAtomicWin32__
#define __msAtomicWin32__

#define inline __inline

#ifndef __x86_64__

#ifdef __SMP__
#	define LOCK lock
#else
#	define LOCK 
#endif

//----------------------------------------------------------------
// CAS functions
//----------------------------------------------------------------
inline char CAS (volatile void * addr, volatile void * value, void * newvalue) 
{
	register char c;
	__asm {
		push	ebx
		push	esi
		mov		esi, addr
		mov		eax, value
		mov		ebx, newvalue
		LOCK cmpxchg dword ptr [esi], ebx
		sete	c
		pop		esi
		pop		ebx
	}
	return c;
}

inline char CAS2 (volatile void * addr, volatile void * v1, volatile long v2, void * n1, long n2) 
{
	register char c;
	__asm {
		push	ebx
		push	ecx
		push	edx
		push	esi
		mov		esi, addr
		mov		eax, v1
		mov		ebx, n1
		mov		ecx, n2
		mov		edx, v2
		LOCK    cmpxchg8b qword ptr [esi]
		sete	c
		pop		esi
		pop		edx
		pop		ecx
		pop		ebx
	}
	return c;
}
#else

//extern "C" char CAS  (volatile void * addr, volatile void * value, void * newvalue);
//extern "C" char CAS2 (volatile void * addr, volatile void * v1, volatile void * v2, void * n1, void * n2);


#ifdef __cplusplus
extern "C" {
#endif

char CAS  (volatile void * addr, volatile void * value, void * newvalue);
char CAS2 (volatile void * addr, volatile void * v1, volatile long long v2, void * n1, long long n2);

#ifdef __cplusplus
}
#endif

#endif
#endif
