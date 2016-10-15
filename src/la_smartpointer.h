/*
 Copyright (C) Grame 2003-2013

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  Grame Research Laboratory, 9 rue du Garet, 69001 Lyon - France
  research@grame.fr

*/

#ifndef __la_smartpointer__
#define __la_smartpointer__

#include <cassert>
#include <atomic>
#include <cinttypes>
#include <stdio.h>
#include "AudioExports.h"

#ifdef WIN32
#pragma warning (disable : 4786)
#endif


/*!
\brief the base class for smart pointers implementation

    Any object that want to support smart pointers should
    inherit from the smartable class which provides reference counting
    and automatic delete when the reference count drops to zero.
*/
class AUDIO_EXPORTS la_smartable {

    public:
        //! gives the reference count of the object
        unsigned refs() const         { return refCount; }
        //! addReference increments the ref count and checks for refCount overflow
        la_smartable* addReference()           { refCount++; assert(refCount != 0); return this; }
        //! removeReference delete the object when refCount is zero
        virtual void removeReference();

    protected:
        std::atomic_ulong refCount;
        la_smartable() : refCount(0) {}
        la_smartable(const la_smartable&): refCount(0) {}
        //! destructor checks for non-zero refCount
        virtual ~la_smartable()
        {
            assert (refCount == 0);
            printf("~la_smartable %" PRIxPTR "\n", (uintptr_t)this);
        }
        la_smartable& operator=(const la_smartable&) { return *this; }
};

class TCmdManager;

class AUDIO_EXPORTS la_smartable1 : public la_smartable {

    private:

        static void removeReferenceAux(la_smartable1* obj, long u1, long u2, long u3);
        static TCmdManager* fDeleteManager;

    public:

        void removeReference();
        static void Init();
        static void Destroy();
};

/*!
\brief the smart pointer implementation

    A smart pointer is in charge of maintaining the objects reference count
    by the way of pointers operators overloading. It supports class
    inheritance and conversion whenever possible.
\n	Instances of the SMARTP class are supposed to use \e smartable types (or at least
    objects that implements the \e addReference and \e removeReference
    methods in a consistent way).
*/
template<class T> class LA_SMARTP {
    private:
        //! the actual pointer to the class
        T* fSmartPtr;

    public:
        //! an empty constructor - points to null
        LA_SMARTP()	: fSmartPtr(nullptr) {}
        //! build a smart pointer from a class pointer
        LA_SMARTP(T* rawptr) : fSmartPtr(rawptr)  {
            printf("LA_SMARTP %" PRIxPTR "\n", (uintptr_t)rawptr);
            if (fSmartPtr) fSmartPtr->addReference();
        }
        //! build a smart pointer from an convertible class reference
        template<class T2>
        LA_SMARTP(const LA_SMARTP<T2>& ptr) : fSmartPtr((T*)ptr) { if (fSmartPtr) fSmartPtr->addReference(); }
        //! build a smart pointer from another smart pointer reference
        LA_SMARTP(const LA_SMARTP& ptr) : fSmartPtr((T*)ptr)     { if (fSmartPtr) fSmartPtr->addReference(); }

        //! the smart pointer destructor: simply removes one reference count
        ~LA_SMARTP()  { if (fSmartPtr) fSmartPtr->removeReference(); }

        //! cast operator to retrieve the actual class pointer
        operator T*() const  { return fSmartPtr;	}

        //! '*' operator to access the actual class pointer
        T& operator*() const {
            // checks for null dereference
            assert (fSmartPtr != nullptr);
            return *fSmartPtr;
        }

        //! operator -> overloading to access the actual class pointer
        T* operator->() const	{
            // checks for null dereference
            assert (fSmartPtr != nullptr);
            return fSmartPtr;
        }

        //! operator -> overloading to access the actual class pointer
        T* getPointer() const	{
            // checks for null dereference
            assert (fSmartPtr != nullptr);
            return fSmartPtr;
        }

        //! operator = that moves the actual class pointer
        template <class T2>
        LA_SMARTP& operator=(T2 p1_)	{ *this=(T*)p1_; return *this; }

        //! operator = that moves the actual class pointer
        LA_SMARTP& operator=(T* p_)	{
            // check first that pointers differ
            if (fSmartPtr != p_) {
                // increments the ref count of the new pointer if not null
                if (p_ != nullptr) p_->addReference();
                // decrements the ref count of the old pointer if not null
                if (fSmartPtr != nullptr) fSmartPtr->removeReference();
                // and finally stores the new actual pointer
                fSmartPtr = p_;
            }
            return *this;
        }
        //! operator = to support inherited class reference
        LA_SMARTP& operator=(const LA_SMARTP<T>& p_)                { return operator=((T *) p_); }
        //! dynamic cast support
        template<class T2> LA_SMARTP& cast(T2* p_)               { return operator=(dynamic_cast<T*>(p_)); }
        //! dynamic cast support
        template<class T2> LA_SMARTP& cast(const LA_SMARTP<T2>& p_) { return operator=(dynamic_cast<T*>(p_)); }
};

#endif
