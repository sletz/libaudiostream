/*

Copyright (C) Grame 2013

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

#ifndef __TLASException__
#define __TLASException__

#include <stdexcept>
#include <iostream>

//---------------------
// Class TLASException
//---------------------

class TLASException : public std::runtime_error {

    public:

        TLASException(const std::string& msg) : std::runtime_error(msg)
        {}
        TLASException(char* msg) : std::runtime_error(msg)
        {}
        TLASException(const char* msg) : std::runtime_error(msg)
        {}

        std::string Message()
        {
            return what();
        }

        void PrintMessage()
        {
            std::cerr << what();
        }
};

#define TRY_CALL                    \
    TAudioGlobals::ClearLibError(); \
    try {                           \
    
#define CATCH_EXCEPTION                                     \
    } catch (TLASException& e) {                            \
        printf("LAS error = %s", e.Message().c_str());      \
        TAudioGlobals::AddLibError(e.Message());            \
    } catch (...) {                                         \
        printf("LAS runtime error");                        \
        TAudioGlobals::AddLibError("LAS runtime error");    \
    }                                                       \
    
#define CATCH_EXCEPTION_RETURN                              \
    } catch (TLASException& e) {                            \
        printf("LAS error = %s", e.Message().c_str());      \
        TAudioGlobals::AddLibError(e.Message());            \
        return 0;                                           \
    } catch (...) {                                         \
        printf("LAS runtime error");                        \
        TAudioGlobals::AddLibError("LAS runtime error");    \
        return 0;                                           \
    }     
#endif
