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

#if defined(__GNUC__)
# if defined(powerpc) || defined(__ppc__)
#  include "lflifoppc.c"
# else
#  include "lflifoIntel.c"
# endif

#elif defined(WIN32)
#  include "lflifoIntel.c"

#else
# error "lflifo.c : target compiler and processor undefined"
#endif
