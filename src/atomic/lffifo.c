/*
  MidiShare Project
  Copyright (C) Grame 1999-2005

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  Grame Research Laboratory, 11, cours de Verdun Gensoul 69002 Lyon - France
  research@grame.fr

*/

#include "lffifo.h"

#if defined(__GNUC__)
# if defined(powerpc) || defined(__ppc__)
#  include "lffifoppc.c"
# else
#  include "lffifoIntel.c"
# endif

#elif defined(WIN32)
#  include "lffifoIntel.c"

#else
# error "lffifo.c : target compiler and processor undefined"
#endif
