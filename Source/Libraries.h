#pragma once

//----//
//JUCE//
//----//

#define DONT_SET_USING_JUCE_NAMESPACE 1
#include "../JuceLibraryCode/JuceHeader.h"
#include <limits.h>

//-----------//
// S7 Scheme //
//-----------//

#include "s7.h"

//---------------//
//Other Libraries//
//---------------//

#ifdef WITH_SDIF
#include "sdif.h"
#endif

#ifdef WITH_LIBLO
#include "lo/lo.h"
#endif

#ifdef WITH_FOMUS
#define FOMUS_TYPESONLY
#include <fomus/fomusapi.h>
#include <fomus/infoapi.h>
#endif

//-----------------//
//Platform-Specific//
//-----------------//

//----------------//
//Standard library//
//----------------//

#include <cctype>
#include <cstdlib>
#include <iostream>
#include <limits>
#include <map>
#include <sstream>
#include <stack>
#include <string>
#include <vector>

#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
