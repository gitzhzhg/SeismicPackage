/*<license>
 -------------------------------------------------------------------------------
  Copyright (c) 2007 ConocoPhillips Company
 
  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:
 
  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.
 
  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
 -------------------------------------------------------------------------------
 </license>*/

//------------------------- sl_sep.hh -------------------------------//
//------------------------- sl_sep.hh -------------------------------//
//------------------------- sl_sep.hh -------------------------------//

//                header file for the SLSep class
//                derived from the SLDelay class
//                       subdirectory sl

#ifndef _SL_SEP_HH_
#define _SL_SEP_HH_

#include "sl/sl_delay.hh"
#include <Xm/Separator.h>

class SLSep : public SLDelay
{

//-------------------------- data ----------------------------//
//-------------------------- data ----------------------------//
//-------------------------- data ----------------------------//

public:

  enum { _HORIZONTAL, _VERTICAL, _UNKNOWN };

private:

  long _direction;            // one of the above enums

//------------------------- functions ---------------------------//
//------------------------- functions ---------------------------//
//------------------------- functions ---------------------------//

public:

  SLSep (SLDelay *slparent, char *name, long direction = _UNKNOWN);
  SLSep (Widget    wparent, char *name, long direction = _UNKNOWN);
  SLSep (Widget    w);
  virtual ~SLSep (void);

  virtual WidgetClass topClass() { return xmSeparatorWidgetClass; }

  virtual Widget make (Widget p = NULL);  // overrides SLDelay

//--------------------------- end functions -------------------------//
//--------------------------- end functions -------------------------//
//--------------------------- end functions -------------------------//

};

#endif

//---------------------------- end ----------------------------------//
//---------------------------- end ----------------------------------//
//---------------------------- end ----------------------------------//
