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


//------------------------- slp_arrow.hh -------------------------------//
//------------------------- slp_arrow.hh -------------------------------//
//------------------------- slp_arrow.hh -------------------------------//

//               header file for the SLpArrow class
//                 derived from the SLpBase class
//                      subdirectory sl

#ifndef _SLP_ARROW_HH_
#define _SLP_ARROW_HH_

#include "sl/slp_base.hh"


class SLpArrow : public SLpBase
{

//---------------------- data -------------------------//
//---------------------- data -------------------------//
//---------------------- data -------------------------//

public:

  enum { _LEFT, _RIGHT, _UP, _DOWN, _UNKNOWN };

private:

  long  _direction;           // one of the above enums
///  Pixel _foreground, _background;   /// removed for motif 1.2

//--------------------- functions -------------------------//
//--------------------- functions -------------------------//
//--------------------- functions -------------------------//

public:

  SLpArrow (SLDelay *slparent, char *name, long ident = 0,
                                   long direction = _UNKNOWN);
  SLpArrow (Widget    wparent, char *name, long ident = 0,
                                   long direction = _UNKNOWN);
  SLpArrow (Widget    w,                   long ident = 0);

  virtual ~SLpArrow (void);
  virtual Widget make (Widget p = NULL);   // overrides SLDelay

private:

  virtual void setSenseResource (void);    // overrides PrimSupport

//--------------------- end of functions ------------------------//
//--------------------- end of functions ------------------------//
//--------------------- end of functions ------------------------//

} ;

#endif

//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
