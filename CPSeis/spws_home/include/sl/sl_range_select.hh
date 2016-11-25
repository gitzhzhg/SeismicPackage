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

//-------------------- sl_range_select.hh -------------------//
//-------------------- sl_range_select.hh -------------------//
//-------------------- sl_range_select.hh -------------------//

//           header file for the SLRangeSelect class
//              derived from the SLSmartForm class
//                        subdirectory sl


#ifndef _SL_RANGE_SELECT_HH_
#define _SL_RANGE_SELECT_HH_

#include "sl/sl_smart_form.hh"

typedef void RangeSelectTrap (void *data, long value);

class SL2Scale;
class SLpPush;
class SLpText;


class SLRangeSelect : public SLSmartForm
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:

  long _value;
  long _minimum;
  long _maximum;
  long _page;

  RangeSelectTrap *_trap;
  void            *_data;

  SL2Scale       *_scale;
  SLSmartForm    *_control;
  SLpPush        *_min;
  SLpPush        *_prev;
  SLpText        *_valtext;
  SLpPush        *_next;
  SLpPush        *_max;

//------------------------ functions -------------------------//
//------------------------ functions -------------------------//
//------------------------ functions -------------------------//


public:          // constructors and destructor

  SLRangeSelect (SLDelay *slparent, char *name, HelpCtx hctx = NULL);
  SLRangeSelect (Widget    wparent, char *name, HelpCtx hctx = NULL);
  virtual ~SLRangeSelect();

private:

  void constructorHelper();

public:        // methods to get values

  long value  ()  const  { return _value  ; }
  long minimum()  const  { return _minimum; }
  long maximum()  const  { return _maximum; }
  long page   ()  const  { return _page   ; }

public:        // methods to set values

  void setValues  (long value, long minimum, long maximum,
                                             long page = 0);
  void setValue   (long value);
  void setMinMax  (long minimum, long maximum);
  void setPage    (long page);
  void setNoValues();

public:       // method to register trap

  void setRangeSelectTrap (RangeSelectTrap *trap, void *data = NULL)
              { _trap = trap; _data = data; }

private:    // traps

  static void scaleTrap (void *data, long ident, long oldvar, long newvar);
  static void  pushTrap (void *data, long ident);
  static long  senseFunValue (void *data);
  static long  senseFunLower (void *data);
  static long  senseFunUpper (void *data);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
