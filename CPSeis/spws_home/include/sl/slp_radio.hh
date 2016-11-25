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


//------------------------- slp_radio.hh -------------------------------//
//------------------------- slp_radio.hh -------------------------------//
//------------------------- slp_radio.hh -------------------------------//

//              header file for the SLpRadio class
//                derived from the SLpBase class
//                      subdirectory sl

#ifndef _SLP_RADIO_HH_
#define _SLP_RADIO_HH_

#include "sl/slp_base.hh"

class RadioList;


class SLpRadio : public SLpBase
{

private:     // data

  RadioList *_radio_list;

public:

  SLpRadio (SLDelay *slparent, char *name, long ident,
                  char *label = NULL, RadioList *radio_list = NULL);
  SLpRadio (Widget    wparent, char *name, long ident,
                  char *label = NULL, RadioList *radio_list = NULL);

  virtual ~SLpRadio (void);
  virtual Widget make (Widget p = NULL); // overrides SLDelay

  void radioListGoingAway ();

public:   // optional convenience functions

  long    radioValue        (void)  const { return ivar(); }
  long    oldRadioValue     (void)  const { return oldIvar(); }
  void setRadioSystemDefault(long x)      { setIvarSystemDefault(x); }
  void setRadioValue        (long x)      { setIvar(x); }
  void setupRadioValue      (long x)            { setupIvarValue(x); }
  void setupRadioPoint      (long *x)           { setupIvarPoint(x); }
  void setupRadioFun (long(*f)(void*), void *d) { setupIvarFun(f,d); }

private:

  virtual void setIvarResource (void);       // overrides PrimSupport
  virtual void setCvarResource (void);       // overrides PrimSupport
  virtual long    ivarResource (void) const; // overrides PrimSupport
  virtual long    ivarDefault  (void) const; // overrides PrimSupport

} ;


#endif

//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
