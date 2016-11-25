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

//------------------------ vfbox_mute.hh --------------------------//
//------------------------ vfbox_mute.hh --------------------------//
//------------------------ vfbox_mute.hh --------------------------//

//               header file for the VfboxMute class
//                derived from the SLDatabox class
//                      subdirectory vfgui

 
#ifndef _VFBOX_MUTE_HH_
#define _VFBOX_MUTE_HH_

#include "sl/sl_databox.hh"


class VfboxMute  :  public SLDatabox
{

//----------------------------- data -------------------------------//
//----------------------------- data -------------------------------//
//----------------------------- data -------------------------------//

private:

  class VfManager *_manager;
  class SLDialog  *_pickpop;   // dialog box showing active velocity function.
  int              _immediate;

//------------------------ functions ------------------------------//
//------------------------ functions ------------------------------//
//------------------------ functions ------------------------------//

public:          // constructors and destructor

  VfboxMute (SLDelay *slparent, char *name,
                              VfManager *manager, SLDialog *pickpop = NULL);
  virtual ~VfboxMute();

  class VfManager *manager      ()  const   { return _manager; }
  class SLDialog  *pickpop      ()  const   { return _pickpop; }
  int              getImmediate ()  const   { return _immediate; }
  void             setImmediate (int value) { _immediate = value; }

private:

  void makeHelper();

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
