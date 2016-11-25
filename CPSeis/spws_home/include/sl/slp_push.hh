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


//------------------------- slp_push.hh -------------------------------//
//------------------------- slp_push.hh -------------------------------//
//------------------------- slp_push.hh -------------------------------//

//               header file for the SLpPush class
//                 derived from the SLpBase class
//                      subdirectory sl

#ifndef _SLP_PUSH_HH_
#define _SLP_PUSH_HH_

#include "sl/slp_base.hh"


class SLShellContainer;

class SLpPush : public SLpBase
{

//---------------------- data -------------------------//
//---------------------- data -------------------------//
//---------------------- data -------------------------//

private:

  enum { _NORMAL_ACTION, _SHOW_HELP, _MANAGE_SHELL, _UNMANAGE_SHELL };

  long              _special_trap_action;   // one of above enums
  SLShellContainer *_slpop;
  char             *_helpname;

//--------------------- functions -------------------------//
//--------------------- functions -------------------------//
//--------------------- functions -------------------------//

public:

  SLpPush (SLDelay *slparent, char *name, long ident = 0, char *label = NULL);
  SLpPush (Widget    wparent, char *name, long ident = 0, char *label = NULL);
  SLpPush (Widget    w,                   long ident = 0, char *label = NULL);

  virtual ~SLpPush (void);
  virtual Widget make (Widget p = NULL);   // overrides SLDelay

  void normalActionWhenPressed (void)
         { _special_trap_action = _NORMAL_ACTION; }
  void showHelpWhenPressed (char *helpname = NULL)
         { _special_trap_action = _SHOW_HELP;
           _helpname = string_alloc(_helpname, helpname, 0); }
  void manageShellWhenPressed (SLShellContainer *slpop)
         { _special_trap_action = _MANAGE_SHELL; _slpop = slpop; }
  void unmanageShellWhenPressed (SLShellContainer *slpop)
         { _special_trap_action = _UNMANAGE_SHELL; _slpop = slpop; }

private:

  virtual Boolean notify (SLPrim *gui);   // overrides SLDelay
  virtual void setCvarResource  (void);   // overrides PrimSupport

//--------------------- end of functions ------------------------//
//--------------------- end of functions ------------------------//
//--------------------- end of functions ------------------------//

} ;

#endif

//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
