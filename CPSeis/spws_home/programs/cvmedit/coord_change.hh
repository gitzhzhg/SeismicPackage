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
#ifndef _New_Coord
#define _New_Coord
 
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include "wproc.h"
#include "oprim/data_user.hh"
#include "model.h"
#include "model_trans.h"
 
class SLDialog;
class SLSmartForm;
class SLpLabel;
class SLpOption;
class CvmApp;
class CoordData;

/* User data structure for windows */
class  CoordChange: public DataUser
{
  public:
    CoordChange(Widget parent,CvmApp *cvmapp, HelpCtx Hctx);
    ~CoordChange();
    void      Init();
    void      GuiUpdate();
    Widget    Form();
    SLDialog *Dial();

  protected:
    static void ControlTrap(void *coordch, long ident);
    void build_opt();
    void modDone(BaseData *, long);
    void dataDeleted( BaseData *bdata);
    SLpLabel  *_oldx;
    SLpLabel  *_oldy;
    SLpLabel  *_oldz;
    SLpOption *_xcop;
    SLpOption *_ycop;
    SLpOption *_zcop;
    SLpOption *_scop;

    CvmApp      *_cvmapp;
    CoordData   *_cddata;
    SLDialog    *_shell;
    SLSmartForm *_form;
    HelpCtx      _Hctx;
};
 

#endif

