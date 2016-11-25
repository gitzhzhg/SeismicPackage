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
#ifndef _mlimits_
#define _mlimits_
 
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include "wproc.h"
#include "mlimits.h"
#include "model_trans.h"
 
class SLDialog;
class SLSmartForm;
class SL2Text;
class CvmApp;

/* User data structure for windows */
class Mlimits
{
  public:
   Mlimits(Widget parent,CvmApp *cvmapp, HelpCtx Hctx);
   ~Mlimits();
   void   mlimitsTrap();
   void   mlimitsInit();
   void   mlimitsGuiUpdate(ModLimits *);
   void   mlimitsGuiUpdate();
   Widget Form();
   SLDialog *Dial();

  protected:
   static void ControlTrap(void *data, long ident);

   float _xmin ;
   float _xmax ;
   float _ymin ;
   float _ymax ;
   float _zmin ;
   float _zmax ;
   SL2Text *_x1,*_x2;
   SL2Text *_y1,*_y2;
   SL2Text *_z1,*_z2;

   CvmApp      *_cvmapp;
   SLDialog    *_shell;
   SLSmartForm *_form;
   HelpCtx      _Hctx;
};
 

#endif

