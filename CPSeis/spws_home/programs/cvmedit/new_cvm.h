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
#ifndef _New_Cvm
#define _New_Cvm

#include "wproc.h"

class SLDelay;
class SLDialog;
class SLSmartForm;
class SL2Text;
class CvmApp;

typedef struct _new_cvm
 { 
   SLDialog    *shell;
   SLSmartForm *work;
   SL2Text     *xtext;
   SL2Text     *ytext;
   SL2Text     *ztext;
   SL2Text     *mtext;
   char        xname[20];
   char        yname[20];
   char        zname[20];
   char        mname[80];
   CvmApp      *cvmapp;
 } new_cvm;


#ifdef __cplusplus
extern "C" {                 // for C++
#endif
SLDialog *NewGUI(Widget parent, CvmApp *data, HelpCtx Hctx);
#ifdef __cplusplus
}                   // for C++
#endif


#endif
