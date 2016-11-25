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
#ifndef _gridsave
#define _gridsave


#include "gridcvm.h"

class SLDialog;
class SLSmartForm;
class SLDelay;
class RadioList;
class SLpFile;

typedef struct _gridsave
 { char        file[96];
   int         file_type;
   int         word_type;
   int         start_slice;
   int         slices;
   SLDialog    *shell;
   SLSmartForm *work;
   RadioList   *radio1; //file format selection
   RadioList   *radio2; // word type selection
   SLpFile     *ofcw;
   gridcvm     *grid_cvm;
   void        *cvmapp;
 } gridsave;


#ifdef __cplusplus
extern "C" {                 // for C++
#endif
SLDelay *GridSaveGUI(SLDelay *slparent, gridcvm *gcvm, HelpCtx Hctx);
#ifdef __cplusplus
}                   // for C++
#endif

#endif
