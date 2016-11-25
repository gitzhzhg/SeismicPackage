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
#ifndef APP_EXEC_HH
#define APP_EXEC_HH

#include <Xm/Xm.h>
#include "sl/sl_form.hh"

class SLPullPop;
class AppInputs;
class AppOutputs;

class AppExec : public SLForm {

public:
  enum
    {INPUTS,					// identifies inputs pushed
     OUTPUTS,					// identifies outputs pushed
     QUIT};					// identifies quit pushed

  AppExec					// constructor
    (Widget parent,				//   Widget parent
     char *name,				//   name given
     HelpCtx hctx,				//   context sensitive help
     SLPullPop *menu,				//   App menu bar
     AppInputs *inputs,				//   App inputs
     AppOutputs *outputs);			//   App outputs

  virtual ~AppExec ();				// destructor

  Boolean notifyComplex				// notify complex callback
    (SLDelay *sender,				//   message sender
     int ident);				//   sender identification

private:
  AppInputs   *_inputs;		       		// inputs popup
  AppOutputs  *_outputs;		       	// outputs popup

  SLPullPop   *_menu;				// App menu bar

};

#endif
