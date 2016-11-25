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
// class that creates the color system gui for a color bar builder
#ifndef CBB_COL_SYS_GUI_HH
#define CBB_COL_SYS_GUI_HH

#include "sl/sl_form.hh"

class ColorBarBuilderPop;
class SLRadioBox;
class SLTogBox;

class CBBColSysGui : public SLForm {

public:

  enum {
    RGB,					// ident to select RGB system
    BHS,					// ident to select BHS system
    GRAY					// ident to select B/W system
  };

  CBBColSysGui					// constructor
    (Widget parent,				//   parent widget
     char *name,				//   widget name
     HelpCtx hctx,				//   context sensitive help
     ColorBarBuilderPop *cbb_pop);		//   CBB pop up

  CBBColSysGui					// constructor
    (SLDelay *container,			//   container
     char *name,				//   widget name
     HelpCtx hctx,				//   context sensitive help
     ColorBarBuilderPop *cbb_pop);		//   CBB pop up

  ~CBBColSysGui ();				// destructor

  virtual Widget make				// make function
    (Widget parent = 0);			//   parent widget

  virtual Boolean notifyComplex			// action notify function
    (SLDelay *obj,				//   SL object acted upon
     int ident);				//   and which element therein

  void selectRGB ();				// select RGB type

  void selectBHS ();				// select BHS type

  void selectGRAY				// select Gray type
    (Boolean set=True);				//   True for set toggle

  int RGBSelected ();				// return 0 if RGB not selectd

  int BHSSelected ();				// return 0 if BHS not selectd

  int GRAYSelected ();				// return 0 if GRAY not selctd

private:
  void init ();					// constructor helper

  ColorBarBuilderPop
    *_cbb_pop;					// color bar builder popup

  SLRadioBox
    *_colsys_box;				// color system radio box

  SLTogBox
    *_gray_box;					// gray toggle box

  Boolean
    _gray_selected;				// flag

};

#endif
