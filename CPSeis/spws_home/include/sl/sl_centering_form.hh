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
// sl_centering_form.hh

#ifndef SL_CENTERING_FORM_HH
#define SL_CENTERING_FORM_HH

#include "sl/sl_delay.hh"
#include <Xm/Xm.h>
#include <Xm/BulletinB.h>

class SLSmartForm;

class SLCenteringForm : public SLDelay {

public:
  SLCenteringForm				// constructor
    (Widget parent,				//   Widget parent
     char *name,				//   widget name
     HelpCtx hctx,				//   context sensitive help
     Boolean make_now = True);			//   make it now flag

  SLCenteringForm				// constructor
    (SLDelay *contain,				//   SL Delay container
     char *name,				//   widget name
     HelpCtx hctx,				//   context sensitive help
     Boolean make_now = True);			//   make it now flag

  virtual Widget make				// make function
    (Widget parent = NULL);			//   given parent widget

  virtual WidgetClass topClass ()		// return the top widget class
    { return(xmBulletinBoardWidgetClass); }

  virtual Boolean isContainer ()		// this is a container class
    { return True; }

  void showEvenSpacing ();			// pass thru to SLSmartForm

  void attach					// pass thru to SLSmartForm
    (SLDelay *gui,				//   GUI to attach
     SLDelay *left,				//   attached to left of GUI
     SLDelay *right  = 0,			//   attached to right of GUI
     SLDelay *top    = 0,			//   attached to top of GUI
     SLDelay *bottom = 0,			//   attached to bottom of GUI
     int oleft       = 0,			//   Offset from left of GUI
     int oright      = 0,			//   Offset from right of GUI
     int otop        = 0,			//   Offset from top of GUI
     int obottom     = 0);			//   Offset from bottom of GUI

protected:
  static void mapCallback			// call back fcn when mapped
    (Widget widget,				//   widget
     XtPointer OBJdata,				//   pointer to object
     XEvent *event);				//   pointer to the event

  static void structCallback			// call back fcn on struct mod
    (Widget widget,				//   widget
     XtPointer OBJdata,				//   pointer to object
     XEvent *event);				//   pointer to the event

  void centerForm ();				// center the given form

  SLSmartForm
    *_sl_form;					// given SLForm object

};

#endif
