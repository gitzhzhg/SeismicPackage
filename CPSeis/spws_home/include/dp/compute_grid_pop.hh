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
#ifndef COMPUTE_GRID_POP_HH
#define COMPUTE_GRID_POP_HH

#include "sl/sl_form_pop.hh"
#include "plot/pick_base.hh"
#include "dp/grid_error_codes.hh"

class ComputeGrid;
class SLPushBox;
class SLTogBox;
class SeisPlot;
class SLScaleTextArrow;

class ComputeGridPop : public SLFPopSep {

public:
  ComputeGridPop				// constructor
    (Widget p,					//   parent widget
     char *name,				//   name of popup
     ComputeGrid *cg,				//   compute grid object
     HelpCtx hctx);				//   context sensitive help

  virtual ~ComputeGridPop ();			// destructor

  virtual Widget make				// make the popup
    (Widget p);					//   parent widget

  virtual void manage ();			// manage the popup

  virtual int selectGrid			// select grid function (pure)
    (SeisPlot *sp) = 0;				//   seismic plot object

  virtual void installPicking () = 0;		// install pickers (pure)

  virtual void removePicking () = 0;		// remove pickers (pure)

  void setDisplay ();				// redisplays the popup

  int failed ();				// return 1 if unsuccessful

  virtual Boolean notifyComplex			// return 1 if unsuccessful
    (SLDelay *obj,				//   object causing the call
     int ident);				//   given identifier

  GridErrorCodes errorStatus ()			// returns error status flag
    { return _error_status; }

protected:
  static void selectInputs			// call back on selecting inpts
    (void *data,				//   popup object
     long button);				//   which button pressed

  void compressionButtonPushed ();		// compress button action

  static void computeResult			// call back on compute result
    (void *data,				//   popup object
     long button);				//   which button pressed

  static void rescaleResult			// call back on rescale result
    (void *data,				//   popup object
     long button);				//   which button pressed

  virtual char *gridname			// return grid name (pure)
    (long which_variable) = 0;			//   which input variable

  void setWidgetActive				// activates a button widget
    (Widget w);					//   which button widget

  void setWidgetInActive			// deactivates a button widget
    (Widget w);					//   which button widget

  virtual Boolean compressIsOK () = 0;		// return True if OK to compres

  ComputeGrid
    *_cg;					// compute grid object

  SLScaleTextArrow
    *_proportion_result;			// proportion scale bar

  long
    _select_inputs_button,			// which inputs button on
    _compute_results_button;			// which results button on

  SLPushBox
    *_select_inputs_box,			// inputs button box
    *_compute_results_box;			// results button box

  SLTogBox
    *_rescale_result;				// rescale result toggle

  Pixel
    _foreground_no_hilite_color,		// fore button color w/o hilite
    _background_no_hilite_color,		// back button color w/o hilite
    _topshadow_no_hilite_color,			// top shadow of button color
    _bottomshadow_no_hilite_color,		// bottom shadow of button colr
    _foreground_hilite_color,			// fore button color w/ hilite
    _background_hilite_color;			// back button color w/ hilite

  Widget
    _Agridlab,					// widget label of A selection
    _Bgridlab,					// widget label of B selection
    _messagelab;				// widget label of message

private:
  GridErrorCodes
    _error_status;				// error status flag

};

class ComputeGridPicker : public PickBase {

private:
  ComputeGridPop
    *_cgp;					// compute grid pop object

  SeisPlot
    *_sp;					// seismic plot object

public:
  ComputeGridPicker				// constructor
    (SeisPlot *sp,				//   seismic plot object
     ComputeGridPop *cgp);			//   compute grid pop object

  virtual ~ComputeGridPicker ();		// destructor

protected:
  virtual void buttonAny			// called on any button pushed
    (int x1,					//   first X-coordinate
     int x2,					//   second X-coordinate
     int y1,					//   first Y-coordinate
     int y2,					//   second Y-coordinate
     int button,				//   which mouse button effectd
     Action action,				//   what happened to button
     Modifier modifier);			//   what keyboard modifier
};

#endif
