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
// y = f(x)
#ifndef GUI_CURVE_FITTER_HH
#define GUI_CURVE_FITTER_HH

#include "sl/sl_form.hh"
#include "curves/curve_constants.hh"

class WeightedCurveFitter;
class GuiCurveRanges;
class CurveParameters;
class SLScaleTextArrow;

class GuiCurveFitter : public SLForm
{

public:
  virtual ~GuiCurveFitter ();			// destructor

  virtual Widget make				// make function
    (Widget parent = NULL);			//   given parent widget

  virtual void manage();

  virtual int dataChangedFit ();		// new coef.s from fitter

  virtual int userChangedFit ();		// new coef.s from user

  virtual void clear ();			// clear errors

  virtual int setFitter				// set the fitter
    (WeightedCurveFitter *fitter,		//   weighted curve fitter obj
     Boolean y_is_independent = False);		//   False if y is depend var

  void fitterDeleted ();			// must call when fittr deletd

  WeightedCurveFitter *fitter ();		// return the fitter

  void setXString				// set the X-string
    (const char *x_str);			//   given character string

  void setYString				// set the Y-string
    (const char *y_str);			//   given character string

  char *getIndependentString			// return the indepndnt string
    (int type);					//   gvn indepndnt-varble type

  char *getDependentString			// return the dependnt-string
    (int type);					//   given depndnt-varble type

  virtual char *getCString			// return the coefficient str
    (int type);					//   given coefficient type

  void rangePopup ();				// popup the gui to set ranges

  GuiCurveRanges *rangeGui ();			// rtrn the gui to set ranges

  virtual int setFitterParameters ();		// set the fitter parameters

  CurveParameters *curveParameters ()		// rtrn curve parameters obj
    { return _curve_parameters; }

  int bellIsOn ()				// return 0 if bell is off
    { return _bell_is_on; }

  void setBellIsOnFlag				// set the bell is on flag
    (int bell_is_on = 1)			//   given the desired setting
    { _bell_is_on = bell_is_on; }

  Boolean coefficientsAreChanging ()		// rtn True if coef are chngng
    { return _coefficients_changing; }

  const char *getTitle() {return _title;}

  void deleteRangeGui();

  virtual void informCoefficientsChanged () {}	// inform that coefs chnged

protected:
  GuiCurveFitter				// constructor
    (Widget parent,				//   Widget parent
     char *name,				//   widget name
     HelpCtx hctx,				//   context sensitive help
     const char *title,				//   gui curve fit title
     int type);					//   gui curve fit type

  GuiCurveFitter				// constructor
    (SLDelay *container,			//   SL Delay container
     char *name,				//   widget name
     HelpCtx hctx,				//   context sensitive help
     const char *title,				//   gui curve fit title
     int type);					//   gui curve fit type

  void init ();					// constructor helper

  virtual Widget coordinateDefinitionLine ();	// rtn the coord defn line wdg

  virtual void display ();			// display errors

  void displayStatus ();			// display fitter status

  int verifyRangeGui ();			// ensure range gui exists

  virtual Boolean notifyComplex			// action notify function
    (SLDelay *obj,				//   SL object acted upon
     int ident);				//   and which element therein

  virtual Boolean otherRecognizedObject		// rtn True if object is known
    (SLDelay *obj);				//   SL object acted upon

  virtual int setOtherGuiValues ();		// extend dataChangedFit

  virtual int setOtherFitterValues ();		// extend userChangedFit

  char *getVString				// return the variable-string
    (int type,					//   given variable type
     char *str);				//   given variable string

  virtual int getErrorStatistics ();		// get current error stats

  virtual int displayErrorStatistics ()		// rtrns 0 if errors not dispd
    { return 1; }

  WeightedCurveFitter
    *_fitter;					// weighted curve fitter objct

  GuiCurveRanges
    *_gui_ranges;				// gui to set curve ranges

  CurveParameters
    *_curve_parameters;				// curve params object

  SLScaleTextArrow
    *_x0_arrow,					// x0 offset i/f
    *_y0_arrow;					// y0 offset i/f

  Widget
    _equa0,					// label widget for fitter nam
    _titleW,					// label widget for title text
    _errtxtW,					// label widget for err text
    _erraveW,					// label widget for err ave
    _errstdW,					// label widget for err std
    _statusW,					// label widget for status
    _erravetxt,					// text widget for err ave vlu
    _errstdtxt,					// text widget for err std vlu
    _statustxt,					// text widget for status str
    _top_attach_widget,				// top attachment widget
    *_dependtxt,				// text widget for depend var
    *_indeptxt;					// text widget for indep var

  Boolean
    _text_set,					// False if text not displayed
    _been_mapped,				// False if gui not mapped yet
    _coefficients_changing,			// False if coefs not chnging
    _y_is_independent;				// False if y is depend var

  const char
    *_title;					// gui curve fit title

  char
    *_x_str,					// string assoc with X var
    *_y_str;					// string assoc with Y var
  
  float
    _x0,					// x0 offset
    _y0,					// y0 offset
    _errave,					// average error
    _errstd;					// std dev of error

  int
    _type,					// gui curve fit type
    _bell_is_on,				// flag = 0 if bell is off
    _status;					// status value

private:
  char *fittername ();				// return fitter name

};

#endif
