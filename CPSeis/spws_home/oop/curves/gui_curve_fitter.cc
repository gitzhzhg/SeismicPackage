#include "curves/gui_curve_fitter.hh"
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
#include "curves/weighted_curve_fitter.hh"
#include "curves/gui_curve_ranges.hh"
#include "curves/curve_parameters.hh"
#include "sl/slp_text.hh"
#include "sl/slp_push.hh"
#include "sl/psuedo_widget.hh"
#include "sl/sl_scale_text_arrow.hh"
#include "cprim.h"
#include "wproc.h"
#include <math.h>
#include <float.h>
#include <Xm/Label.h>

static void range_trap (void *data, long /*ident*/)
{
  GuiCurveFitter *gui = (GuiCurveFitter *)data;
  gui->rangePopup ();
}

static String defres[] = {
  "*errave.labelString:      ;  Err:  ave = ",
  "*errstd.labelString:      ,  std = ",
  "*status.labelString:      ;            Status:  ",
  "*rangepb.labelString:     Set Ranges",
  "*indep1.labelString:      x",
  "*depend1.labelString:     y",
  "*indep2.labelString:      x'",
  "*depend2.labelString:     y'",
  "*Yequals.labelString:      = ",
  "*Yminus.labelString:       - ",
  "*semiColon.labelString:   ;  ",
  "*Xequals.labelString:      = ",
  "*Xminus.labelString:       - ",
  0};

GuiCurveFitter::GuiCurveFitter (Widget parent, char *name,
  HelpCtx hctx, const char *title, int type) :
  SLForm (parent, name, hctx, False, False),
  _title                  (title),
  _type                   (type),
  _text_set               (False),
  _coefficients_changing  (False),
  _bell_is_on             (1)
{
  init ();
  setDefaultResources (parent, name, defres);
}

GuiCurveFitter::GuiCurveFitter (SLDelay *container, char *name,
  HelpCtx hctx, const char *title, int type) :
  SLForm (container, name, hctx, False, False),
  _title                  (title),
  _type                   (type),
  _text_set               (False),
  _coefficients_changing  (False),
  _bell_is_on             (1)
{
  init ();
  if (container->made()) {
    setDefaultResources (container->topWidget(), name, defres);
  }
  else {
    setDefaultResources (container->pW()->display(), name, defres);
  }
}

GuiCurveFitter::~GuiCurveFitter ()
{
  if (_x_str           ) free   (_x_str)         , _x_str            = 0;
  if (_y_str           ) free   (_y_str)         , _y_str            = 0;
  if (_curve_parameters) delete _curve_parameters, _curve_parameters = 0;
  if (_indeptxt        ) delete [] _indeptxt     , _indeptxt         = 0;
  if (_dependtxt       ) delete [] _dependtxt    , _dependtxt        = 0;

  //Next added by MLS 03/2000 to delete the range gui that was created
  //in the verifyRangeGui method. 
  if(_gui_ranges        ) delete _gui_ranges;
}

Widget GuiCurveFitter::make (Widget parent)
{
  if (made()) return topWidget ();

  parent = parent ? parent : wParent ();

  SLForm::make ();

   //If table type we are done
  if(_type == CV::TABLE)
    {
    return topWidget();
    }

  _titleW = XtVaCreateManagedWidget (_title,
                         xmLabelWidgetClass,  topWidget(),
                         XmNborderWidth,      0,
                         NULL);
  if (displayErrorStatistics()) {
    _erraveW = XtVaCreateManagedWidget ("errave",
                         xmLabelWidgetClass,  topWidget(),
                         XmNborderWidth,      0,
                         NULL);

    _errstdW = XtVaCreateManagedWidget ("errstd",
                         xmLabelWidgetClass,  topWidget(),
                         XmNborderWidth,      0,
                         NULL);

    SLpText *erravetxt = new SLpText (this, "erravetxt", 0, PrimSupport::_FLOAT,
      13, 6);
    erravetxt->showLabelAppearance ();
    _erravetxt = erravetxt->W ();

    SLpText *errstdtxt = new SLpText (this, "errstdtxt", 0, PrimSupport::_FLOAT,
      13, 6);
    errstdtxt->showLabelAppearance ();
    _errstdtxt = errstdtxt->W ();
  }




 



  SLpPush *rangepb = new SLpPush (this, "rangepb");
  rangepb->setAtrap (range_trap, this);

  _statusW = XtVaCreateManagedWidget ("status",
                         xmLabelWidgetClass,   topWidget(),
                         XmNborderWidth,       0,
                         NULL);

  SLpText *statustxt = new SLpText (this, "statustxt", 0, SLpText::_CHAR,
    52);
  statustxt->showLabelAppearance ();
  _statustxt = statustxt->W ();

  _indeptxt[0] = XtVaCreateManagedWidget ("indep1",
                         xmLabelWidgetClass,  topWidget(),
                         XmNborderWidth,      0,
                         NULL);

  _dependtxt[0] = XtVaCreateManagedWidget ("depend1",
                         xmLabelWidgetClass,  topWidget(),
                         XmNborderWidth,      0,
                         NULL);
  int k2;
  for (k2 = 1; k2 < 3; k2++) {

    _dependtxt[k2] = XtVaCreateManagedWidget ("depend2",
                         xmLabelWidgetClass,  topWidget(),
                         XmNborderWidth,      0,
                         NULL);

    _indeptxt[k2] = XtVaCreateManagedWidget ("indep2",
                         xmLabelWidgetClass,  topWidget(),
                         XmNborderWidth,      0,
                         NULL);
  }

// attach the push button, title, and status line
  XtVaSetValues (rangepb->W(),
                         XmNtopAttachment,    XmATTACH_FORM,
                         XmNleftAttachment,   XmATTACH_FORM,
                         NULL);

  XtVaSetValues (_titleW,
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        rangepb->W(),
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       rangepb->W(),
                         XmNleftOffset,       5,
                         NULL);

  XtVaSetValues (_statusW,
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _titleW,
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       _titleW,
                         NULL);

  XtVaSetValues (_statustxt,
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _titleW,
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       _statusW,
                         NULL);

// clear the displayed values
  clear ();

// attach the prime coordinate definition line
  _top_attach_widget = coordinateDefinitionLine ();

  return topWidget ();
}


void GuiCurveFitter::manage()
{
  SLForm::manage();

  if(_type == CV::TABLE)
    XtUnmanageChild(_titleW);
}


void GuiCurveFitter::init ()
{
  _been_mapped       = False;
  _titleW            = 0;
  _errtxtW           = 0;
  _erraveW           = 0;
  _errstdW           = 0;
  _statusW           = 0;
  _erravetxt         = 0;
  _errstdtxt         = 0;
  _statustxt         = 0;
  _indeptxt          = 0;
  _dependtxt         = 0;
  _fitter            = 0;
  _gui_ranges        = 0;
  _y_is_independent  = False;
  _x_str             = 0;
  _y_str             = 0;
  _curve_parameters  = 0;
  _x0_arrow          = 0;
  _y0_arrow          = 0;
  _x0                = 0;
  _y0                = 0;


  //Table type
  if(_type == CV::TABLE) 
    {
    _curve_parameters = new CurveParameters ();
    return;
    }

  _x_str = newstr ("x");
  _y_str = newstr ("y");

  _indeptxt  = new Widget[3];
  _dependtxt = new Widget[3];
  for (int k2 = 0; k2 < 3; k2++) _indeptxt[k2] = 0, _dependtxt[k2] = 0;

  _x0_arrow = new SLScaleTextArrow (this, "x0_arrow", getHelpCtx(),
    &_x0, True, _x0, (float)-1, (float)1, (float).01, (int)7, (int)10,
    XmHORIZONTAL, 0, (Dimension)60);

  _x0_arrow->setComplexNotify (this);

  _y0_arrow = new SLScaleTextArrow (this, "y0_arrow", getHelpCtx(),
    &_y0, True, _y0, (float)-1, (float)1, (float).01, (int)7, (int)10,
    XmHORIZONTAL, 0, (Dimension)60);

  _y0_arrow->setComplexNotify (this);

  _curve_parameters = new CurveParameters ();
}

Widget GuiCurveFitter::coordinateDefinitionLine ()
{
  if(_type == CV::TABLE) return NULL;

  Widget curveName, Yequals, Yminus, semiColon, Xequals, Xminus;

  char *str = new char[strlen(CV::getCurveName(_type))+3];
  strcpy (str, CV::getCurveName(_type));
  strcat (str, ": ");

  XmString label_string = XmStringCreateSimple (str);
  curveName = XtVaCreateManagedWidget ("curveName",
                         xmLabelWidgetClass,  topWidget(),
                         XmNborderWidth,      0,
			 XmNlabelString,      label_string,
                         NULL);
  XmStringFree (label_string);
  delete [] str;

  Yequals = XtVaCreateManagedWidget ("Yequals",
                         xmLabelWidgetClass,  topWidget(),
                         XmNborderWidth,      0,
                         NULL);

  Yminus = XtVaCreateManagedWidget ("Yminus",
                         xmLabelWidgetClass,  topWidget(),
                         XmNborderWidth,      0,
                         NULL);

  semiColon = XtVaCreateManagedWidget ("semiColon",
                         xmLabelWidgetClass,  topWidget(),
                         XmNborderWidth,      0,
                         NULL);

  Xequals = XtVaCreateManagedWidget ("Xequals",
                         xmLabelWidgetClass,  topWidget(),
                         XmNborderWidth,      0,
                         NULL);

  Xminus = XtVaCreateManagedWidget ("Xminus",
                         xmLabelWidgetClass,  topWidget(),
                         XmNborderWidth,      0,
                         NULL);

// attach managed widgets as desired
  XtVaSetValues (curveName,
                         XmNtopAttachment,    XmATTACH_WIDGET,
                         XmNtopWidget,        _titleW,
                         XmNtopOffset,        5,
                         XmNleftAttachment,   XmATTACH_FORM,
                         NULL);

  XtVaSetValues (_dependtxt[1],
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        curveName,
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       curveName,
                         NULL);

  XtVaSetValues (Yequals,
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _dependtxt[1],
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       _dependtxt[1],
                         NULL);

  XtVaSetValues (_dependtxt[0],
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _dependtxt[1],
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       Yequals,
                         NULL);

  XtVaSetValues (Yminus,
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _dependtxt[1],
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       _dependtxt[0],
                         NULL);

  XtVaSetValues (_y0_arrow->W(),
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _dependtxt[1],
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       Yminus,
                         NULL);

  XtVaSetValues (semiColon,
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _dependtxt[1],
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       _y0_arrow->W(),
                         NULL);

  XtVaSetValues (_indeptxt[1],
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _dependtxt[1],
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       semiColon,
                         NULL);

  XtVaSetValues (Xequals,
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _dependtxt[1],
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       _indeptxt[1],
                         NULL);

  XtVaSetValues (_indeptxt[0],
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _dependtxt[1],
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       Xequals,
                         NULL);

  XtVaSetValues (Xminus,
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _dependtxt[1],
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       _indeptxt[0],
                         NULL);

  XtVaSetValues (_x0_arrow->W(),
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _dependtxt[1],
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       Xminus,
                         NULL);

  return _y0_arrow->W();
}

void GuiCurveFitter::display ()
{
  if (!_fitter) return;

  getErrorStatistics ();

  if(_type == CV::TABLE) return;

  double tiny = FLT_EPSILON * FLT_EPSILON;
  if (fabs((double)_errave) < tiny) _errave = 0;
  if (             _errstd  < tiny) _errstd = 0;
  if (_erravetxt) wprocVAShowMsg (_erravetxt, "%13.6g", _errave);
  if (_errstdtxt) wprocVAShowMsg (_errstdtxt, "%13.6g", _errstd);

  if (!_text_set) {
    char x_str[3], y_str[3];
    int k2;
    if (_y_is_independent) {
      if (_dependtxt[0]) wprocVAShowMsg (_dependtxt[0], "%.1s", _x_str);
      if (_indeptxt [0]) wprocVAShowMsg (_indeptxt [0], "%.1s", _y_str);

      strcpy (x_str, _x_str);
      strcat (x_str, "'"   );
      strcpy (y_str, _y_str);
      strcat (y_str, "'"   );
      for (k2 = 1; k2 < 3; k2++) {
        if (_dependtxt[k2]) wprocVAShowMsg (_dependtxt[k2], "%.2s",  x_str);
        if (_indeptxt [k2]) wprocVAShowMsg (_indeptxt [k2], "%.2s",  y_str);
      }
    }
    else {
      if (_indeptxt[0] ) wprocVAShowMsg (_indeptxt [0], "%.1s", _x_str);
      if (_dependtxt[0]) wprocVAShowMsg (_dependtxt[0], "%.1s", _y_str);

      strcpy (x_str, _x_str);
      strcat (x_str, "'");
      strcpy (y_str, _y_str);
      strcat (y_str, "'");
      for (k2 = 1; k2 < 3; k2++) {
        if (_indeptxt [k2]) wprocVAShowMsg (_indeptxt [k2], "%.2s", x_str);
        if (_dependtxt[k2]) wprocVAShowMsg (_dependtxt[k2], "%.2s", y_str);
      }
    }
    _text_set = _dependtxt[0] && _dependtxt[1] && _dependtxt[2] &&
                _indeptxt [0] && _indeptxt [1] && _indeptxt [2]   ;
  }

  displayStatus ();
}

void GuiCurveFitter::displayStatus ()
{
  if (!_fitter) return;

   if(_type == CV::TABLE) return; 

  _status = _fitter->errorStatus ();

  if (_statustxt) wprocVAShowMsg (_statustxt,
    (char *)CV::statusMessage(_status));
  if (_status != CV::NORMAL && _bell_is_on) {
    XBell (XtDisplay(topWidget()), 50);
  }
}

void GuiCurveFitter::clear ()
{
  if(_type == CV::TABLE) return;

  if (_erravetxt) wprocVAShowMsg (_erravetxt, " ");
  if (_errstdtxt) wprocVAShowMsg (_errstdtxt, " ");
  if (_statustxt) wprocVAShowMsg (_statustxt, " ");
}

int GuiCurveFitter::setFitter (WeightedCurveFitter *fitter,
  Boolean y_is_independent)
{
  assert (fitter);

  _fitter = fitter;
  getErrorStatistics ();
  _y_is_independent = y_is_independent;

  assert (verifyRangeGui());

  int err = _curve_parameters->setFitter ((CurveFitter *)_fitter);
  if (err != CV::NORMAL) return err;

  err = setFitterParameters ();
  if (err != CV::NORMAL) return err;

  display ();

  return err;
}

void GuiCurveFitter::fitterDeleted ()
{
  _fitter = 0;
}

WeightedCurveFitter *GuiCurveFitter::fitter ()
{
  return _fitter;
}

void GuiCurveFitter::deleteRangeGui()
{
  if(_gui_ranges)
    {
    delete _gui_ranges;
    _gui_ranges = 0;
    }
}

void GuiCurveFitter::setXString (const char *x_str)
{
  if(_type == CV::TABLE) return;

  if (_x_str) free (_x_str), _x_str = 0;
  _x_str = newstr (x_str);
}

void GuiCurveFitter::setYString (const char *y_str)
{
  if(_type == CV::TABLE) return;

  if (_y_str) free (_y_str), _y_str = 0;
  _y_str = newstr (y_str);
}

char *GuiCurveFitter::getIndependentString (int type)
{
  if(_type == CV::TABLE) return "";

  char *retval;
  if (_y_is_independent) {
    retval = getVString (type, _y_str);
  }
  else {
    retval = getVString (type, _x_str);
  }
  return retval;
}

char *GuiCurveFitter::getDependentString (int type)
{
  if(_type == CV::TABLE) return "";

  char *retval;
  if (_y_is_independent) {
    retval = getVString (type, _x_str);
  }
  else {
    retval = getVString (type, _y_str);
  }
  return retval;
}

char *GuiCurveFitter::getCString (int type)
{
  if(_type == CV::TABLE) return "";

  assert (type >= CV::A0 &&
    type <= CV::maximumCoefficientIndex(fitter()->type()));

  char *retval = new char[3];
  if (type == CV::A0) {
    strcpy (retval, "c0");
  }
  else if (type == CV::A1) {
    strcpy (retval, "c1");
  }
  else if (type == CV::A2) {
    strcpy (retval, "c2");
  }
  return retval;
}

void GuiCurveFitter::rangePopup ()
{
  if(_type == CV::TABLE) return;

  assert (verifyRangeGui());

  if (!_gui_ranges->made()) {
    char *name = fittername ();
    char *title = (char *)malloc
      (strlen(name)+strlen("Coefficient Ranges")+1);
    strcpy (title, (const char *)name);
    free (name);
    strcat (title, "Coefficient Ranges");
    _gui_ranges->make (this->W());
    _gui_ranges->setTitle (title);
    free (title);
  }
  _gui_ranges->manage ();
}

GuiCurveRanges *GuiCurveFitter::rangeGui ()
{
  if(_type != CV::TABLE) return (GuiCurveRanges *)0;
  return _gui_ranges;
}

int GuiCurveFitter::setFitterParameters ()
{
  int err;
  float min, max, inc;
  double d0;

  err = _fitter->coefficient (CV::X0, &d0);
  if (err != CV::NORMAL) {
    displayStatus ();
    return err;
  }

  min = (float)_curve_parameters->coefficientMinimum        (CV::X0);
  max = (float)_curve_parameters->coefficientMaximum        (CV::X0);
  inc = (float)_curve_parameters->coefficientRangeIncrement (CV::X0);

// fix the offset range until the user sets it directly
  _curve_parameters->setCoefficientRange (CV::X0, min, max);

  if (_x0_arrow) {
    _x0_arrow->setRange     (min, max);
    _x0_arrow->setIncrement (inc);
    _x0_arrow->setValue     ((float)d0);
  }
 
  err = _fitter->coefficient (CV::Y0, &d0);
  if (err != CV::NORMAL) {
    displayStatus ();
    return err;
  }

  min = (float)_curve_parameters->coefficientMinimum        (CV::Y0);
  max = (float)_curve_parameters->coefficientMaximum        (CV::Y0);
  inc = (float)_curve_parameters->coefficientRangeIncrement (CV::Y0);

// fix the offset range until the user sets it directly
  _curve_parameters->setCoefficientRange (CV::Y0, min, max);

  if (_y0_arrow) {
    _y0_arrow->setRange     (min, max);
    _y0_arrow->setIncrement (inc);
    _y0_arrow->setValue     ((float)d0);
  }

  return CV::NORMAL;
}

int GuiCurveFitter::verifyRangeGui ()
{
  int retval;
  if (!_gui_ranges) {
    _gui_ranges = new GuiCurveRanges (this, "gui_curve_ranges",
    getHelpCtx(), this);
    retval = 1;
  }
  else {
    retval = (CurveFitter *)_fitter == _gui_ranges->fitter ();
  }
  return retval;
}

char *GuiCurveFitter::fittername ()
{
/* // no longer getting the fitter type string from the widget
// get the fitter type compound string from the widget
  XmString cmp_string;
  XtVaGetValues (_curveName,
                      XmNlabelString,      &cmp_string,
                      NULL);

// determine the string context of the compound string
  char *retval;
  XmStringContext context;
  if (!XmStringInitContext(&context,cmp_string)) {
    XtWarning ("Can't convert Fitter-Name compound string");
    XmStringFree (cmp_string);
    retval = (char *)malloc (strlen(_title)+1);
    strcpy (retval, _title);
    return retval;  // remember to free retval in calling routine!!!!
  }
  XmStringFree (cmp_string);

// convert the fitter type compound string to a "Xt" char string
  XmStringCharSet   charset;
  XmStringDirection direction;
  Boolean           separator;
  char *text, buf[128];

  char *pbuf = buf;
  while (XmStringGetNextSegment(context,&text,&charset,&direction,
    &separator)) {
    pbuf += (strlen(strcpy(pbuf,text))); // let the ptr run thru buf
    if (separator == True) {
      *pbuf++ = '\n'; // add new line
      *pbuf   = 0;    // NULL-terminate
    }
    XtFree (text);    // free that text segment
  }
  XmStringFreeContext (context);

// concatenate the given title with the fitter type
  pbuf = buf;
  retval = (char *)malloc (strlen(_title)+strlen((const char *)buf)+2);
  strcpy (retval, _title);
  strcat (retval, " ");
  strcat (retval, (const char *)pbuf);
  return retval;  // remember to free retval in calling routine!!!!
*/ // no longer getting fitter name out of Widget

// return combined title and fitter type

// do arithmetic with pointers to get the length of the string you want
  size_t leng = strlen (CV::getCurveName(_type));
  assert (leng > 0);

  char *retval = (char *)malloc (strlen(_title)+leng+3);
  strcpy (retval, _title);
  strcat (retval, " ");
  strcat (retval, CV::getCurveName(_type));
  strcat (retval, " ");
  return retval; // remember to free retval in calling routine!!!!
}

Boolean GuiCurveFitter::notifyComplex (SLDelay *obj, int ident)
{
  Boolean retval = True;

  if (obj == _x0_arrow || obj == _y0_arrow ||
    otherRecognizedObject (obj)) {
    if (ident == SLScaleTextArrow::VALUE_INPUT   ||
        ident == SLScaleTextArrow::VALUE_CHANGED   ) {
// motion is complete
      _coefficients_changing = False;
    }
    else if (ident == SLScaleTextArrow::ARROW_PRESSED ||
             ident == SLScaleTextArrow::SLIDER_DRUG     ) {
// motion is in progress
      _coefficients_changing = True;
    }
// motion is complete
// the user changed one of the coefficients
    if (userChangedFit() != CV::NORMAL) retval = False;
  }
  return retval;
}

int GuiCurveFitter::dataChangedFit ()
{
  if (!_fitter) return CV::NORMAL;

// get new coefficients from the fitter and update the coefficients on the
//   gui
  double x0, y0, tiny;
  int err;

  tiny    = FLT_EPSILON * FLT_EPSILON;

  err  = _fitter->coefficient (CV::X0, &x0);
  if (err == CV::NORMAL) {
    if (fabs(x0) < tiny) {
// x0 was found to be trivial
      x0 = 0;
    }
    if (_x0_arrow) _x0_arrow->setValue (x0);
  }
  else {
    displayStatus ();
    return err;
  }

  err  = _fitter->coefficient (CV::Y0, &y0);
  if (err == CV::NORMAL) {
    if (fabs(y0) < tiny) {
// y0 was found to be trivial
      y0 = 0;
    }
    if (_y0_arrow) _y0_arrow->setValue (y0);
  }
  else {
    displayStatus ();
    return err;
  }

  err = setOtherGuiValues ();
  if (err != CV::NORMAL) {
    displayStatus ();
    return err;
  }

// update the error stats on the gui
  display ();
  return err;
}

int GuiCurveFitter::userChangedFit ()
{
  if (!_fitter) return CV::NORMAL;

  int err;

// use the coefficients on the gui and set the coefficients on the fitter
  err = _fitter->setCoefficient (CV::X0, (double)_x0);
  if (err != CV::NORMAL) {
    displayStatus ();
    return err;
  }

  err = _fitter->setCoefficient (CV::Y0, (double)_y0);
  if (err != CV::NORMAL) {
    displayStatus ();
    return err;
  }

  err = setOtherFitterValues ();
  if (err != CV::NORMAL) {
    displayStatus ();
    return err;
  }

// redo the internal coefficients and the error statistics
  err = _fitter->redoCoefficients ();
  if (err != CV::NORMAL) {
    displayStatus ();
    return err;
  }

// inform the outside world that the coefficients changed
  informCoefficientsChanged ();

// update the error stats on the gui
  display ();
  return err;
}

Boolean GuiCurveFitter::otherRecognizedObject (SLDelay * /*obj*/)
{
  return False;
}

int GuiCurveFitter::setOtherGuiValues ()
{
  return CV::NORMAL;
}

int GuiCurveFitter::setOtherFitterValues ()
{
  return CV::NORMAL;
}

char *GuiCurveFitter::getVString (int type, char *str)
{

  if(_type == CV::TABLE) return "";

  assert (str);

  char *retval;
  if (type == CV::ORIGINAL_VARIABLE) {
    retval = new char[strlen(str)+1];
    strcpy (retval, str);
  }
  else if (type == CV::OFFSET_VARIABLE) {
    retval = new char[strlen(str)+2];
    strcpy (retval, str);
    strcat (retval, "0");
  }
  else if (type == CV::TRANSLATED_VARIABLE) {
    retval = new char[strlen(str)+2];
    strcpy (retval, str);
    strcat (retval, "'");
  }
  else {
    assert (0);
  }
  return retval;
}

int GuiCurveFitter::getErrorStatistics ()
{
  if (displayErrorStatistics()) {
    _errave = (float)_fitter->errorAverage ();
    _errstd = (float)_fitter->errorStandardDeviation ();
    _status =        _fitter->errorStatus ();
  }
  return CV::NORMAL;
}
