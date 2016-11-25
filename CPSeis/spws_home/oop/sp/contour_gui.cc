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
//========================= COPYRIGHT NOTICE ================================
//====  CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.      ========
//====   PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK       ========
//======================== COPYRIGHT NOTICE =================================

//===========================================================================
//========== This gui is attached to a parent gui and controls    ===========
//========== contouring parameters for data. Note at some point   ===========
//========== the GpModelContourGui in gpplots and VA's semblance  ===========
//========== guis which were written before this should be made   ===========
//========== to use this as their base class.                     ===========
//========== Michael L. Sherrill 06/2002                          ===========
//===========================================================================

#include "sp/seis_plot.hh"
#include "sp/contour_gui.hh"
#include "sl/sl_push_box.hh"
#include "sl/sl_text_box.hh"
#include "sl/sl_tog_box.hh"
#include <Xm/Label.h>
#include <Xm/Frame.h>
#include <Xm/Separator.h>
#include <X11/cursorfont.h>
#include <math.h>
#include <stdio.h>

static String  defres[]= {
    "*values_box_Frame.topPosition:  20",
    "*values_box_Frame.leftPosition: 15",
    "*contour_tog.labelString:       Display Contours",
    "*min_contour_valL.labelString:  Min Contour Value:",
    "*max_contour_valL.labelString:  Max Contour Value:",
    "*num_contoursL.labelString:     Number Of Contours:",
    "*contour_only_tog.labelString:  Display Contours Only",
    "*min_contour_val.value:         0.0",
    "*max_contour_val.value:         100.0",
    "*num_contours.value:            20",
    NULL};





//===========================================================================
//========== Constructor                                          ===========
//===========================================================================
ContourGui::ContourGui(SLDelay           *parent,
                       char              *name,
                       SeisPlot          *sp,
                       HelpCtx           hctx)
                   : SLForm(parent,name,hctx,True,False),
                     _sp(sp), _first_time(True), _min_contour_val(1450.0F),
                     _max_contour_val(10000.0F), _num_contours(20)
{

  static SLText texts[]  = {
    {"min_contour_val",  NULL, NULL, SLType_float, MIN_CONTOUR_VAL},
    {"max_contour_val",  NULL, NULL, SLType_float, MAX_CONTOUR_VAL},
    {"num_contours","range:1 20,default:20",NULL, SLType_int,NUM_CONTOURS},
  };
  texts[0].target= &_min_contour_val;
  texts[1].target= &_max_contour_val;
  texts[2].target= &_num_contours;

  static SLTog contour_tog[] =
  {
    { "Display Contours", NULL, DO_CONTOURS },
  };

  static SLTog contour_only_tog[] =
  {
    { "Display Contours Only", NULL, CONTOURS_ONLY },
  };


  _contour_box = new SLTogBox(this, "contour_tog",getHelpCtx(),contour_tog,
                              XtNumber(contour_tog), False, False, False );
  _contour_box->setComplexNotify(this);
   
  _values_box= new SLTextBox( this, "values_box", getHelpCtx(),
                              texts, XtNumber(texts), True, 1, False, False );
  _values_box->setComplexNotify(this);

  _contour_only_box = new SLTogBox(this, "contour_only_tog",getHelpCtx(),
                                   contour_only_tog, XtNumber(contour_only_tog),
                                   False, False, False );


  _values_box->SetValue(MIN_CONTOUR_VAL, 0.0F);
  _values_box->SetValue(MAX_CONTOUR_VAL, 100.0F);
  _values_box->SetValue(NUM_CONTOURS,    20L);
  
  _plot_type = PlotImage::PlotARRAY;

}



//===========================================================================
//========== Destructor                                           ===========
//===========================================================================
ContourGui::~ContourGui()
{
}



//===========================================================================
//========== Make                                                 ===========
//===========================================================================
Widget ContourGui::make(Widget p)
{

  if ( made() ) return topWidget();
  SLForm::make(p);

  wprocShowMsg(_values_box->LabW(MIN_CONTOUR_VAL), "Min Contour Value:");
  wprocShowMsg(_values_box->LabW(MAX_CONTOUR_VAL), "Max Contour Value:");
  wprocShowMsg(_values_box->LabW(NUM_CONTOURS),    "Number Contours:");

  XtVaSetValues( _contour_box->W(),
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNleftOffset,       20,
                 XmNtopAttachment,    XmATTACH_FORM,
                 XmNtopOffset,        10, NULL);
  _contour_box->SetTog(DO_CONTOURS, False);

  XtVaSetValues( _values_box->W(),XmNleftAttachment, XmATTACH_FORM,
                 XmNleftOffset,     20,
                 XmNtopAttachment,  XmATTACH_WIDGET,
                 XmNtopWidget,      _contour_box->W(), NULL);
  XtSetSensitive(_values_box->W(),  False);

   
  XtVaSetValues( _contour_only_box->W(),
                 XmNleftAttachment,   XmATTACH_FORM,
                 XmNleftOffset,       20,
                 XmNtopAttachment,    XmATTACH_WIDGET,
                 XmNtopWidget,        _values_box->W(),
                 XmNtopOffset,        1,  NULL);
  _contour_only_box->SetTog(CONTOURS_ONLY, False);
  XtSetSensitive(_contour_only_box->W(),  False);

  return topWidget();

}



//===========================================================================
//========== Manage                                               ===========
//===========================================================================
void ContourGui::manage()
{

  if(_first_time && _sp->imageIsDisplayed())
    {
      _values_box->SetValue(MIN_CONTOUR_VAL,  _sp->minP() );
      _values_box->SetValue(MAX_CONTOUR_VAL,  _sp->maxP() );
      _first_time = False;
    }

  _first_time = False;

  SLForm::manage();

}



//===========================================================================
//========== Handle sensitivities, etc                            ===========
//===========================================================================
Boolean ContourGui::notifyComplex(SLDelay *obj, int /*ident*/)
{

  if(obj == _contour_box)
    { 
      XtSetSensitive(_values_box->W(),
                     _contour_box->IsSelected(DO_CONTOURS));
      XtSetSensitive(_contour_only_box->W(),
                     _contour_box->IsSelected(DO_CONTOURS)); 
    }

  return True;
}


//===========================================================================
//========== Public method to return if user has selected a slice ===========
//===========================================================================
Boolean ContourGui::contoursRequested()
{
  return _contour_box->IsSelected(DO_CONTOURS);
}

//===========================================================================
//========== Public method to return user's slice requested       ===========
//===========================================================================
float  ContourGui::minContourValue()
{
  return _min_contour_val;
}


//===========================================================================
//========== Public method to return user's slice requested       ===========
//===========================================================================
float  ContourGui::maxContourValue()
{
  return _max_contour_val;
}

//===========================================================================
//========== Public method to return user's slice requested       ===========
//===========================================================================
int  ContourGui::numContours()
{
  return _num_contours;
}



//===========================================================================
//========== Transfer plot parameters to the SeisPlot             ===========
//===========================================================================
void ContourGui::setParameters(SeisPlot *sp)
{
  float contour_increment;

  _sp = sp;

  if(_first_time && _sp->imageIsDisplayed())
    {
      _values_box->SetValue(MIN_CONTOUR_VAL,  _sp->minP() );
      _values_box->SetValue(MAX_CONTOUR_VAL,  _sp->maxP() );
      _first_time = False;
    }

  _first_time = False;
  
  if(_contour_box->IsSelected(DO_CONTOURS))
    {
      _sp->setContours(_num_contours);
      _sp->setMinMaxP(_min_contour_val, _max_contour_val);
      _sp->setPlotWithContours(True);
      _sp->setContoursOnly(_contour_only_box->IsSelected(CONTOURS_ONLY));
      contour_increment= (_max_contour_val-_min_contour_val)/(_num_contours-1);
      _sp->setContourIncrement(contour_increment);
    }
  else
    {
      _sp->setContours(0);
      _sp->setPlotWithContours(False);
      _sp->setContoursOnly(False);
    }

  _sp->setPlotType(_plot_type);

}

int ContourGui::plotContoursOnly()
{

  if(!_contour_box->IsSelected(DO_CONTOURS)) 
    return False;
  else
    return _contour_only_box->IsSelected(CONTOURS_ONLY);
}


void ContourGui::setContourOption(int set)
{
  _contour_box->SetTog(DO_CONTOURS, set);
}

void ContourGui::setPlotContoursOnly(int set)
{
  _contour_only_box->SetTog(CONTOURS_ONLY, set);
}


void ContourGui::setContourMinValue(float v)
{
  _values_box->SetValue(MIN_CONTOUR_VAL, v);
}

void ContourGui::setContourMaxValue(float v)
{
  _values_box->SetValue(MAX_CONTOUR_VAL, v);
}
