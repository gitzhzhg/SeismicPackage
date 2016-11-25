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
//author Michael L. Sherrill 01/94
//creates menu to control grid image parameters
#include <Xm/Label.h>
#include <Xm/Frame.h>
#include <Xm/Separator.h>
#include <stdio.h>
#include "sp/seis_grid_pop.hh"
#include "sp/seis_plot.hh"
#include "sl/sl_push_box.hh"
#include <X11/cursorfont.h>
#include <math.h>


#define    PWIDTH_VAL           4.0F
#define    PHEIGHT_VAL          4.0F
#define    PLEFT_VAL            0.0F
#define    PRIGHT_VAL         100.0F
#define    PTOP_VAL             0.0F
#define    PBOTTOM_VAL        100.0F


static String  defres[]= {
    "*popup.title:                   Grid Menu",
    ".height:                        400",
    ".width:                         375",
    "*header_box_Frame.topPosition:  20",
    "*header_box_Frame.leftPosition: 15",
    "*gridlab.fontList:              -*-*-bold-r-*-*-*-180-*-*-p-*-*-*",
    "*gridlab.labelString:           Grid Coordinates",
    "*pwidthL.labelString:           Width/Inches:",
    "*pheightL.labelString:          Height/Inches:",
    "*xminL.labelString:             Left:",
    "*xmaxL.labelString:             Right:",
    "*yminL.labelString:             Top:",
    "*ymaxL.labelString:             Bottom:",
    "*enforce.labelString:           Enforce Symetrical Size",
    "*pwidth.value:                  4.0",
    "*pheight.value:                 4.0",
    "*xmin.value:                    0.0",
    "*xmax.value:                    100.0",
    "*ymin.value:                    0.0",
    "*ymax.value:                    1000.0",
    NULL};




#define ParentClass SLFPopSep


SeisGridPop::SeisGridPop( Widget            p,
                          char              *name,
                          SeisPlot          *sp,
                          HelpCtx           hctx) 
       : SLFPopSep(p,name,FP_DOALL,hctx,True,False),   _plot_on_doaciton(True),
                            _use_file_defaults(False), _new_appdefaults(True),
                            _sp(sp), _first_time(True)

{

  static SLText texts[]  = {
    {"pwidth",  NULL, NULL, SLType_float, PWIDTH},
    {"pheight", NULL, NULL, SLType_float, PHEIGHT},
    {"xmin",    NULL, NULL, SLType_float, PLEFT},
    {"xmax",    NULL, NULL, SLType_float, PRIGHT},
    {"ymin",    NULL, NULL, SLType_float, PTOP},
    {"ymax",    NULL, NULL, SLType_float, PBOTTOM},
  };
  texts[0].target= &_pwidth;
  texts[1].target= &_pheight;
  texts[2].target= &_left;
  texts[3].target= &_right;
  texts[4].target= &_top;
  texts[5].target= &_bottom;

  static SLTog enforce[]  = 
   {
     { "enforce", NULL, ENFORCE },
   };     

   setDefaultResources( p, name, defres);

   
   _header_box= new SLTextBox( this, "header_box", getHelpCtx(),
                               texts, XtNumber(texts), True, 1, True, False );
   _header_box->setAltFocusAction ( (SLTextfunc)CoordFocusAction, this );
   _header_box->setAltLosingAction( (SLTextfunc)CoordLosingFocusAction, this );
   
   _enforcebox = new SLTogBox(this, "enforce_box",getHelpCtx(),enforce,
                            XtNumber(enforce), True, False, False );
   _enforcebox->setAltChoiceAction( 
                            (SLToggleButtonfunc)EnforceAction, this);

   _plot_type = PlotImage::PlotGRID;
}


SeisGridPop::~SeisGridPop()
{
}

void SeisGridPop::CoordFocusAction( void *data, long which )
{
SeisGridPop *obj = (SeisGridPop *)data;

  if(which == PHEIGHT)return;
  obj->_focus_id = which;
  obj->_focus_value = obj->_header_box->GetFloat((int)which);
}


void SeisGridPop::CoordLosingFocusAction( void *data, long which )
{
SeisGridPop *obj = (SeisGridPop *)data;

  if(which == PHEIGHT)return;
  if(which != obj->_focus_id)return;
  if(obj->_focus_value == obj->_header_box->GetFloat((int)which)) return;

  if(obj->_enforcebox->IsSelected(ENFORCE))
    {
    obj->_header_box->SetValue(PHEIGHT,  
               obj->_sp->getSymetricalSize(False, obj->_left, obj->_right,
                                                  obj->_top, obj->_bottom,
                                                  &obj->_pwidth));
    obj->_header_box->SetValue(PWIDTH,obj->_pwidth);
    }
}

Widget SeisGridPop::make(Widget p)
{

   if ( made() ) return topWidget();
   SLFPopSep::make(p);


   XtVaSetValues( _header_box->W(), XmNleftAttachment, XmATTACH_POSITION,
                                    XmNtopAttachment,  XmATTACH_POSITION, NULL);


   _enforcebox->SetTog(ENFORCE,True);

   XtVaSetValues( _enforcebox->W(),XmNtopAttachment,  XmATTACH_WIDGET,
                                   XmNtopWidget,      _header_box->W(),
                                   XmNtopOffset,      0,
                                   XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET, 
                                   XmNleftWidget,     _header_box->W(), 
                                   XmNleftOffset,     0, 
                                   XmNrightAttachment,XmATTACH_OPPOSITE_WIDGET, 
                                   XmNrightWidget,    _header_box->W(), 
                                   XmNrightOffset,    0, NULL);
   
   Widget gridlab = XtVaCreateManagedWidget("gridlab",xmLabelWidgetClass, 
                                            topWidget(), 
                              XmNleftAttachment,   XmATTACH_POSITION,
                              XmNleftPosition,     22,
                              XmNtopAttachment,    XmATTACH_POSITION,
                              XmNtopPosition,      7,  NULL);


   return topWidget();

}


//============================================================================
//====================== Enforce symetrical size     =========================
//============================================================================
void SeisGridPop::EnforceAction( void *data, long /*which*/ )
{
SeisGridPop *obj = (SeisGridPop *)data;

  if(obj->_enforcebox->IsSelected(ENFORCE))
    {
    obj->_header_box->SetValue(PHEIGHT,  
               obj->_sp->getSymetricalSize(False, obj->_left, obj->_right,
                                                  obj->_top, obj->_bottom,
                                                  &obj->_pwidth));
    obj->_header_box->SetValue(PWIDTH,obj->_pwidth);
    }

}

Boolean SeisGridPop::ValidInput()
{
 Boolean stat;


 if (made()) 
    stat= ParentClass::ValidInput();
 else 
    stat= True;

 return (stat); 
}


void SeisGridPop::UndoInput()
{
  SLFormPop::UndoInput();
}


void SeisGridPop::setPlotType(int type)
{
  _plot_type = type;
}


void SeisGridPop::DoAction()
{
  ParentClass::DoAction();
  _sp->setPlotType(_plot_type);
  _sp->setGridX1(_left);
  _sp->setGridX2(_right);
  _sp->setGridY1(_top);
  _sp->setGridY2(_bottom);
  _sp->setGridWidth(_pwidth);
  _sp->setGridHeight(_pheight);
  _sp->plot();
}




void SeisGridPop::manage()
{
  if(_first_time && _sp->imageIsDisplayed())
    {
     _header_box->SetValue(PWIDTH,  _sp->gridWidth() );
     _header_box->SetValue(PHEIGHT, _sp->gridHeight() );
     _header_box->SetValue(PLEFT,    _sp->gridX1() );
     _header_box->SetValue(PRIGHT,    _sp->gridX2() );
     _header_box->SetValue(PTOP,    _sp->gridY1() );
     _header_box->SetValue(PBOTTOM,    _sp->gridY2() );
     _first_time = False;
    }

  SLFPopSep::manage();

}

void SeisGridPop::reloadDefaults(Boolean)
{
  SLFPopSep::reloadDefaults();

  _header_box->reloadDefaults();
  DoAction();
}


void SeisGridPop::reloadSystemDefaults(Boolean do_method)
{
  SLFPopSep::reloadSystemDefaults(do_method);
  _header_box->SetValue(PWIDTH,  PWIDTH_VAL);
  _header_box->SetValue(PHEIGHT, PHEIGHT_VAL);
  _header_box->SetValue(PLEFT,   PLEFT_VAL);
  _header_box->SetValue(PRIGHT,  PRIGHT_VAL);
  _header_box->SetValue(PTOP,    PTOP_VAL);
  _header_box->SetValue(PBOTTOM, PBOTTOM_VAL);
  DoAction();
}
