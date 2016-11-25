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
#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/Label.h>
#include "sp/seis_xyout_pop.hh"
#include "sp/seis_plot.hh"
#include "sl/sl_radio_box.hh"
#include "sl/sl_text_box.hh"

enum { XDEF, XHEAD, YDEF, YHEAD, XHEAD_RANGE, YHEAD_RANGE};

static String  defres[]= {
   "_popup.title:                Customize Header Word Readout",

   "*x_which_hw*T0.labelString:  Customize X Readout-",
   "*x_which_hw*T0.alignment:    ALIGNMENT_BEGINNING", 
   "*xdef.labelString:           Do Normal X Readout",
   "*xhead.labelString:          Use Selected Header Word", 
   "*xdef.set:                   true", 

   "*y_which_hw*T0.labelString:  Change Y Readout to do Second X Header Word-",
   "*y_which_hw*T0.alignment:    ALIGNMENT_BEGINNING", 
   "*ydef.labelString:           Do Normal Y Readout",
   "*yhead.labelString:          Do X Readout with Selected Header Word",
   "*ydef.set:                   true",
  NULL};

static SLRadio xhead_rads[]  = {
         { "xdef",  XDEF },
         { "xhead", XHEAD },
       };
static SLRadio yhead_rads[]  = {
         { "ydef",  YDEF },
         { "yhead", YHEAD },
       };

SeisXYOutPop::SeisXYOutPop( Widget    p,
                            char      *name,
                            HelpCtx   hctx,
                            SeisPlot  *sp) 
              : SLFPopSep(p,name,FP_DOALL,hctx,False,False), 
                SeisInform(sp) 
{
  static SLText xtext[]  = {
    {"x_hw_choice",   "range:0 99999999,default:0",  NULL,  SLType_int,
                                                                  XHEAD_RANGE },
  };
  static SLText ytext[]  = {
    {"y_hw_choice",   "range:0 99999999,default:0",  NULL,  SLType_int,
                                                                  YHEAD_RANGE },
  };
  xtext[0].target=&_x_current_hw; 
  ytext[0].target=&_y_current_hw; 

  setDefaultResources( p, name, defres);

  _x_which_hw= new SLRadioBox( this, "x_which_hw", getHelpCtx(),
                              xhead_rads, XtNumber(xhead_rads), NULL, 
                              False, True );
  _x_hw_choice= new SLTextBox( this,"x_hw_choice",getHelpCtx(),
                             xtext,XtNumber(xtext), False );

  _y_which_hw= new SLRadioBox( this, "y_which_hw", getHelpCtx(),
                              yhead_rads, XtNumber(yhead_rads), NULL, 
                              False, True );
  _y_hw_choice= new SLTextBox( this,"y_hw_choice",getHelpCtx(),
                             ytext,XtNumber(ytext), False );
}

SeisXYOutPop::~SeisXYOutPop()
{
}


Widget SeisXYOutPop::make(Widget p)
{
   if ( made() ) return topWidget();
   SLFPopSep::make(p);
   p= wParent();

  
   XtVaSetValues( _x_which_hw->W(),
                            XmNtopAttachment,   XmATTACH_FORM,
                            XmNleftAttachment,  XmATTACH_FORM,
                            XmNleftOffset,      15,
                            XmNtopOffset,       15, 
                            NULL);

   XtVaSetValues( _x_hw_choice->W(), 
                            XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
                            XmNbottomWidget,     _x_which_hw->W(),
                            XmNleftAttachment,   XmATTACH_WIDGET,
                            XmNleftWidget,       _x_which_hw->W(),
                            NULL);
  

   Widget sep= make_attached_sep(topWidget(), "sep");
   XtVaSetValues( sep, XmNtopAttachment, XmATTACH_WIDGET,
                       XmNtopWidget,     _x_which_hw->W(),
                       XmNtopOffset,     10, NULL);

   XtVaSetValues( _y_which_hw->W(),
                            XmNtopAttachment,  XmATTACH_WIDGET,
                            XmNtopWidget,      sep,
                            XmNtopOffset,      10,
                            XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
                            XmNleftWidget,     _x_which_hw->W(),
                            NULL);

   XtVaSetValues( _y_hw_choice->W(), 
                            XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
                            XmNbottomWidget,     _y_which_hw->W(),
                            XmNleftAttachment,   XmATTACH_WIDGET,
                            XmNleftWidget,       _y_which_hw->W(),
                            NULL);
  


   Widget tmp=  XtVaCreateManagedWidget( "", xmLabelWidgetClass, topWidget(),
                                         XmNleftAttachment, XmATTACH_WIDGET,
                                         XmNleftWidget,     _x_hw_choice->W(),
                                         XmNrightAttachment, XmATTACH_FORM,
                                         XmNrightOffset,    5,
                                         XmNtopAttachment,  XmATTACH_FORM,
                                         XmNtopOffset,      5,
                                         NULL);
   Widget tmp1= XtVaCreateManagedWidget( "", xmLabelWidgetClass, topWidget(),
                                         XmNtopAttachment, XmATTACH_WIDGET,
                                         XmNtopWidget,     _y_which_hw->W(),
                                         XmNbottomAttachment, XmATTACH_WIDGET,
                                         XmNbottomWidget,    bottomSeparator(),
                                         XmNleftAttachment,  XmATTACH_FORM,
                                         XmNleftOffset,       5,
                                         XmNtopOffset,        5,
                                         XmNbottomOffset,     25,
                                         NULL);
   return topWidget();

}



Boolean SeisXYOutPop::ValidInput() { return True; }

void SeisXYOutPop::DoAction()
{
  void *x;
  SeisPlot *sp;
  for ( sp= top(&x); (sp); sp= next(&x) ) applyParams(sp);
}

void SeisXYOutPop::applyParams(SeisPlot *sp)
{
  char tmpstr[30];

  int xhw= (int)_x_hw_choice->GetInt(XHEAD_RANGE);
  if ( (_x_which_hw->WhichSelected() == XHEAD) && (xhw > 0)) {
      if(xhw <= sp->numHeaders())
        sprintf(tmpstr, "H%1d: ", xhw); 
      else
        sprintf(tmpstr, "H%1d NA",xhw);
      sp->setXReadoutHeader(xhw);
      sp->setLocationOutputXLabel(tmpstr);
  } // end if xhw
  else {
      sp->setXReadoutHeader(-1);
      sp->setLocationOutputXLabel(NULL);
  }

  int yhw= (int)_y_hw_choice->GetInt(YHEAD_RANGE);
  if ( (_y_which_hw->WhichSelected() == YHEAD) && (yhw > 0)) {
      if(yhw <= sp->numHeaders())
        sprintf(tmpstr, "H%1d: ", yhw); 
      else
        sprintf(tmpstr, "H%1d NA",yhw);
      sp->setAltYReadoutHeader(yhw);
      sp->setLocationOutputYLabel(tmpstr);
  } // end if yhw
  else {
      sp->setAltYReadoutHeader(-1);
      sp->setLocationOutputYLabel(NULL);
  }
}


void SeisXYOutPop::managing()
{
}

Boolean SeisXYOutPop::notifyComplex(SLDelay *obj, int ident)

{
   if (obj == _x_which_hw) {
       if (ident == XDEF) {
          _x_hw_choice->clear(XHEAD_RANGE);
       }
       else {
          long val= _x_hw_choice->GetInt(XHEAD_RANGE);
          if (val != 0) _x_hw_choice->SetValue(XHEAD_RANGE,val);
          else          _x_hw_choice->SetValue(XHEAD_RANGE,1L);
       }
   }
   else if (obj == _y_which_hw) {
       if (ident == YDEF) {
          _y_hw_choice->clear(YHEAD_RANGE);
       }
       else {
          long val= _y_hw_choice->GetInt(YHEAD_RANGE);
          if (val != 0) _y_hw_choice->SetValue(YHEAD_RANGE,val);
          else          _y_hw_choice->SetValue(YHEAD_RANGE,1L);
       }
   }
   else if (obj == _x_hw_choice) {
      if (_x_hw_choice->GetInt(XHEAD_RANGE) == 0) {
             _x_hw_choice->clear(XHEAD_RANGE);
             _x_which_hw->SetRadio(XDEF);
      }
      else {
             _x_which_hw->SetRadio(XHEAD);
      }
   }
   else if (obj == _y_hw_choice) {
      if (_y_hw_choice->GetInt(YHEAD_RANGE) == 0) {
             _y_hw_choice->clear(YHEAD_RANGE);
             _y_which_hw->SetRadio(YDEF);
      }
      else {
             _y_which_hw->SetRadio(YHEAD);
      }
   }
   return True;
}

void SeisXYOutPop::addControl(SeisPlot *sp)
{
   addSeisPlot(sp);
}


void SeisXYOutPop::removeControl(SeisPlot *sp)
{
   delSeisPlot(sp);
}

void SeisXYOutPop::notCurrentInWindow(SeisPlot *sp)
{
  addSeisPlot(sp->currentSPInWindow());
  applyParams(sp->currentSPInWindow());
}

void SeisXYOutPop::newSeisPlotCreatedInWindow(SeisPlot *sp)
{
  addSeisPlot(sp);
  applyParams(sp);
}
