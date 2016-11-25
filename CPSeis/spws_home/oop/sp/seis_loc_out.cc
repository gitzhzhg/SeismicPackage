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
#include "sp/seis_loc_out.hh"
#include "sp/seis_plot.hh"
#include "sp/seis_plot_tie.hh"
#include <stdio.h>
#include <Xm/TextF.h>


static String  defres[]= {
           "*XmTextField.editable:     False",
           "*XmTextField.traversalOn:  False",
           "*XmTextField.cursorPositionVisible : False",
           "*vout.rightOffset:    5",
           "*vout.bottomOffset:   5",
           "*vout.columns:        30",
           "*xloc.columns:        14",
           "*yloc.columns:        14",
           "*XmTextField.marginHeight: 1",
           "*XmTextField.marginWidth:  1",
      NULL };


SeisLocOut::SeisLocOut(Widget   p,
                       char     *name,
                       SeisPlot *sp,
                       HelpCtx  hctx) :

               SLForm(p, name, hctx, True), _tie_displayed(False),
               SeisInform(sp) 
{
   setDefaultResources( XtDisplay(p), name, defres);

/*
 * Set all resources in XtVaCreateManagedWidget, sun gives runtime
 * warnings when using XtVaSetValues and a TextField's traversalOn is True.
 * ehs   07Nov94
 */
   _aout= XtVaCreateManagedWidget("aout", xmTextFieldWidgetClass, topWidget(),
                         XmNbottomAttachment, XmATTACH_FORM,
                         XmNrightAttachment,  XmATTACH_FORM,
                         XmNleftAttachment,   XmATTACH_FORM,
                         XmNtopAttachment,    XmATTACH_NONE,
                         NULL);

   _xloc= XtVaCreateManagedWidget("xloc", xmTextFieldWidgetClass, topWidget(),
                         XmNbottomAttachment, XmATTACH_WIDGET,
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNtopAttachment,    XmATTACH_NONE,
                         XmNrightAttachment,  XmATTACH_NONE,
                         XmNleftWidget,       _aout,
                         XmNbottomWidget,     _aout, NULL);

   _yloc= XtVaCreateManagedWidget("yloc", xmTextFieldWidgetClass, topWidget(),
                         XmNbottomAttachment, XmATTACH_WIDGET,
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNtopAttachment,    XmATTACH_NONE,
                         XmNrightAttachment,  XmATTACH_NONE,
                         XmNleftWidget,       _xloc,
                         XmNbottomWidget,     _aout, NULL);


   if (sp) {
      sp->setLocationOutput(_xloc, _yloc, _aout);
   }

   ifTieShowTime();
}


SeisLocOut::~SeisLocOut()
{
 SeisPlot *sp;
 void *x;

 for(sp=top(&x); (sp); sp= top(&x) ) {
      removeControl(sp);
 }

}



void SeisLocOut::addControl(SeisPlot *sp)
{
   addSeisPlot(sp);
   sp->setLocationOutput(_xloc, _yloc, _aout);
}

void SeisLocOut::removeControl(SeisPlot *sp)
{
 delSeisPlot(sp);
 sp->removeLocationOutput();
}


void SeisLocOut::ifTieShowTime(Boolean disable_amp)
{
 SeisPlot *sp;
 void *x;

 if (_tie_displayed) {
    if (disable_amp) 
         for(sp= top(&x); (sp); sp= next(&x) )
                 sp->setLocationOutput(_xloc, _yloc, NULL);
    else 
         for(sp= top(&x); (sp); sp= next(&x) )
                 sp->setLocationOutput(_xloc, _yloc, _aout);
 } // End if
 _disable_amp= disable_amp;
}




void SeisLocOut::postZoomSeparateWindow(SeisPlot *, SeisPlot *zoomsp)
{
  addControl(zoomsp);  
}



void SeisLocOut::addingTie(SeisPlotTie *sp, SeisPlot * )
{
 sp->addOutputWidget(_aout);
 _tie_displayed= True;
 ifTieShowTime(_disable_amp);
}


void SeisLocOut::removingTie(SeisPlotTie *sp, SeisPlot *)
{
 sp->delOutputWidget(_aout);
 _tie_displayed= False;

 for(SeisPlot *q= top(); (q); q= next() )
          q->setLocationOutput(_xloc, _yloc, _aout);
}

Widget SeisLocOut::getXloc() { return _xloc; }
Widget SeisLocOut::getYloc() { return _yloc; }
Widget SeisLocOut::getAout() { return _aout; }

void SeisLocOut::notCurrentInWindow(SeisPlot *sp)
{
  void *x;
  SeisPlot *newsp= sp->currentSPInWindow();
  if (!find(newsp, &x)) {
     addControl(newsp);
  }

}
