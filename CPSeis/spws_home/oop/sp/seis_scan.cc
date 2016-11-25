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
#include "sp/seis_scan.hh"
#include "sp/seis_plot.hh"
#include <Xm/PushB.h>
#include <Xm/ArrowB.h>
#include <Xm/Label.h>
#include <stdio.h>


static String  defres[]= {
        "*rarrowstr.labelString:    Scan\\nRight",
        "*larrowstr.labelString:    Scan\\nLeft",
        "*XmLabel.fontList:         -*-*-bold-r-*--14-*-*-*-*-*-*-1",
        "*XmArrowButton.width:      40",
        "*XmArrowButton.height:     40",
        "*XmArrowButton.shadowThickness:   0",
        "*larrow.arrowDirection:    ARROW_LEFT",
        "*rarrow.arrowDirection:    ARROW_RIGHT",
        "*rarrowstr.leftOffset:     8",
        "*XmLabel.marginHeight:     1",
        "*XmLabel.marginWidth:      0",
      NULL };


SeisScan::SeisScan(Widget   p,
                   char     *name,
                   SeisPlot *sp,
                   HelpCtx  hctx) :

                    SLForm(p, name, hctx, False), SeisInform(sp)
{

   Widget l0, l1;

   setDefaultResources( XtDisplay(p), name, defres);

   _rarrow= XtVaCreateManagedWidget( "rarrow", xmArrowButtonWidgetClass, 
                                     topWidget(), 
                                     XmNuserData, Right,
                                     NULL);

   _larrow= XtVaCreateManagedWidget( "larrow", xmArrowButtonWidgetClass, 
                                     topWidget(),
                                     XmNuserData, Left,
                                     XmNbottomAttachment, XmATTACH_FORM,
                                     NULL );

   XtAddCallback(_rarrow, XmNactivateCallback, 
                 (XtCallbackProc)arrowCallback, (XtPointer)this);
   XtAddCallback(_larrow, XmNactivateCallback, 
                 (XtCallbackProc)arrowCallback, (XtPointer)this);



   l0= XtVaCreateManagedWidget( "larrowstr", xmLabelWidgetClass, topWidget(),
                                 XmNbottomAttachment, XmATTACH_FORM,
                                 XmNleftAttachment,   XmATTACH_WIDGET,
                                 XmNleftWidget,       _larrow,
                                 NULL );

   l1= XtVaCreateManagedWidget( "rarrowstr", xmLabelWidgetClass, topWidget(),
                                 XmNbottomAttachment, XmATTACH_FORM,
                                 XmNleftAttachment,   XmATTACH_WIDGET,
                                 XmNleftWidget,       l0,
                                 NULL );

   XtVaSetValues( _rarrow,       XmNbottomAttachment, XmATTACH_FORM,
                                 XmNleftAttachment,   XmATTACH_WIDGET,
                                 XmNleftWidget,       l1,  
                                 NULL );


}


SeisScan::~SeisScan()
{
}

void SeisScan::addControl(SeisPlot *sp)
{
   addSeisPlot(sp);
}

void SeisScan::removeControl(SeisPlot *sp)
{
   delSeisPlot(sp);
}




void SeisScan::arrowCallback(Widget w,
                             XtPointer udata,
                             XmArrowButtonCallbackStruct* CBdata)
{
  SeisScan *obj = (SeisScan *)udata;
  obj->arrow(w, udata, CBdata);
}

void SeisScan::arrow(Widget w,
                     XtPointer,
                     XmArrowButtonCallbackStruct* )
{
 long wconst;
 SeisPlot::ScanDir dir;
 SeisPlot *sp;

 XtVaGetValues(w, XmNuserData, &wconst, NULL);
 

 switch (wconst) {
      case Left :
              dir= SeisPlot::Left;
              break;
      case Right :
              dir= SeisPlot::Right;
              break;
 }
 for(sp= top(); (sp); sp= next() ) {
       if (!sp->isMultiPlane()) {
             if (sp->isCurrentInWindow()) {
                   sp->scan(dir);
                  if (sp->getChainedSP())
                        sp->getChainedSP()->scan(dir);
             }
       }
 }
 //for(sp= top(); (sp); sp= next() ) {
 //      if (sp->isMultiPlane()) {
 //            if (sp->getChainedSP() && 
 //                sp->getChainedSP()->isCurrentInWindow()) 
 //                  sp->scan(dir);
 //      }
 // }


}

void SeisScan::newSeisPlotCreatedInWindow(SeisPlot *newsp)
{
  addControl(newsp);
}
