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
#ifndef SEISCONTROL_H
#define SEISCONTROL_H


#include "sl/sl_form.hh"
#include "sl/sl_push_box.hh"
#include "sp/seis_inform.hh"

class SeisPlot;
class SeisLocOut;
class SeisScan;
class SeisMovie;
class SeisZoomer;
class ControlInform;
class SeisControlPushBox;

class SeisControl : public SLForm, public SeisInform {

   friend class SeisControlPushBox;
   private:
     static void pushCallback(Widget, XtPointer, XmPushButtonCallbackStruct*);
     SLPushButtonfunc _altPushAction;
     void             *_altPushData;

   protected:

       void push(Widget, XtPointer, XmPushButtonCallbackStruct*);
       SeisLocOut *_loc;
       SeisScan   *_scan;
       SeisMovie  *_movie;
       SLDelay    *_alternate;
       ControlInform *_inform;
       SeisControlPushBox *_push_box;
       Widget     _rc;
       Widget     _zoom_abort;
       SeisZoomer *_zoomer;
   public:
       SeisControl( Widget   p,
                    char     *name    ="control",
                    SeisPlot *sp      =NULL,
                    HelpCtx  hctx     =NULL,
                    Boolean  doscan   =True,
                    Boolean  domovie  =True);
       virtual ~SeisControl();
       void addControl(SeisPlot *sp);
       void addXYControlOnly(SeisPlot *sp);
       void removeControl(SeisPlot *sp);

       void addPush(char *name, int ident);
       void delPush(int ident);
       Boolean pushExist(int ident);
       Widget getPush(int ident);
       void ifTieShowTime(Boolean doit =True);
       void setAlternate(SLDelay *alt);
       void setAltPushAction( SLPushButtonfunc action =NULL, void *data =NULL)
                                   { _altPushAction= action;
                                     _altPushData  = data; };

       virtual void newPlot(SeisPlot *sp);
       virtual void noPlotDisplayed(SeisPlot *sp);
       virtual void preZoom(SeisPlot *sp, SeisZoomer *zoomer,
                            SeisPlot::ZoomDir dir);
       virtual void zoomInProgress(SeisPlot *sp, SeisPlot::ZoomDir dir);
       virtual void postZoom(SeisPlot *sp, SeisPlot::ZoomDir dir);
       virtual void newSeisPlotCreatedInWindow(SeisPlot *);
       virtual void notCurrentInWindow(SeisPlot *);
       virtual Widget getPushButton(int which);
};
#endif
