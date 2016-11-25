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
#include "sp/seis_movie.hh"
#include "sp/seis_plot.hh"
#include <Xm/PushB.h>
#include <Xm/ArrowB.h>
#include <stdio.h>


static String  defres[]= {
  //     "*XmScale.titleString: Movie Control",
          "*XmScale.scaleMultiple: 1",
      NULL };



SeisMovie::SeisMovie(Widget   p,
                     char     *name,
                     SeisPlot *sp,
                     HelpCtx  hctx) :
              SLArrowScale(p, name, hctx, NULL, True), SeisInform(sp),
              _prev_value(getValue())
{
   setDefaultResources( XtDisplay(p), name, defres);

   //show_msg( scaleW(), "Movie Control" );


}

SeisMovie::~SeisMovie()
{
}

void SeisMovie::ValueChangeAction( long value)
{
 SeisPlot *sp;
 SeisPlot::MovieDir mdir=SeisPlot::AnyChange;
 long tot_frames;

 if (arrowAction()) {
   mdir= (value<_prev_value) ? SeisPlot::StepBackward : SeisPlot::StepForward;
 }
   //                     && (!sp->overlayOnTop())

 for(sp= top(); (sp); sp= next() ) {
     tot_frames= sp->plottedFrames();
     if ( tot_frames > 1       && 
          value > 0            && 
          value <= tot_frames  && 
          sp->isCurrentInWindow()  ){
                 sp->movieToFrame((int)(value-1),mdir);
     }
 } // end loop
 _prev_value= (int)value;

}

void SeisMovie::addControl(SeisPlot *sp)
{
   addSeisPlot(sp);
   sp->movieToFrame(getValue()-1);
}

void SeisMovie::removeControl(SeisPlot *sp) 
{
 delSeisPlot(sp);
}

void SeisMovie::newPlot(SeisPlot *sp)
{
  Boolean is_movie= False;
  int heigh=0;
  SeisPlot *q;
 
 for(q= top(); (q); q= next()) {
     if ( q->movie() && q->isCurrentInWindow() ){
         is_movie= True;
         if (heigh < q->plottedFrames()) heigh= (int)q->plottedFrames();
     }
 }
 if (is_movie) {
         setRange(1, heigh);
         if (sp->currentFrame() == 0) {
                long value= getValue();
                if ((sp->movie() ) && (value > 0)  && 
                    (value <= sp->plottedFrames()) && 
                    (!sp->overlayOnTop()) ){
                        sp->movieToFrame((int)(value-1),SeisPlot::StepForward);
                }
         }
         else 
                setValue( (int)sp->currentFrame() + 1);
                ValueChangeAction( sp->currentFrame() + 1);
         manage();
 }
 else {
         setRange(1, 2);
         unmanage();
 }
}


void SeisMovie::noPlotDisplayed(SeisPlot *)
{
  SeisPlot *q;
  void *x;
  Boolean no_movie= True;

  for(q= top(&x); ((q)&&(no_movie)); q= next(&x)) {
       if (no_movie) 
            no_movie= (!q->isPlotDisplayed() || !q->isCurrentInWindow()
                                             || !q->movie());
  }
  if (no_movie) {
      setRange(1, 2);
      unmanage();
  }
}

void SeisMovie::newSeisPlotCreatedInWindow(SeisPlot *newsp)
{
  addControl(newsp);
}

void SeisMovie::notCurrentInWindow(SeisPlot *oldsp)
{
  SeisPlot *sp= oldsp->currentSPInWindow();
  if ( sp->movie() ) {
         setRange(1, (int)sp->plottedFrames() );
         setValue( (int)sp->currentFrame() + 1);
         ValueChangeAction( sp->currentFrame() + 1);
         manage();
 }
 else {
         setRange(1, 2);
         unmanage();
 }
  //newPlot(sp);
}

void SeisMovie::postMovie(SeisPlot *sp, SeisPlot::MovieDir )
{
  setValue( (int)sp->currentFrame() + 1);
}

long SeisMovie::getNumFrames()
{
  return _high;
}
