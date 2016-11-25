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
//author Michael L. Sherrill
//class that controls dragging an overlay image
#ifndef SEISDRAG_H
#define SEISDRAG_H


#include "sp/seis_plot.hh"

class SeisPlot;

class SeisDrag {

  private:
        int                    _dest_x;
        int                    _dest_y;
                                  
  protected:     
        Boolean                _doing_drag;
        Boolean                _separate_window;
        struct PlotImage       *_ip;
        SeisPlot               *_oversp;
        SeisPlot               *_undersp;
        SeisPlot               *_tiesp;
        static XtTranslations  _pointer_trans;
        XtTranslations         _original_trans;
        char                   *_old_sp_mousehelp;
        char                   *_old_tie_mousehelp;
        void DoDrag( Widget w, XEvent *event, char *mode[]);
        static void DragTrans( Widget w, XEvent *event, char *mode[]);

  public:
        SeisDrag( SeisPlot *oversp, SeisPlot *undersp); 
        void setTiePlot(SeisPlot *sp= NULL);
        virtual ~SeisDrag();
        void startDrag();                      //installs drag
        virtual void dragComplete(SeisPlot *sp);
};

#endif




