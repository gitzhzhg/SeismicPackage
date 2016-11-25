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
#ifndef SEIS_SCRWIN_HH
#define SEIS_SCRWIN_HH

#include "sl/sl_scroll_win.hh"


class SeisPlot;
class ViewWin;


class SeisScrWin : public SLScrollWin {


  private:
   SeisPlot *_sp;
  public:
   SeisScrWin(const Widget p, const char *name, SeisPlot *sp);
   virtual void annotate( Widget       anno_da,
                          int          x,
                          int          y,
                          unsigned int width,
                          unsigned int height,
                          long         window_x,
                          long         window_y,
                          int          which_side );
   virtual void visibleAreaChange(int          x,
                                  int          y,
                                  unsigned int width,
                                  unsigned int height);
   virtual void startingDrag();
   virtual void endingDrag();
   void setCurrentSP(SeisPlot *sp);

   void setBordersAndScrollBars( ViewWin   **view_win_array,
                                 SeisPlot  **seisplot_array,
                                 long      number_of_elements,
                                 char      *need_to_replot_array,
                                 long      narrow_border = 5,
                                 long      wide_border   = 91);


  protected:

};

#endif
