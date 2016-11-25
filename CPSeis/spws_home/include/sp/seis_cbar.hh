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
#ifndef SEISCBAR_H
#define SEISCBAR_H

#include "wproc.h"
#include "sl/sl_delay.hh"
#include "sp/seis_color.hh"
#include <Xm/Frame.h>


//--------------------------------------
//----------------------------- SeisCbar
//--------------------------------------
class SeisCbar : public SLDelay, public SeisColor {
       private:
          static void CExposeCallback( Widget, XtPointer, 
                                       XmDrawingAreaCallbackStruct* );

       protected:
          void CExpose( Widget, XtPointer, XmDrawingAreaCallbackStruct*);
          Widget               _da;             // Drawing area widget

       public:
          SeisCbar( Widget            p,
                    char              *name,
                    SeisPlot          *sp,
                    Boolean           separate_colors =True,
                    Boolean           always_update_from_sp= False,
                    Boolean           make_now        =False,
                    int               which_cols = 0);

          SeisCbar( SLDelay           *contain,
                    char              *name,
                    SeisPlot          *sp,
                    Boolean           separate_colors =True,
                    Boolean           always_update_from_sp= False,
                    Boolean           make_if_can     =True,
                    int               which_cols = 0);
          virtual ~SeisCbar() {}; 

          virtual  int  readColorFile(char *fname);
          virtual  void setPredef(int predef);
          int      getPredef();
          long    availableColors() {return _col.cnum; };
          virtual Widget make(Widget p);
          virtual WidgetClass topClass() { return(xmFrameWidgetClass); };
          void    redraw (float cell_width_factor = 1.0);
          void    setColormap (Colormap colormap);
          virtual void colorInfoChangedImmediately (ColorInfo *col);
                                                               // ColorInfoSet
};
#endif
