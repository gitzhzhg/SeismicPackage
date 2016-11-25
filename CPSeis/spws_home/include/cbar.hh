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
#ifndef CBAR_H
#define CBAR_H

#include "wproc.h"
#include "sl/sl_prim.hh"
#include "sl/sl_form_pop.hh"
#include "sl/sl_delay.hh"
#include "sl/psuedo_widget.hh"
#include "file_choice.h"
#include "image.h"
#include <Xm/Frame.h>
//#include "vel_data.h"


static const int CbarSucc     = 1;
static const int CbarTooMany  = 2;
static const int CbarInvalid  = 3;

//--------------------------------------
//----------------------------- Cbar
//--------------------------------------
class Cbar : public SLDelay {
       private:
          static void CExposeCallback( Widget, XtPointer, 
                                XmDrawingAreaCallbackStruct* );

       protected:
          void CExpose( Widget, XtPointer, 
                                XmDrawingAreaCallbackStruct*);
          Widget               _da;             // Drawing area widget
          ColorInfoPtr         _col_request;
          ColorInfoPtr         _col_plot;
          ColorInfoPtr         _col_gs;
          struct PlotImage     *_image;
          long                 _curr_color;
          long                 _numcolors;
          float                _rgb[1024];
          long                 _first_disp;
          virtual ~Cbar() {}; 

       public:
          Cbar( Widget            p,
                char              *name,
                struct PlotImage  *image,
                ColorInfoPtr      col_request,
                ColorInfoPtr      col_plot,
                ColorInfoPtr      col_gs,
                Boolean           make_now =False);

          Cbar( PsuedoWidget      *pw,
                char              *name,
                struct PlotImage  *image,
                ColorInfoPtr      col_request,
                ColorInfoPtr      col_plot,
                ColorInfoPtr      col_gs);

          void    load_color(long predef);
          long    load_file(char *filename);
          void    load_image();
          void    update_cbar();
          long    availableColors() {return _col_request->cnum; };
          virtual Widget make(Widget p);
          virtual WidgetClass topClass() { return(xmFrameWidgetClass); };


};



/*
//--------------------------------------
//----------------------------- CbarPop
//--------------------------------------
class CbarPop :  public Cbar, public SLFPopSep{
       protected:

       public:
           CbarPop( Widget            p,
                    char              *name,
                    struct PlotImage  *image,
                    ColorInfoPtr      col_plot,
                    ColorInfoPtr      col_gs);
           ~CbarPop() {};
       virtual void manage();
};
*/

#endif
