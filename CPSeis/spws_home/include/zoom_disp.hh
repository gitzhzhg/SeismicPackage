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
#include "wproc.h"
#include "sl/sl_form_pop.hh"
#include "sl/sl_prim.hh"
#include "va.h"
#include "image.h"


//---------------------------------------
//----------------------------- ZoomDisp
//---------------------------------------
class ZoomDisp :  public SLFPopSep {

   private:
       static void doDirCallback(Widget, struct CB*, XmAnyCallbackStruct*);
       static void zExposeCallback(Widget, XtPointer, 
                                      XmDrawingAreaCallbackStruct*);
   protected:
       long _which_disp;
       enum {NUM_PUSH= 8};
       void doDir(Widget, struct CB*, XmAnyCallbackStruct*);
       void zExpose(Widget, XtPointer,XmDrawingAreaCallbackStruct*);
       virtual void    DoAction();
       struct ImageInput _user;
       struct PlotImage  *_orgin_image;
       VelStruct         *_vel;
       Widget            _dir_ctl;           // row colummn
       Widget            _zoom_scroll;       // scrolled window
       struct CB         _cb[NUM_PUSH];
       Widget            _dir_w[NUM_PUSH];
       Widget            _zoom_draw;         // drawing area
       struct PlotImage  _image;

   public:
//       Widget            _zoom_draw;         // drawing area
//       struct PlotImage  _image;
       ZoomDisp( Widget            p,
                 char              *name,
                 HelpCtx           hctx,
                 long              which_disp);
       long load_zoom ( struct PlotImage *_orgin_image,
                        VelStruct        *_vel,
                        long             x1,
                        long             width,
                        long             y1,
                        long             height); //other parameters
       void clean();
       virtual void manage();
       Widget zoomDraw() { return _zoom_draw; }
       struct PlotImage *zoomImage() { return &_image; }


};
