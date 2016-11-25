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
#include <stdlib.h>
#include <Xm/DrawingA.h>
#include <Xm/Frame.h>
#include "sp/seis_cbar.hh"
#include "sl/color_info_set.hh"

SeisCbar::SeisCbar( Widget    p,
                    char      *name,
                    SeisPlot  *sp,
                    Boolean   separate_colors,
                    Boolean   always_update_from_sp,
                    Boolean   make_now,
                    int       which_cols) :
         SLDelay(p,name,NULL), 
         SeisColor(sp,separate_colors,always_update_from_sp, which_cols),
         _da(NULL)
{
 if (make_now) make(p);
}

SeisCbar::SeisCbar( SLDelay           *contain,
                    char              *name,
                    SeisPlot          *sp,
                    Boolean           separate_colors,
                    Boolean           always_update_from_sp,
                    Boolean           make_if_can,
                    int               which_cols) :

         SLDelay(contain,name,NULL), 
         SeisColor(sp,separate_colors,always_update_from_sp, which_cols), 
         _da(NULL)
{ 
 if ((contain->made())&&(make_if_can)) make(contain->W());
}


Widget SeisCbar::make(Widget   p)
{
 if (made()) return topWidget();
 SLDelay::make(p);
 p= wParent();

 Widget w= XtVaCreateManagedWidget( _name, xmFrameWidgetClass, p, NULL);
 setTopWidget(w);
 _da= XtVaCreateManagedWidget("colorbar", xmDrawingAreaWidgetClass,w,NULL);
 XtAddCallback( _da, XmNexposeCallback, 
               (XtCallbackProc)CExposeCallback, (XtPointer)this );

 return w;
}


void SeisCbar::CExposeCallback(Widget w,
                                     XtPointer udata,
                                     XmDrawingAreaCallbackStruct *CBdata)

{
  SeisCbar *obj = (SeisCbar *)udata;
  obj->CExpose(w, udata, CBdata);
}


void SeisCbar::CExpose(Widget,
                       XtPointer,
                       XmDrawingAreaCallbackStruct *)

{
  if (_da) drawCbarOn(_da);
}


void SeisCbar::setPredef(int predef)
{
  SeisColor::setPredef(predef);
  if (_da) drawCbarOn(_da);
}

int SeisCbar::readColorFile(char *fname)
{
  int succ= SeisColor::readColorFile(fname);
  if (_da) drawCbarOn(_da);
  return succ;
}

void SeisCbar::redraw (float cell_width_factor)
{
 if (_da) drawCbarOn (_da, cell_width_factor);
}

void SeisCbar::setColormap (Colormap colormap)
{
  assert (_da);
  XtVaSetValues (_da, XmNcolormap, colormap, NULL);
  _col.cmap = colormap;
  if (_col_segment) {
    ColorInfoCollection::updateEverywhere (&_col);
  }
}

void SeisCbar::colorInfoChangedImmediately (ColorInfo *col)
{
  assert (col == &_col);
  redraw ();
}
