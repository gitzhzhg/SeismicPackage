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
#include "sp/multi_zoom_inform.hh"
#include "sp/multi_zoom.hh"


MultiZoomInform::MultiZoomInform(MultiZoom *mz, SeisPlot *sp)  :
                 SeisInform(sp), _mz(mz)
{
}

void MultiZoomInform::preZoom(SeisPlot           *sp, 
                              SeisZoomer         *zoomer, 
                              SeisPlot::ZoomDir   dir)
{
  if (dir == SeisPlot::Up || dir == SeisPlot::UpSeparateWin)
         _mz->setZoomer(sp, zoomer);
}

void MultiZoomInform::postZoom(SeisPlot *sp, SeisPlot::ZoomDir dir)
{
  if (dir == SeisPlot::Up            || 
      dir == SeisPlot::UpSeparateWin ||
      dir == SeisPlot::Abort ) {
               _mz->abortOthers(sp);
  }
}

void MultiZoomInform::postZoomSeparateWindow(SeisPlot *, SeisPlot *)
{
}
