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
#include "pick_point.hh"
#include "sl/prim_support.hh"
#include "sp/seis_plot.hh"
#include "seis_transf.hh"

#ifdef __cplusplus
extern "C" {                 // for C++
#endif

#ifdef __cplusplus
}                   // for C++
#endif


PickPoint::PickPoint(PlotBase *plot)
	: PickBase(plot)
{_x1 = 0;
 _x2 = 0;
 _x3 = 0;
}

PickPoint::PickPoint(PlotBase *plot, float *x1, float *x2, float *x3)
	: PickBase(plot)
{_x1 = x1;
 _x2 = x2;
 _x3 = x3;
}

PickPoint::~PickPoint()
{_x1=0; _x2=0; _x3=0;
}

void PickPoint::noModButtonOnePress(int x, int y)
{float t;
 if(_x1 && _x2);
 MapToWorld(x, y, _x2, _x3, &t, _x1);
 PrimSupport::updateEverything();
}

void PickPoint::getPosition(float *x1, float *x2, float *x3)
{
 *x1 =*_x1;
 *x2 =*_x2;
 *x3 =*_x3;
 printf("x=%f y=%f z=%f\n",*_x2,*_x3,*_x1);
}

void PickPoint::MapToWorld(int xx, int yy,
     float *p, float *s, float *t, float *zeit)
{// convert pixel values (xx,yy) into user data values.
 // map xx --> (p,s,t)|nd yy --> zeit
 SeisTransf *tr=NULL;
 SeisPlot   *sp=NULL;
 sp = (SeisPlot *) getPlot();
 *p = 0;
 *s = 0;
 *t = 0;
 *zeit=0;
 *p = sp->xWC(xx);
 *zeit = sp->yWC(yy);
 if(sp->plotType() >= PlotImage::PlotCOLOR) return;
 tr = (SeisTransf *) sp->transform();
 if(tr)
  {int trace= (int) sp->getTraceFromPixel( ( long) xx);
   tr->TraceToKeys(sp,trace,p,s,t);
  }
}

int PickPoint::PointInside(float px,float py,
    float ex,float ey,float width,float height)
{
  if (px >= ex && px <= ex+width &&
      py >= ey && py <= ey+height) return 1;
  return 0;
}
