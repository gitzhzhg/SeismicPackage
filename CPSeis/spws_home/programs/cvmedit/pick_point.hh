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
#ifndef _PICK_POINT_HH
#define _PICK_POINT_HH

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "plot/pick_base.hh"

class PlotBase;

class PickPoint : public PickBase
{
  public:

    PickPoint(PlotBase *plot);
    PickPoint(PlotBase *plot, float *x1, float *x2, float *x3);
    ~PickPoint();
                
    void getPosition(float *x1, float *x2, float *x3);
    void setPosition(float *x1, float *x2, float *x3)
     {_x1 = x1; _x2 = x2; _x3 = x3;}

  protected:

    float           *_x1;
    float           *_x2;
    float           *_x3;

    void noModButtonOnePress    (int x , int y  );

    void MapToWorld(int xx, int yy,
     float *p, float *s, float *t, float *zeit);
    int PointInside(float px,float py,
        float ex,float ey,float width,float height);


  private:

};

#endif


