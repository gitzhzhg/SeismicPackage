#include "sp/seis_plot.hh"
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
#include "cube/cube_slice_transform.hh"



CubeSliceTransform::CubeSliceTransform( WhichPlot axis, 
                                        int      h1,
                                        int      h2) :
                       _axis(axis), _h1(h1), _h2(h2)
{}

float CubeSliceTransform::convDataToXOffset(SeisPlot *sp, float data)
{
 float retval;
 int header= (_axis == CrossLine) ? _h2 : _h1;
 float trace= sp->getTraceFromHeader(header,data);
 if(trace < 0) return -1.0;

 //The following commented since the trace number value should not
 //need to be adjusted for movies in this application. MLS 01/99
 // if (sp->movie())
 //        retval= trace - (sp->currentFrame() * sp->plottedNplt());
 //else 
 retval= trace;

 //if plotting color add half a trace so data will be centered in trace
 if(sp->plotType() == PlotImage::PlotCOLOR) retval += .5;
 return retval;
}

float CubeSliceTransform::convXOffsetToData(SeisPlot *sp, float trace)
{
 int header= (_axis == CrossLine) ? _h2 : _h1;
 float retval= sp->getHeaderFromTrace((int) (trace + 0.5F), header);
 return retval;
}

float CubeSliceTransform::convDataToYOffset(SeisPlot *, float data)
{
 return data; 
}

float CubeSliceTransform::convYOffsetToData(SeisPlot *, float offset)
{
 return offset;
}

void CubeSliceTransform::setHeaders(int h1, int h2)
{
  _h1= h1;
  _h2= h2;
}
void CubeSliceTransform::getHeaders(int *h1, int *h2)
{
  *h1= _h1;
  *h2= _h2;
}
