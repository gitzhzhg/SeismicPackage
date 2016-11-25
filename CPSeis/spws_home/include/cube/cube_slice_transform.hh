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
#ifndef CUBESLICETRANSFORM_HH 
#define CUBESLICETRANSFORM_HH

#include "sp/sp_transform.hh"

class SeisPlot;


class CubeSliceTransform : public SPTransform {
  public:
   enum WhichPlot {InLine, CrossLine, TimeSlice};
  private:
      int      _h1;
      int      _h2;
      WhichPlot _axis;
      
  public:
   CubeSliceTransform( WhichPlot axis, int h1 =17, int h2 =18);
   void setHeaders(int h1, int h2);
   void getHeaders(int *h1, int *h2);
   virtual float convDataToXOffset(SeisPlot*, float data);
   virtual float convXOffsetToData(SeisPlot*, float offset);
   virtual float convDataToYOffset(SeisPlot*, float data);
   virtual float convYOffsetToData(SeisPlot*, float offset);
};

#endif

