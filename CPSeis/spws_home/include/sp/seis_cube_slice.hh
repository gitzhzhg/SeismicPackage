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
#ifndef SEISCUBESLICE_HH
#define SEISCUBESLICE_HH


class SeisPlot;

class SeisCubeSlice {

  public:
      enum AxisType { InLine, CrossLine, TimeSlice };
  private:
      
  protected:     
      SeisPlot   *_sp;
      AxisType    _axis_type;
      int         _current_slice;
      int         _plotted_slice;
      Boolean     _use_temp_global;

      Boolean setTimeSlice(int slice);
      Boolean setInlineSlice(int slice);
      Boolean setCrosslineSlice(int slice);

  public:
      SeisCubeSlice(SeisPlot *sp, AxisType axis_type);

      Boolean isCube();
      void setAxis(AxisType axis_type);
      Boolean setSlice(int slice);
      int totalSlices();
      int currentSlice();
      void getCoords(float *lmin,  float *lmax, 
                     float *xlmin, float *xlmax,
                     float *tsmin, float *tsmax);
      void useTempGlobal (Boolean use_temp = True);
      float convertIndexToWC(int index);
      int   convertWCToIndex(float coord);
      void  getAxisHeaders(int *h1, int *h2);
      SeisPlot *sp();
};
#endif
