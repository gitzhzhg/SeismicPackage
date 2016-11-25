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
#ifndef HARDCOPYTRACEPLOT_HH
#define HARDCOPYTRACEPLOT_HH

class HardCopyPlot;
class SeisPlot;


class HardCopyTracePlot { 

  public:
     enum PlotType { Wiggle, WiggleFill, Color };
  private:
     enum DataType { DataIsFloat, DataIsByte };
     HardCopyPlot     *_hcp;
     long              _rp;       //  boolean value;
     long              _RtoL;
     long              _invert_yaxis;
     float             _ct;
     float             _is;
     float             _ti;
     float             _sr;
     int               _line_index_color;
     float             _trace_data;
     DataType          _data_type;
     PlotType          _ptype;
//   float             _x_offset;
     float             _y_offset;
     float             _plot_width;
     

  public:
     HardCopyTracePlot(HardCopyPlot *hcp);
     void  setRP(long g);
     long  rp()                    { return _rp; }
     void  setRtoL(long g)         { _RtoL= g;}
     long  rToL()                  { return _RtoL; }
     void  setInvert(long g)       { _invert_yaxis = g;}
     long  invert()                { return _invert_yaxis; }
     void  setCT(float v)          { _ct= v;}
     float ct()                    { return _ct; }
     void setIS(float is);
     float is();
     void setTI(float ti)          { _ti= ti;}
     float ti()                    { return _ti;}
     void  setPlotWidth(float w)   {_plot_width= w;}
     void plot(SeisPlot             *sp,
               long                  num_traces, 
               long                  num_samples,
               long                  num_display_samp,
               long                  first_trace,
               int                   frame,
               double                max_amplitude);
     void plot();
     void setSampleRate(float sr)   { _sr= sr;}
     void setPlotType(PlotType ptype);
//     void setXOffset(float xoff) { _x_offset= xoff;}
     void setYOffset(float yoff) { _y_offset= yoff;}

    // -------- not used --------
            void setNumTraces(long t)           { _num_traces= t;}
            void setNumSamplesPerTrace(long s)  { _num_samples= s;}
            long              _num_traces;
            long              _num_samples;

    // -------- not used --------






};
#endif
