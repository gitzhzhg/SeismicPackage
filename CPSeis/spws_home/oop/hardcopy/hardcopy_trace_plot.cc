#include "hardcopy/hardcopy_plot.hh" 
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
#include "hardcopy/hardcopy_trace_plot.hh" 
#include "sp/seis_plot.hh" 
#include "image_amplitude_recovery.hh"
#include <assert.h> 
#include <stdlib.h> 
#include <locale.h>
#include "cgm.h"

#define True  1
#define False 0



HardCopyTracePlot::HardCopyTracePlot(HardCopyPlot *hcp) :
                        _hcp(hcp), _sr(0), _is(2.0), _ct(4.0),
                        _RtoL(False), _ptype(WiggleFill), _rp(False),
                        _y_offset(0), _invert_yaxis(False),
                        _plot_width(1)
{
  cgmPipTraceOrientation(0.0F,-1.0F,1.0F,0.0F);
  _line_index_color= HardCopyPlot::blackColor();
  cgmPipTraceVaFill(.001,.999,0,0,0,_line_index_color,0,0,0,0);
  setPlotType(_ptype);
}




void  HardCopyTracePlot::setRP(long g) { _rp= g; }
void HardCopyTracePlot::setIS(float is) { _is= is; }

void  HardCopyTracePlot::setPlotType(PlotType ptype)
{
  static int     wiggle_modes[2]={1,2};
  static int     color_modes[2]={4};
  _ptype= ptype;
  switch (_ptype) {
       case Wiggle :    cgmPipTraceDisplayModes(wiggle_modes, 1); break;
       case WiggleFill: cgmPipTraceDisplayModes(wiggle_modes, 2); break;
       case Color     : cgmPipTraceDisplayModes(color_modes, 1); break;
  } // end switch

}



void HardCopyTracePlot::plot(SeisPlot       *sp,
                             long            num_traces, 
                             long            num_samples,
                             long            num_display_samp,
                             long            first_trace,
                             int             frame,
                             double          max_amplitude)
{
   long i,j,k, pos;
   assert(_sr);
   float   baseline_sf;
   float   amplitude_sf;
   float   this_trace_place=0;
   unsigned char   *byt_trace=NULL;
   float   *this_trace=NULL;
   float   trace_spacing;
   float   scale_amp;
   long    start_trace;
   long    end_trace;
   int     increment;
   //long    start, end;
   float   offset;
   int     y_direction= 1;
   float   start_point;


   _hcp->setTransformation(HardCopyPlot::INCHES);
   _hcp->setLineColor(_line_index_color);
   float ti= num_traces /  _plot_width;
   baseline_sf=  _is * _sr;
   trace_spacing = 1.0 / ti;

   scale_amp = 128.0;
   _ct = _ct / 2.0;

   amplitude_sf= _ct * trace_spacing;

   cgmPipTraceScaleFactors(baseline_sf, amplitude_sf);
   

   if (_RtoL) {
       start_trace= first_trace; 
       end_trace= first_trace - num_traces; 
       increment= -1;
   } // end if
   else {
       start_trace= first_trace; 
       end_trace= num_traces + first_trace; 
       increment= 1;
   } // end else
   this_trace= new float[num_display_samp];

   if (_invert_yaxis) y_direction= -1;

   offset= _hcp->xMap();
   //if (offset) offset--;
   //start_point= _hcp->leftBorderWidth() + (trace_spacing * offset ) + 
   //                                 (trace_spacing * 0.5 ); 
   start_point= offset + (trace_spacing * 0.5 ); 

   _hcp->setTransformation(HardCopyPlot::YplotXinches);
   for(i=start_trace,pos=0; (i!= end_trace); i+= increment,pos++) {

         sp->getAmplitudeRecovery()->scaleDataForDisplay(0,
                       ((frame-1) * num_traces) +  i, 0,num_samples - 1);
         byt_trace= sp->getAmplitudeRecovery()->getScaledByteDataArray(0); 

         k=  (_invert_yaxis) ? num_display_samp-1 : 0;         
         if (!_rp) {
             for(j=0; (j< num_display_samp); j++, k+= y_direction)
                    this_trace[j]= byt_trace[k] - scale_amp;
         }
         else {
             for(j=0; (j< num_display_samp); j++, k+= y_direction)
                    this_trace[j]= (byt_trace[k] - scale_amp) * -1;
         }
         cgmPipTrace( start_point + (pos * trace_spacing),
                          _hcp->getY0() + _y_offset, 
                          0, max_amplitude, 
                         this_trace, (int)num_display_samp, 
                         NULL, 0, NULL, 0 );
         this_trace_place++;
   } // end if

}
