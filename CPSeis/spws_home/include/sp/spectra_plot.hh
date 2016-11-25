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
//********************************************************
//Author Michael L. Sherrill 2000-08-07
//Class that creates window and plot for spectra analysis
//********************************************************

#ifndef SPECTRA_PLOT
#define SPECTRA_PLOT

#include "sl/sl_form_help_pop.hh"
#include "sp/seis_inform.hh"

#include "wproc.h"


class SpectraPlot :  public SLFormHelpPop, public SeisInform {


 public:
       SpectraPlot(Widget               p,
                   char                 *name,
                   HelpCtx              hctx,
                   class SeisPlot       *parent_sp,
                   class SpectraMenu    *menu,
                   int                  numcolors);

       virtual ~SpectraPlot();

       virtual void manage();
       virtual Widget make(Widget p);
       virtual void mouseOutputUpdate(class SeisPlot *sp, float x, float y);
       class   SeisPlot *getSP() {return _sp;}
       virtual int runAnalysis();
       virtual int plot();
       virtual void newPlot(SeisPlot *sp); //inform
       virtual void destroyed(SeisPlot *sp);
       virtual void removeButton();
       virtual void postScan(SeisPlot *sp, SeisPlot::ScanDir dir);
       virtual void extraButton(int ident);
       virtual void prepareForRedisplay();
       virtual void clearCbytImageMarks();
       enum{MAX_NAMED_COLORS = 10, COLOR_POP = 11, COLOR_BAR = 12, SPCT = 13,
            GRIDX_HDR = 57,        GRIDY_HDR = 58, ANNO_HDR  = 59 };
       enum{VECTOR_TYPE, COLOR_TYPE};

  protected:

  private:
       class SeisPlot           *_parent_sp;
       class SeisPlot           *_sp;
       class SpectraMenu        *_menu;
       class SeisCbarPop        *_cbar_pop;
       class SeisColorPop       *_color_pop;
       class SLPushBox          *_color_opts;
       class TagLinkedList      *_tagll;
       class Tag                *_tag;
       class SeisLocOut         *_loc;
       class SpectraTablePop    *_spectra_table_pop;
       Boolean                  _been_managed;
       long                     _numcolors;
       class VectData           *_vect_data;
       class VectData           *_cbyt_vect_data;
       class SeisVectLinkedList *_vect_ll;
       class SeisVectLinkedList *_cbyt_vect_ll;
       class Vector             *_vector;
       class Vector             *_cbyt_vector;
       ColorInfo                *_col;
       int                      _num_vector_points;
       float                    _x1, _x2, _y1, _y2;
       Boolean                  _vectors_ok;
       float                    *_xdata, *_ydata;
       float                    *_cbyt_xdata, *_cbyt_ydata;
       float                    _new_sample_rate;
       float                    _max_frequency_returned;
       float                    _freq_scale;
       long                     _color_index;
       int                      _cbyt_pixel_count;
       int                      _vector_count;
       float                    *_trace_in;
       double                   *_dhd; 
       float                    *_amps_out;
       float                    *_phase_out;
       float                    *_avg_amps_out;
       float                    *_avg_phase_out;
       unsigned long            *_cbyt_pixels; 
       unsigned int             *_dead_trace_index;
       float                    _window_begin;
       float                    _window_end;
       long                     _num_time_freq;
       int                      _plot_type;
       int                      _next_plot_type;
    
       int  allocateArrays();
       void cleanupArrays();
       void processPowerOrDb(float *amps);
       void getMinAndMaxOfData(long n, float *data, float *min, float *max);
       int  createVectors(long index, float *amps, float *phase);
       int  createNormalizedVectors(long count, float *amps);
       int  createAveragedVector(long nplt, float *amps, float *phase);
       void drawColorLabels();
       void drawAxisLabels(); 
       void markCbytImageTraces();
       void labelPlot();
       void computeWindow(float *hd, float *beginning, float *end);          
       long getNumSamplesPerWindow();
};

#endif
