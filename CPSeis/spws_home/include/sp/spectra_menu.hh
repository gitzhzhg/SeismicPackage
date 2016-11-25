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
//************************ COPYRIGHT NOTICE ****************************
//      CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.        *
//       PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK         *
//************************ COPYRIGHT NOTICE ****************************



//************************************************************************
//***             Menu to create a spectra analysis plot in a          ***
//***             separate window Author:Michael L. Sherrill 08/2000   ***
//************************************************************************

#ifndef SPECTRA_MENU
#define SPECTRA_MENU

#define MUTE_HDR 1
#define LAV_HDR  24

#include "sl/sl_form_pop.hh"
#include "sp/seis_inform.hh"



class SpectraMenu :  public SLFPopSep, public SeisInform {

   public:
       SpectraMenu( Widget               p,
                    char                 *name,
                    HelpCtx              hctx,
                    class SeisPlot       *displayed_sp);
       virtual ~SpectraMenu();
       virtual Widget make(Widget p);
       virtual void manage();
       virtual void reloadDefaults(Boolean do_method= True);
       virtual void reloadSystemDefaults(Boolean do_method = True);
       virtual Boolean notifyComplex(SLDelay*, int ident);
       virtual void notCurrentInWindow(SeisPlot *sp);
       virtual void newPlot(SeisPlot *sp); //for inform
       virtual void destroyed(SeisPlot *sp);//for inform


       enum { NPLT, ISKP, NDO, NSKP, PLOT_WIDTH,         PLOT_HEIGHT, 
              FIXED_WINDOW,          MUTE_WINDOW,        WINDOW_BEGINNING_FIXED,
              WINDOW_BEGINNING_MUTE, WINDOW_END_FIXED,   WINDOW_END_MUTE, 
              TAPER_LENGTH,          AMPLITUDE_TYPE,     PHASE_TYPE, 
              AMPLITUDE_AND_PHASE_TYPE,                  NORMALIZE,
              TIMEFREQ_TYPE,         SINGLE_SPECTRA,     AVERAGED_SPECTRA,
              FAMILY_SPECTRA,        DISPLAY_LOW_FREQ,   DISPLAY_HI_FREQ,
              LINEAR_AMPLITUDE,      POWER_AMPLITUDE,    DB_AMPLITUDE,
              WRAPPED,               UNWRAPPED,          UNWRAPPED_FLATTENED,
              FREQ_MAX,              UNWRAPPED_LOW_FREQ, UNWRAPPED_HI_FREQ, 
              FLATTENED_LOW_FREQ,    FLATTENED_HI_FREQ,  TF_DISTRIBUTION_MODE,
              TF_DISTRIBUTION_SLICE_MODE, TF_IBAND_MODE, TF_ICENTERFREQ_MODE,
              TF_IDOMINANTFREQ_MODE, TF_IMAXIMALFREQ_MODE,  TF_IQ_MODE,
              TF_NONE_TFMARGINAL,    TF_NORMALIZE_TIMEMARGINAL,
              TF_NORMALIZE_FREQMARGINAL, TF_WINDOW_LENGTH, TF_FFT_LENGTH, 
              TF_OPT_LEVEL, TF_SLICE,TF_ADAPTIVE_OPTIMIZED, 
              TF_SHORT_TIME_FOURIER, TF_SINUSOID, TF_HAMMING, TF_RECTANGULAR,
              TF_HANNING, TF_BLACKMAN,TF_BARTLET, REPLACE_WINDOW, NEW_WINDOW,
              TF_LINEAR, TF_SPLINE, USER_Y, USER_Y_TOP, USER_Y_BOTTOM
            }; 

       enum{TIME_SHIFT_HEADER = 48, AVERAGE_PHASE_HEADER = 49,
            TIME_FIRST_HEADER = 50};

       enum{MAX_STORED_PLOTS = 2};

       float getPlotWidth();
       float getPlotHeight();
       float getDisplayLowFreq();
       float getDisplayHiFreq();
       int   getSpectraType();
       int   getAmplitudeType();
       char  *getPhaseType();
       float getFreqMax();
       float getFlattenedLowFreq();
       float getFlattenedHiFreq();
       float getUnwrappedLowFreq();
       float getUnwrappedHiFreq();
       float getTaperLength();
       float getFixedWindowBeginningTime();
       float getFixedWindowEndingTime(); 
       float getMuteWindowBeginningTime();
       float getMuteWindowEndingTime();
       void  getTracePattern(long *nplt, long *iskp, long *ndo, long *nskp);
       int   getNumSpectra();
       int   getWindowType();
       float getTfWindowLength(){return _tf_window_length;}
       float getTfFftLength(){return _tf_fft_length;}
       float getTfOptLevel(){return _tf_opt_level;}
       float getTfSlice(){return _tf_slice;}
       char  *getTfMode(int *index = NULL);
       char  *getTfMarginal(int *index = NULL);
       long  getNumTimeFreqTraces();
       char  *getFastWindowType(int *index = NULL);
       char  *getKernal(int *index = NULL);
       void  userRemovedPlot(class SpectraPlot *plot);
       int   normalizeFamily();
       int   getSineInterpolation();
       int   userSettingDisplayedY(float *top, float *bottom);

   protected:
       class SeisPlot                  *_displayed_sp;
       Boolean                         _first_time;
       Boolean                         _first_plot_made;
       Boolean                         _finished_make;
       int                             _nplt;
       int                             _iskp;
       int                             _ndo;
       int                             _nskp;
       float                           _pwidth;
       float                           _pheight;
       float                           _left;
       float                           _right;
       float                           _top;
       float                           _bottom;
       float                           _plot_width;
       float                           _plot_height;
       float                           _window_beginning_fixed;
       float                           _window_beginning_mute;
       float                           _window_end_fixed;
       float                           _window_end_mute;
       float                           _taper_length;
       float                           _freq_max;
       float                           _display_low_freq;
       float                           _display_hi_freq;
       float                           _user_y_top;
       float                           _user_y_bottom;
       float                           _unwrapped_low_freq;
       float                           _unwrapped_hi_freq;
       float                           _flattened_low_freq;
       float                           _flattened_hi_freq;
       float                           _tf_window_length;
       float                           _tf_fft_length;
       float                           _tf_opt_level;
       float                           _tf_slice;
       char                            _filename[512];
       class SLTextBox                 *_trace_select_box;
       class SLTextBox                 *_plot_size_box;
       class SLRadioBox                *_window_option_box;
       class SLRadioBox                *_window_type_box;
       class SLTextBox                 *_window_beginning_box;
       class SLTextBox                 *_window_end_box;
       class SLTextBox                 *_taper_length_box;
       class SLRadioBox                *_spectra_type_box;
       class SLTogBox                  *_normalize_box;
       class SLRadioBox                *_display_selected_box;
       class SLTextBox                 *_display_freq_box;
       class SLTogBox                  *_user_y_tog;
       class SLTextBox                 *_user_y_box;
       class SLRadioBox                *_amplitude_type_box;
       class SLRadioBox                *_phase_type_box;
       class SLTextBox                 *_freq_limit_box;
       class SLOptionMenu              *_tf_distribution_options;
       class SLOptionMenu              *_tf_mode_options;
       class SLOptionMenu              *_tf_marginal_options;
       class SLOptionMenu              *_tf_short_time_window;
       class SLOptionMenu              *_tf_sine_interpolation;
       class SLTextBox                 *_tf_text_params;
       Widget                          _information;
       virtual void DoAction();
       virtual void okButton();
       virtual void applyButton();
       virtual int validate();
       Boolean setPlotParameters();
 
 

  private:
       void  initValues();
       int   checkTracePattern();
       void  checkPlotSize();      
       void  setWindowSensitivity();
       int   checkWindow(Boolean check_size = True);
       void  checkTaper();
       void  setSpectraType();
       void  setPhaseType();
       float getSmallestMute();
       float getLargestMute();
       void  setTimeFreqState(Boolean check_size = True);
       class SpectraPlot **_plot_array; 
       void  setNormalizeState();
};







#endif
