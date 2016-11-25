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
//***             Creates a spectra analysis plot in a                 ***
//***             separate window Author:Michael L. Sherrill 08/2000   ***
//************************************************************************

#include "sp/spectra_plot.hh"
#include "sp/spectra_menu.hh"
#include "sp/spectra_table_pop.hh"
#include "sp/seis_plot.hh"
#include "sp/seis_loc_out.hh"
#include "sp/seis_cbar_pop.hh"
#include "sp/seis_color_pop.hh"
#include "image_amplitude_recovery.hh"
#include "sl/sl_client_message.hh"
#include "sl/shell_stat_msg.hh"
#include "sl/sl_push_box.hh"
#include "spectraprim_c.h"
#include "rice.h"
#include "sine.h"
#include "vect/vect_data.hh"
#include "vect/ll_seis_vect.hh"
#include "vect/ll_tag.hh"
#include "vect/tag.hh"
#include "sl/shell_watch.hh"
#include "sl/sl_error_pop.hh"
#include "sl/colorset_collection.hh"
#include "sl/paintset_collection.hh"
#include "sl/paintset.hh"
#include "minimax.h"

#include <stdlib.h>
#include <math.h>

#define MUTE_HEADER 1
#define LAV_HEADER  24


static String  defres[]= 
    {
    ".resizePolicy:              RESIZE_NONE",
    //".height:                    375",
    //".width:                     1100",
    "*Active.background:         red",
    NULL
    };

static String namedColors[] =
    {
    "blue", "red", "green", "brown", "cyan", "yellow", "orange",
    "dark sea green", "pink", "black", NULL 
    };

//============================================================================
//====================== Constructor with own window =========================
//============================================================================
SpectraPlot::SpectraPlot(Widget               p,
                         char                 *name,
                         HelpCtx              hctx, 
                         SeisPlot             *parent_sp,
                         SpectraMenu          *menu,
                         int                  numcolors)
                   : SLFormHelpPop(p, name, FP_DOREMOVE | FP_DOHELP,
	                           hctx, True, 2, False, True, numcolors, True),
                   SeisInform(parent_sp), 
                     _parent_sp(parent_sp), _numcolors(numcolors), 
                     _been_managed(False), _menu(menu) 
                    
{
  
  _sp                  = NULL;
  _tag                 = NULL;
  _tagll               = NULL;
  _loc                 = NULL;
  _cbar_pop            = NULL;
  _xdata               = NULL;
  _ydata               = NULL;
  _cbyt_xdata          = NULL;
  _cbyt_ydata          = NULL;
  _color_pop           = NULL;
  _color_index         = 0;
  _vector_count        = 0;
  _trace_in            = NULL;
  _dhd                 = NULL;
  _amps_out            = NULL;
  _phase_out           = NULL;
  _avg_amps_out        = NULL;
  _avg_phase_out       = NULL;
  _vector              = NULL;
  _cbyt_vector         = NULL;
  _cbyt_vect_ll        = NULL;
  _cbyt_vect_data      = NULL;
  _dead_trace_index    = NULL;
  _spectra_table_pop   = NULL;
  _col                 = NULL;

  setDefaultResources( p, name, defres);

  make(p);

  //Add a mouse readout that supports the frequencies
  if(_menu->getSpectraType() == SpectraMenu::TIMEFREQ_TYPE)
    addSeisPlot(_sp);

  //Spct type, create popup to display list of time, shift, and phase values
  if(_menu->getSpectraType() != SpectraMenu::TIMEFREQ_TYPE &&
     (strcmp(_menu->getPhaseType(), "FLATTEN") == 0))
    {
    _spectra_table_pop = new SpectraTablePop(topWidget(),"spct_table",
                                             getHelpCtx(),"SPCT");
    }


  _vect_ll   = new SeisVectLinkedList();
  _vect_ll->addPlot(_sp);
  _vect_data = NULL;

  _col = new ColorInfo;
  _col->numplanes = 0;
  _col->cmap = _sp->getColormap ();

}



//============================================================================
//====================== Destructor               =========================
//============================================================================
SpectraPlot::~SpectraPlot()
{

  if (_col->cnum > 0) {
/*    
    XFreeColors(_sp->getDisplay(), _sp->getColormap(), _pixels,
                _pixel_count, 0UL);
*/
    ColorsetCollection::clear (_col);
  }
  ColorsetCollection::remove (_col);
  delete _col;


  if(_vect_ll)        delete _vect_ll;
  if(_cbyt_vect_ll)   delete _cbyt_vect_ll;
  if(_vect_data)      delete _vect_data;
  if(_cbyt_vect_data) delete _cbyt_vect_data;
  if(_xdata)          free(_xdata);
  if(_ydata)          free(_ydata);
  if(_cbyt_xdata)     free(_cbyt_xdata);
  if(_cbyt_ydata)     free(_cbyt_ydata);
  if(_tagll)          delete _tagll;
  if(_sp)             delete _sp;
  if(_tag)            delete _tag;
  if(_loc)            delete _loc;
  if(_cbar_pop)       delete _cbar_pop;
  if(_color_pop)      delete _color_pop;
  if(_dead_trace_index) free (_dead_trace_index); 
  if(_spectra_table_pop)delete(_spectra_table_pop);
  cleanupArrays();
}


//============================================================================
//====================== Make Popup                  =========================
//============================================================================
Widget SpectraPlot::make(Widget p)
{
 
   if ( made() ) return topWidget();

   Widget parent = p ? p : wParent();
   ShellStatMsg bld_info(parent, "Building Spectra Plot...");


   SLFormHelpPop::make(p);

   setTitle("Spectrum Analysis Plot");

   _sp = new SeisPlot(topWidget(), "Spectra");

   _sp->setLoadableColors(max(33,_numcolors));

   
   _sp->initArrayTypePlot();
   _sp->cancelArrayTypeData();

   //May want these later
   //_tagll = new TagLinkedList(_sp);
   //_tag = NULL;


   _loc = new SeisLocOut(topWidget(),"spectra_loc_out", _sp, getHelpCtx());

   return topWidget();
  
}


//============================================================================
//====================== Manage Popup                =========================
//============================================================================
void SpectraPlot::manage()
{
int pixels_wide, pixels_high;
int hpixels_per_inch, vpixels_per_inch;

  //The following attachements were put into this area because we get
  //a circular dependency error if it is destroyed and never managed
  //and the attachments have been registered.
  if(_been_managed == False)
   { 

   //If a color fill type plot create the color options and color bar buttons
   if(_menu->getSpectraType() == SpectraMenu::TIMEFREQ_TYPE &&
      (strcmp(_menu->getTfMode(),"slice") != 0)                  )
      {
      addExtraButton("Colors Options", COLOR_POP);
      addExtraButton("Color Bar", COLOR_BAR);
      }

   //If spct flatten make a button for the table 
   if(_menu->getSpectraType() != SpectraMenu::TIMEFREQ_TYPE &&
      (strcmp(_menu->getPhaseType(), "FLATTEN") == 0))
       addExtraButton("Flatten values", SPCT);

   XtVaSetValues( topWidget(), XmNresizePolicy, XmRESIZE_NONE, NULL);

   //Set the window to be the approximate size of the plot
   hpixels_per_inch = _sp->getHorizontalPixelsPerInch(
                            XtDisplay(_parent_sp->getWidget()), 
                            DefaultScreen(XtDisplay(_parent_sp->getWidget())) );
   pixels_wide = (int)((_menu->getPlotWidth() * hpixels_per_inch) + 
                       _sp->leftBorder() + _sp->rightBorder());
   pixels_wide += 15;//add pixels for scroll bar size
  
   vpixels_per_inch = _sp->getVerticalPixelsPerInch(
                            XtDisplay(_parent_sp->getWidget()), 
                            DefaultScreen(XtDisplay(_parent_sp->getWidget())) );
   pixels_high = (int)((_menu->getPlotHeight() * vpixels_per_inch) +
                       _sp->topBorder() + _sp->bottomBorder());
   pixels_high += 120;//add pixels for the widget readout height

   pixels_wide = min(pixels_wide, 1200);
   pixels_wide = max(700, pixels_wide);//so all buttons will show on bottom
   pixels_high = min(pixels_high, 1000);

   XtVaSetValues(topWidget(),XmNwidth,pixels_wide,XmNheight,pixels_high,NULL);


   XtVaSetValues( _sp->W(), XmNleftAttachment,   XmATTACH_FORM,
                            XmNrightAttachment,  XmATTACH_FORM,
                            XmNtopAttachment,    XmATTACH_FORM,
                            XmNbottomAttachment, XmATTACH_WIDGET,
                            XmNbottomWidget,     bottomSeparator(), 
                            NULL);

   XtVaSetValues(_loc->W(), XmNrightAttachment,  XmATTACH_FORM,
                            XmNbottomAttachment, XmATTACH_FORM,
                            NULL);

   XtVaSetValues(buttonContainer(),
                            XmNmarginHeight,     5,
                            XmNleftAttachment,   XmATTACH_FORM,
                            XmNrightAttachment,  XmATTACH_WIDGET,
                            XmNrightWidget,      _loc->W(),
                            XmNrightOffset,      5,
                            NULL);


   _been_managed = True;

   }//end never been managed


  SLFormHelpPop::manage();

  XSync(XtDisplay(topWidget()),False);

}


void SpectraPlot::extraButton(int ident)
{

  switch(ident)
    {
    case COLOR_POP:
      //_color_pop->setTitle("Something to identify what plot this goes with");
      _color_pop->makeAndManage();
    break;

    case COLOR_BAR:
      //_cbar_pop->setTitle("Something to identify what plot this goes with");
      _cbar_pop->makeAndManage();
    break;

    case SPCT:
      _spectra_table_pop->manage();
    break;

    default:
      assert(0);
    break;
    }     
}




int SpectraPlot::allocateArrays()
{
int error = 1;
long nplt, iskp, ndo, nskp;

  _menu->getTracePattern(&nplt, &iskp, &ndo, &nskp);

  _trace_in  = (float *) calloc( 1, 
                (int)(nplt * _parent_sp->displayedSamples() * sizeof(float)));
  if(_trace_in == NULL)
     return error;

  _dhd       = (double *) calloc( 1, 
                (int)(nplt * _parent_sp->numHeaders() * sizeof(double) ));
  if(_dhd == NULL)
    {
    free (_trace_in);
    return error;
    }

  //Determine the number of traces we will get back from rice_timefreq
  _num_time_freq = _menu->getNumTimeFreqTraces();


  _amps_out  = (float *) calloc( 1, 
               (int)((nplt * _num_time_freq) * _parent_sp->displayedSamples() *
                                                               sizeof(float)));
  if(_amps_out == NULL)
    {
    free (_trace_in);
    free (_dhd);
    return error;
    }


  _phase_out = (float *) calloc( 1, 
                 (int)(nplt * _parent_sp->displayedSamples() * sizeof(float)));
  if(_phase_out == NULL)
    {
    free (_trace_in);
    free (_dhd);
    free (_amps_out);
    return error;
    }

  if(_menu->getNumSpectra()  == SpectraMenu::AVERAGED_SPECTRA)
    {
    _avg_amps_out = (float *) calloc( 1, 
                 (int)(nplt * _parent_sp->displayedSamples() * sizeof(float)));
    if(_avg_amps_out == NULL)
      {
      free (_trace_in);
      free (_dhd);
      free (_amps_out);
      free (_phase_out);
      return error;
      }

    _avg_phase_out = (float *) calloc( 1, 
                 (int)(nplt * _parent_sp->displayedSamples() * sizeof(float)));
    if(_avg_phase_out == NULL)
      {
      free (_trace_in);
      free (_dhd);
      free (_amps_out);
      free (_phase_out);
      free (_avg_amps_out);
      return error;
      }
    }

  _dead_trace_index  = (unsigned int *) calloc( 1, 
                (int)(nplt * sizeof(unsigned int)));
   if(_dead_trace_index == NULL)
      {
      free (_trace_in);
      free (_dhd);
      free (_amps_out);
      free (_phase_out);
      if(_avg_amps_out)  free (_avg_amps_out);
      if(_avg_phase_out) free (_avg_phase_out);
      return error;
      }

 
  return (error = 0);
}





void SpectraPlot::cleanupArrays()
{

  if(_trace_in         != NULL) free (_trace_in); _trace_in = NULL;
  if(_dhd              != NULL) free (_dhd); _dhd = NULL;
  if(_amps_out         != NULL) free (_amps_out); _amps_out = NULL;
  if(_phase_out        != NULL) free (_phase_out); _phase_out = NULL;
  if(_avg_amps_out     != NULL) free (_avg_amps_out); _avg_amps_out = NULL;
  if(_avg_phase_out    != NULL) free (_avg_phase_out); _avg_phase_out = NULL;
  if(_xdata            != NULL) free (_xdata); _xdata = NULL;
  if(_ydata            != NULL) free (_ydata); _ydata = NULL;
}




int SpectraPlot::runAnalysis()
{
int error = 0;
AmplitudeRecovery *ar = _parent_sp->getAmplitudeRecovery();
float *hd = _parent_sp->getHeaderArrayForUpdate();
long i, j, k, nplt, iskp, ndo, nskp;
long original_hoffset;
long trace_in_offset, dhd_offset;
long trace_count, n1, n2, hit_end, ngrps, num;
long trace_num, m, n;
long out_offset;
long window_offset_samps;
int  num_tf_traces_in = 1;
char *scale_mode;
char *tfmode, *kernal_mode;
char *window_type;
int  nsamp, stat;
long total_traces;
float *tf_headers, tf_slice;
long last_tf_trace_created = 0;
long tf_offset, slice_index;
long tfi, txi;
Boolean doing_tf_slice = False;
ShellWatch watch= topWidget();
int window_length, fft_length;
double dbthr = 100.0;
int kmode;
SLErrorPop *error_pop;
char errmsg[5000], tmpmsg[50];
int notify_of_dead = 0;
long max_traces       = 0;

  for(i = 0; i < _parent_sp->plottedFrames(); i++)
    max_traces += _parent_sp->displayedTraces(i);


  XSync(XtDisplay(topWidget()),False);

  _num_vector_points = 0;

  //the following param may need to be put in the user's menu
  _freq_scale = 100.0F;

  _menu->getTracePattern(&nplt, &iskp, &ndo, &nskp);

  error = allocateArrays();
  if(error)
    {
    cleanupArrays();
    return error;
    }


  //Set time frequency params
  if(_menu->getSpectraType() == SpectraMenu::TIMEFREQ_TYPE)
    {
    nsamp = getNumSamplesPerWindow();
    total_traces = _num_time_freq * nplt;
    stat = _sp->initArrayTypeData(1, 1, total_traces, nsamp, _amps_out);
    if(!stat) return (error = 1);
    tf_headers = _sp->getHeaderArrayForUpdate();
    if(tf_headers == NULL) return (error = 1);
    _sp->setHeaders(GRIDX_HDR, GRIDY_HDR);
    _sp->setXLabelHeader(ANNO_HDR + 1);

    kernal_mode = _menu->getKernal(&kmode);

    window_type = _menu->getFastWindowType();

    if(_menu->getAmplitudeType() == SpectraMenu::LINEAR_AMPLITUDE ||
       _menu->getAmplitudeType() == SpectraMenu::POWER_AMPLITUDE    )
      scale_mode = "lin";
    else
      scale_mode = "dB";

    //Rice does not understand a slice so make sure we call it
    //for distribution when the user has set the slice mode
    tfmode = _menu->getTfMode();
    if(strcmp(tfmode,"slice") == 0)
      {
      tfmode = "tfdist";
      doing_tf_slice = True;
      }
    }

  



  //Set up the user requested read pattern and perform the analysis
  trace_count = 0;
  n1          = iskp + 1;
  n2          = n1 + (ndo - 1);
  hit_end     = 0;
  ngrps       = ((nplt - 1) / ndo) + 1;

  for(i = 1, j = 0; i <= ngrps; i++)
    {
    if(n2 < 1 || n2 > max_traces) 
      hit_end = 1;
    num = ndo;
    num = (nplt - (i - 1) * num < num) ? nplt - (i - 1) * num : num;

    for(j = 0; j < num; j++)
      {
      trace_num = n1 + j;
      trace_in_offset = trace_count * _parent_sp->displayedSamples();
      dhd_offset      = trace_count * _parent_sp->numHeaders();
      original_hoffset= (trace_num - 1) * _parent_sp->numHeaders();
      if(!doing_tf_slice)
        out_offset    = _num_vector_points * trace_count;
      else
        out_offset    = trace_count;

      //Store reference to dead traces
      if(hd[original_hoffset + LAV_HEADER] == 0.0F)
        {
        _dead_trace_index[trace_count] = 1;
        ++notify_of_dead;
        if(notify_of_dead < 51)//Only print out the first 100
          {
          sprintf(tmpmsg," Dead Input trace %d ignored\n",trace_num);
          strcat(errmsg,tmpmsg);
          }
        }
      else
        {
        _dead_trace_index[trace_count] = 0;
        }

      for(k = 0; k < _parent_sp->displayedSamples(); k++)
        _trace_in[k + trace_in_offset] = 
           ar->getTrueAmplitude(trace_num - 1, k);

      for(k = 0; k < _parent_sp->numHeaders(); k++)
        _dhd[k + dhd_offset] = hd[k + original_hoffset];

      computeWindow(&hd[k + original_hoffset], &_window_begin, 
                    &_window_end);

      //Spct type
      if(_menu->getSpectraType() != SpectraMenu::TIMEFREQ_TYPE)
        fprintf(stderr,"Error: spectraprim_c missing:\n");
        /*
        {
        _num_vector_points = spectraprim_c(
          _menu->getPhaseType(),             SpectraMenu::TIME_SHIFT_HEADER,
          SpectraMenu::AVERAGE_PHASE_HEADER, SpectraMenu::TIME_FIRST_HEADER,
          _menu->getFlattenedLowFreq(),      _menu->getFlattenedHiFreq(),
          _menu->getFreqMax(),               _freq_scale,       
          _menu->getUnwrappedLowFreq(),      _menu->getUnwrappedHiFreq(),
          _menu->getTaperLength(),           _window_begin,
          _window_end,                       _parent_sp->displayedSamples(),
          _parent_sp->numHeaders(),          _parent_sp->sampleRate(),
          _parent_sp->plottedTmin(),         &_new_sample_rate,
          &_trace_in[trace_in_offset],       &_dhd[dhd_offset],
          &_amps_out[out_offset],            &_phase_out[out_offset]      );
        }
        */
      else//Do time frequency analysis
        {
        window_offset_samps = (long)((_window_begin - _parent_sp->plottedTmin())
                                  / _parent_sp->sampleRate() );
        window_length = (int)(_menu->getTfWindowLength() 
                                  / _parent_sp->sampleRate() + 1.5);
        fft_length    = (int)(_menu->getTfFftLength() 
                                  / _parent_sp->sampleRate() + 1.5);

        if(kmode != SpectraMenu::TF_SINUSOID)        
          _num_vector_points  = rice_timefreq(
                      &_trace_in[trace_in_offset + window_offset_samps],
                      nsamp,
                      &_amps_out[out_offset * _num_time_freq * nsamp],
                      _parent_sp->sampleRate(), 
                      num_tf_traces_in,
                      kernal_mode,
                      tfmode,
                      window_type,
                      scale_mode,
                      _menu->getTfMarginal(),
                      window_length,
                      fft_length,
                      dbthr,
                      (double)_menu->getTfOptLevel());
        else
        fprintf(stderr,"Error: spectraprim_c missing:\n");
        /*
         _num_vector_points = sine_timefreq("spec", 1, 
                      _menu->getSineInterpolation(), 
                      _menu->getTfWindowLength(), 
                      _parent_sp->sampleRate(), nsamp,
                      &_trace_in[trace_in_offset + window_offset_samps],
                      &_amps_out[out_offset * _num_time_freq * nsamp]);
        */ 
        }

      if(_num_vector_points)//Success
       {
       if(_menu->getSpectraType() != SpectraMenu::TIMEFREQ_TYPE)
         {

          //If spct flatten, populate the table
          if(_menu->getSpectraType() != SpectraMenu::TIMEFREQ_TYPE &&
             (strcmp(_menu->getPhaseType(), "FLATTEN") == 0))
              _spectra_table_pop->addRow(trace_count, 0, 
                  (float)_dhd[dhd_offset+SpectraMenu::TIME_SHIFT_HEADER-1],
                  (float)_dhd[dhd_offset+SpectraMenu::AVERAGE_PHASE_HEADER-1],
                  (float)_dhd[dhd_offset+SpectraMenu::TIME_FIRST_HEADER-1]);

         _max_frequency_returned =
                   (_new_sample_rate * _freq_scale) * (_num_vector_points - 1);

         //Display the spectra values in Power or Db options
         if(_menu->getAmplitudeType() != SpectraMenu::LINEAR_AMPLITUDE &&
            _menu->getSpectraType()   == SpectraMenu::AMPLITUDE_TYPE      )
              processPowerOrDb( &_amps_out[out_offset] );

         //Normalize family of spectra
         if(_menu->normalizeFamily() && trace_count == nplt - 1)
           {
           error = createNormalizedVectors(trace_count + 1, _amps_out);
           if(error)
             {
             cleanupArrays();
             return error;
             }
           }
         //Single spectra or non-normalized family of spectra
         else if((_menu->getNumSpectra()  == SpectraMenu::SINGLE_SPECTRA ||
                 _menu->getNumSpectra()  == SpectraMenu::FAMILY_SPECTRA)   &&
                 !_menu->normalizeFamily()  )
           {
           error = createVectors(trace_count, &_amps_out[out_offset], 
                                 &_phase_out[out_offset]);
           if(error)
             {
             cleanupArrays();
             return error;
             }
           }
         //Spectra averaging
         else if(trace_count == nplt - 1)//have all of the data
           {
           error = createAveragedVector(trace_count + 1, 
                                        &_amps_out[0], 
                                        &_phase_out[0]);
           if(error)
             {
             cleanupArrays();
             return error;
             }
           }//end of averaging

         }
       else//Time frequency 
         {
         // Rice takes care of linear and db but not power
         if(kmode != SpectraMenu::TF_SINUSOID) 
           {
           if(_menu->getAmplitudeType() == SpectraMenu::POWER_AMPLITUDE)
              processPowerOrDb( &_amps_out[out_offset * _num_time_freq * nsamp]);
           }
         else//Sinusoid
           {
           if(_menu->getAmplitudeType() != SpectraMenu::LINEAR_AMPLITUDE)
              processPowerOrDb( &_amps_out[out_offset * _num_time_freq * nsamp]);
           }


         //Do a vector type plot if a time frequency slice was requested
         if(strcmp(_menu->getTfMode(),"slice")==0)
           {
           _num_vector_points = _num_time_freq;

           
           float nyquist = 1.0F / (2.0F * _parent_sp->sampleRate());
           _new_sample_rate = nyquist / (_num_time_freq - 1);
           _max_frequency_returned = nyquist;
           _freq_scale = 1.0F;
           
           //We will use the phase_out array to hold the time slice values
           tf_slice = _menu->getTfSlice();           
           slice_index = (int)((tf_slice - _window_begin) / 
                                _parent_sp->sampleRate() );
           for(tfi = 0; tfi < _num_time_freq; tfi++)
             {
             txi = (out_offset * _num_time_freq * nsamp ) +
                   (tfi * nsamp) + slice_index;
             _phase_out[tfi] = _amps_out[txi];
             }

           error = createVectors(trace_count, &_amps_out[out_offset], 
                                 &_phase_out[out_offset]);
           if(error)
             {
             cleanupArrays();
             return error;
             }
           }//end vector type slice time frequency
         else//time frequency color plot
           {
           if(last_tf_trace_created == 0)
             {
             _y1 = _window_begin;
             _y2 = _window_end;
             }
           else
             {
             _y1 = min(_window_begin, _y1);
             _y2 = max(_window_end,   _y2);
             }

           //Fill in the time frequency headers with the parent sp headers
           //Note that we may have several time frequency traces from a single
           //input trace.
           for(m = 0; m < _num_time_freq; m++)
             {
             tf_offset = last_tf_trace_created * _sp->numHeaders();
             for(n = 0; n < _sp->numHeaders(); n++)
               {
               tf_headers[n + tf_offset + m * _sp->numHeaders()] = 
                                                      hd[n + original_hoffset];
               if(n == ANNO_HDR)//transfer parent sp anno to this sp
                  tf_headers[n + tf_offset + m * _sp->numHeaders()] = 
                              hd[_parent_sp->header1() - 1 + original_hoffset];
                
               }
             }
           }
         last_tf_trace_created += _num_time_freq;
         }//end time frequency types
       }//end successful
     else//Analysis unsucessful 
       {
       error = 1;
       }

     trace_count++;

     }//end for j loop of traces in pattern


   n1 += (ndo - 1) + nskp + 1;
   if(n1 < 1 || n1 > max_traces || hit_end > 0) 
      i = ngrps + 1;/*done*/
   n2 = n1 + (ndo - 1);
   }//Done with analysis, (end for i loop of all input traces)



  //Now assign a sequential coordinate header for tf non vector plotting
  if(_menu->getSpectraType() == SpectraMenu::TIMEFREQ_TYPE  &&
     (strcmp(_menu->getTfMode(),"slice") != 0)                 )
    {
    for(m = 0; m < last_tf_trace_created; m++)
      {
      tf_headers[m * _sp->numHeaders() + GRIDX_HDR]=(float)m + 1.0F;
      tf_headers[m * _sp->numHeaders() + GRIDY_HDR] = 0.0F;
      }
    }    
  else
    {
     cleanupArrays();
    }


  //Error if all traces in were dead
  long total_ok = nplt;
  for(i = 0; i < nplt; i++)
    if(_dead_trace_index[i])
      --total_ok;
  if(total_ok < 1)
    error = 1;


  if(!error && notify_of_dead)//Notify of any dead traces
    {
     if(notify_of_dead > 50)
        {
        sprintf(tmpmsg,
                "There are %d more dead traces not listed.\n",
                 notify_of_dead - 50);
        strcat(errmsg,tmpmsg);
        }
    error_pop = new SLErrorPop(topWidget(),"Dead traces ignored",errmsg);
    }

  return error;

}




//Compute the window for the analysis
void SpectraPlot::computeWindow(float *hd, float *beginning, float *end)
{

   if(_menu->getWindowType() == SpectraMenu::FIXED_WINDOW)
     {
     *beginning = _menu->getFixedWindowBeginningTime();
     *end       = _menu->getFixedWindowEndingTime();
     }
   else
     {
     *beginning = (hd[MUTE_HEADER] * _parent_sp->sampleRate())
                   + _menu->getMuteWindowBeginningTime();
     *end       = _menu->getMuteWindowEndingTime() + *beginning;
     }
}


long SpectraPlot::getNumSamplesPerWindow()
{
int num_samples;
float beginning;
float end;

   if(_menu->getWindowType() == SpectraMenu::FIXED_WINDOW)
     {
     beginning = _menu->getFixedWindowBeginningTime();
     end       = _menu->getFixedWindowEndingTime();
     }
   else
     {
     beginning = _menu->getMuteWindowBeginningTime();
     end       = _menu->getMuteWindowEndingTime();
     }

   num_samples = (int) ((end - beginning)  / _parent_sp->sampleRate() + 1.5);

   return num_samples;

}


void SpectraPlot::getMinAndMaxOfData(long n, float *data, float *amp_min,
                                     float *amp_max)
{

  *amp_min = *amp_max = data[0];
  for(long i = 0; i < n; i++)
    {
    *amp_min = (data[i] < *amp_min) ? data[i] : *amp_min;
    *amp_max = (data[i] > *amp_max) ? data[i] : *amp_max;
    }

}





//Do a requested power or db display
void SpectraPlot::processPowerOrDb( float *amps)
{
long i;
int kmode;
char *kernal_mode;

  if(_menu->getSpectraType() == SpectraMenu::TIMEFREQ_TYPE)
    {
    kernal_mode = _menu->getKernal(&kmode);

    if(kmode != SpectraMenu::TF_SINUSOID)//Rice so only need to do power  
      {
      for(i = 0; i < _num_time_freq * getNumSamplesPerWindow(); i++)
        amps[i] = pow(amps[i], 2);
      return;    
      }

    //Next two are sinusoid
    if(_menu->getAmplitudeType() == SpectraMenu::POWER_AMPLITUDE)
      {
      for(i = 0; i < _num_time_freq * getNumSamplesPerWindow(); i++)
        amps[i] = pow(amps[i], 2);
      return;
      }
    else if(_menu->getAmplitudeType() == SpectraMenu::DB_AMPLITUDE)
      {
      for(i = 0; i < _num_time_freq * getNumSamplesPerWindow(); i++)
        {
        if(amps[i] != 0.0F)
          amps[i] = 20 * log10(amps[i]);
        }
      return;
      }
    else
      {
      assert(0);
      }

    }



  //Spct types below
  if(_menu->getAmplitudeType() == SpectraMenu::POWER_AMPLITUDE)
    {
    for(i = 0; i < _num_vector_points; i++)
      amps[i] = pow(amps[i], 2);
    }
  else if(_menu->getAmplitudeType() == SpectraMenu::DB_AMPLITUDE)
    {
    for(i = 0; i < _num_vector_points; i++)
      {
      if(amps[i] != 0.0F)
        amps[i] = 20 * log10(amps[i]);
      }
    }
  else
    {
    assert(0);
    }
  

}


int SpectraPlot::createVectors(long /*index*/, float *amps, float *phase)
{
long i;
int error = 0;


  if(_xdata == NULL)
    {
    _xdata = (float *) calloc( 1, (int)_num_vector_points * sizeof(float)); 
    if(_xdata == NULL)
    return (error = 1);
    }
  else
    {
    _xdata = (float *)realloc(_xdata, (int)_num_vector_points * sizeof(float)); 
    }

  if(_ydata == NULL)
    {
    _ydata = (float *) calloc( 1, (int)_num_vector_points * sizeof(float)); 
    if(_ydata == NULL)
    return (error = 1);
    }
  else
    {
    _ydata = (float *)realloc(_ydata, (int)_num_vector_points * sizeof(float)); 
    }


  //Set the x axis (Hz)
  _x1 = 0.0; //Is this always the first frequency from fortran?
  _x2 = _max_frequency_returned;
  _xdata[0] = 0.0;//Is this always the first frequency from fortran?
  for(i = 1; i < _num_vector_points; i++)
    _xdata[i] = _xdata[i-1] + (_new_sample_rate * _freq_scale);



  //Set the y axis (Amplitudes, Hz, or Time)
  if(_menu->getSpectraType() == SpectraMenu::AMPLITUDE_TYPE)//amp type
    for(i = 0; i < _num_vector_points; i++)
      _ydata[i] = amps[i];
  else //Phase type or time frequency slice
    for(i = 0; i < _num_vector_points; i++)
      _ydata[i] = phase[i];

  if(!_vector_count)//first time thru
    _y1 = _y2 = _ydata[0];
  for(i = 0; i < _num_vector_points; i++)
    {
    _y1 = _ydata[i] < _y1 ? _ydata[i] : _y1;
    _y2 = _ydata[i] > _y2 ? _ydata[i] : _y2;
    }
 
  //Prior to doing a db amplitude the input data may have a 0.0 somewhere
  //in it. The processPowerOrDb method ignores the value so here we want to
  //make it stand out as the smallest value of the display 
  if(_menu->getSpectraType()   == SpectraMenu::AMPLITUDE_TYPE &&
     _menu->getAmplitudeType() == SpectraMenu::DB_AMPLITUDE     ) 
    {
    for(i = 0; i < _num_vector_points; i++)
      {
      if(_ydata[i] == 0.0F)
        {
        //printf("Debug warning zeros found at %d is %f\n",i);
        _ydata[i] = _y1 * 1.05;//Add 5 percent to the low end range 
        _y1 *= 1.05;
        }
      }
    }
 


  //Now do the vector drawing work
  _vect_data = new VectData((int)_num_vector_points, _xdata, _ydata);
  _vector = _vect_ll->add(_vect_data, namedColors[_color_index], 2);
  _vector->makeVisible();
  ++_vector_count;

  ++_color_index;
  if(_color_index > MAX_NAMED_COLORS - 1)//Repeat colors if needed
    _color_index = 0;
  
  return error;

}



int SpectraPlot::createNormalizedVectors(long count, float *amps) 
{
long i, j;
int error = 0;
float lav, scaler;

  

  if(_xdata == NULL)
    {
    _xdata = (float *) calloc( 1, (int)_num_vector_points * sizeof(float)); 
    if(_xdata == NULL)
    return (error = 1);
    }
  else
    {
    _xdata = (float *)realloc(_xdata, (int)_num_vector_points * sizeof(float)); 
    }

  if(_ydata == NULL)
    {
    _ydata = (float *) calloc( 1, (int)_num_vector_points * sizeof(float)); 
    if(_ydata == NULL)
    return (error = 1);
    }
  else
    {
    _ydata = (float *)realloc(_ydata, (int)_num_vector_points * sizeof(float)); 
    }


  for(j = 0; j < count; j++)
    {
    if(_dead_trace_index[j] == 0)//Not a dead trace 
      {
      //Is it correct to assume we always have positive amps?
      lav = amps[j * _num_vector_points];  

      for(i = j * _num_vector_points; i < (j+1) * _num_vector_points; i++)
      lav = lav < amps[i] ? amps[i] : lav;

      scaler = 1.0F / lav;

      for(i = j * _num_vector_points; i < (j+1) * _num_vector_points; i++)
        amps[i] *= scaler;

      //Set the x axis (Hz)
      _x1 = 0.0; //Is this always the first frequency from fortran?
      _x2 = _max_frequency_returned;
      _xdata[0] = 0.0;//Is this always the first frequency from fortran?
      for(i = 1; i < _num_vector_points; i++)
        _xdata[i] = _xdata[i-1] + (_new_sample_rate * _freq_scale);

      //Set the y axis 
      for(i = 0; i < _num_vector_points; i++)
          _ydata[i] = amps[j * _num_vector_points + i];

      if(!_vector_count)//first time thru
        _y1 = _y2 = _ydata[0];

      for(i = 0; i < _num_vector_points; i++)
        {
        _y1 = _ydata[i] < _y1 ? _ydata[i] : _y1;
        _y2 = _ydata[i] > _y2 ? _ydata[i] : _y2;
        }
 
      //Prior to doing a db amplitude the input data may have a 0.0 somewhere
      //in it. The processPowerOrDb method ignores the value so here we want to
      //make it stand out as the smallest value of the display 
      if(_menu->getSpectraType()   == SpectraMenu::AMPLITUDE_TYPE &&
         _menu->getAmplitudeType() == SpectraMenu::DB_AMPLITUDE     ) 
        {
        for(i = 0; i < _num_vector_points; i++)
          {
          if(_ydata[i] == 0.0F)
            {
            printf("Debug warning 0 found at %d is %f\n",i);
            _ydata[i] = _y1 * 1.05;//Add 5 percent to the low end range 
            _y1 *= 1.05;
            }
          }
        }
 

      //Now do the vector drawing work
      _vect_data = new VectData((int)_num_vector_points, _xdata, _ydata);
      _vector = _vect_ll->add(_vect_data, namedColors[_color_index], 2);
      _vector->makeVisible();
      ++_vector_count;

      ++_color_index;
      if(_color_index > MAX_NAMED_COLORS - 1)//Repeat colors if needed
        _color_index = 0;

      }//End not a dead trace if

    }

  return error;

}


int SpectraPlot::createAveragedVector(long nplt, float *amps, float *phase)
{
int m, n, o;
int error = 0;
long total_to_average;

   for(m = 0; m < nplt; m++)
     {
     if(_dead_trace_index[m] == 0)
       {
       for(n = 0; n < _num_vector_points; n++)
         {
         amps[n]  += amps [m * _num_vector_points + n];
         phase[n] += phase[m * _num_vector_points + n];
         }
       }
     }

   total_to_average = nplt;
   for(m = 0; m < nplt; m++)
     if(_dead_trace_index[m])
       --total_to_average;

   if(total_to_average < 1)
      return error = 1;

   for(o = 0; o < _num_vector_points; o++)
     {
     amps[o]  /= (float)total_to_average;
     phase[o] /= (float)total_to_average;
     }

   error = createVectors(0, amps, phase);

   return error;     


}


int  SpectraPlot::plot()
{
int error = 0;
int stat;
long nplt, iskp, ndo, nskp;
float amp_min;
float amp_max;
float top, bottom;
long total_traces;
int user_y_override;

  //Vector type plotting
  if(_menu->getSpectraType() != SpectraMenu::TIMEFREQ_TYPE ||
     (strcmp(_menu->getTfMode(),"slice") == 0)                 )
    {
    //Override y axis is user has requested to use his own y axis instead
    //of the y axis that is currently set to the data. 
    user_y_override = _menu->userSettingDisplayedY(&top, &bottom);
    if(user_y_override)
      {
      _y2 = top;
      _y1 = bottom;
      }
    else
      {
      //Add a little space on the y axis so extreme points dont butt up
      //next to the display y axis. (5%) 
      _y1 *= 1.05;
      _y2 *= 1.05;
      }

    _sp->setPlotType(PlotImage::PlotGRID);
    _sp->setPlotSize(_menu->getPlotWidth(), _menu->getPlotHeight());
    _sp->setManualTransform(True);
    _sp->setManualX1(_menu->getDisplayLowFreq());
    _sp->setManualX2(_menu->getDisplayHiFreq());
    _sp->setManualY1(_y2);
    _sp->setManualY2(_y1);
    _sp->setGridXYS(_menu->getDisplayLowFreq(), _menu->getDisplayHiFreq(),
                    _y2, _y1);
    _sp->setDrawXlines(True);
    _sp->setDrawYlines(True);
    _sp->setGridColor (PaintsetCollection::white(XtScreen(
      _sp->imageGraphic())));
    }
  else//Color fill (time frequency) type
    {
    _color_pop = new SeisColorPop( topWidget(), "Spectra Color", _sp,
                                      getHelpCtx(), True, True);
    _color_pop->make();

    _cbar_pop  = new SeisCbarPop(topWidget(), "Color Bar", 
                                     _sp, getHelpCtx());

    _menu->getTracePattern(&nplt, &iskp, &ndo, &nskp);
    total_traces = _num_time_freq * nplt;
    _sp->setPlotType(PlotImage::PlotARRAY);  
    _sp->setNorm(PlotImage::EXTERNALNORM);
    int nsamp = getNumSamplesPerWindow();
    getMinAndMaxOfData(nsamp * total_traces, _amps_out, &amp_min, &amp_max);
    _color_pop->setAmplitudes(amp_min, amp_max);
    _sp->setDoAmplitude(True);
    _sp->setDoPercent(False);
    _sp->setExternalAmp(amp_max);
    _sp->setMinColorAmp(amp_min);
    _sp->setMaxColorAmp(amp_max);
    _sp->setTI(_menu->getPlotWidth()); 
    _sp->setIS(_menu->getPlotHeight());
    _sp->setGridWidth(_menu->getPlotWidth());
    _sp->setGridHeight(_menu->getPlotHeight());
    _sp->setDrawXlines(False);
    _sp->setDrawYlines(True);
    _sp->setFirstLbl(1L);
    _sp->setLblInc(_num_time_freq);
    _sp->setHeaders(GRIDX_HDR + 1, GRIDY_HDR + 1);
    _sp->setMatchHeader(GRIDX_HDR + 1);
    _sp->setGridXYS(1, total_traces + 1, _y1, _y2);
    _sp->setTminTmax(_y1, _y2);
    _sp->setTimingLines(_parent_sp->primTimingLine(),
                      _parent_sp->secTimingLine()); 
    float srval = (_y2 - _y1) / (float)(max(nsamp - 1.0,1.0)); 
    _sp->setSrval(srval);
    _sp->setManualTransform(False);
    _sp->applySmoother(True);
    }




  //manage();

  _sp->backingStore(True);

  stat = _sp->plot();

  if(stat < 1)
    {
    unmanage();
    error = 1;
    }

  //Label the plot
  if(!error)
    {
    if(_menu->getSpectraType() == SpectraMenu::TIMEFREQ_TYPE &&
      (strcmp(_menu->getTfMode(),"slice") != 0)                  )
        _plot_type = COLOR_TYPE;
   else
        _plot_type = VECTOR_TYPE;

    labelPlot();
    }


  return error;

}


//Draw a representation of the vector colors used if more than 1 vector
void SpectraPlot::drawColorLabels()
{
char color_string[128];
Display *dpy      = _sp->getDisplay();
GC gc             = _sp->getScratchGC();
XColor closest, exact;
int x, y;
int label_loc, i;


  if(_vector_count < 1)
    return;

  strcpy(color_string,"");
      
  x = _sp->leftBorder();
  y = _sp->topBorder() / 2;
  strcpy(color_string,"Input trace colors ->");
  label_loc = strlen(color_string) * _sp->annoFont()->max_bounds.width;
  XSetForeground(dpy, gc, PaintsetCollection::black(DefaultScreenOfDisplay(
    dpy)));
  XDrawString(dpy, _sp->imagePixmap(0), gc, x, y, color_string,
              strlen(color_string));
  
  Paintset *paintset = PaintsetCollection::fetchExisting (_col->cmap);

  _col->cnum = MAX_NAMED_COLORS;

  if (!ColorsetCollection::readOnly(_sp->W())) {
    ColorsetCollection::shareWith (_sp->getColInfo());
    assert (!ColorsetCollection::allocate(_col));
    ColorsetCollection::shareWith ();
  }

  for(i = 0; i < min(_vector_count, MAX_NAMED_COLORS); i++)
    {
/*
    if(XAllocNamedColor(dpy, colormap, namedColors[i], &closest, &exact))
      {
      _pixels[i]= closest.pixel;
      ++_pixel_count;
      }
    else
      {
      _pixels[i] = PaintsetCollection::black (DefaultScreenOfDisplay(dpy));
      }
*/
      _col->pix[i] = paintset->getForegroundPixelFromName (namedColors[i]);

    XSetForeground(dpy, gc, _col->pix[i]);
    x += label_loc;
    sprintf(color_string, "%d ", i + 1);
    label_loc += strlen(color_string);
    XDrawString(dpy, _sp->imagePixmap(0), gc, x, y, color_string, 
                strlen(color_string));
    label_loc = strlen(color_string) * _sp->annoFont()->max_bounds.width;
    }

  if(_vector_count >= MAX_NAMED_COLORS)
    {
    XSetForeground(dpy, gc, PaintsetCollection::black(DefaultScreenOfDisplay(
      dpy)));
    strcpy(color_string,"  Colors repeat...");
    XDrawString(dpy, _sp->imagePixmap(0), gc, 
                x + _sp->annoFont()->max_bounds.width , y, color_string,
                strlen(color_string));
    }


   if(_col->cnum)
     {
/*
      XFreeColors(_sp->getDisplay(), _sp->getColormap(), _pixels, 
                  _pixel_count, 0UL);
*/
      ColorsetCollection::clear (_col);
     }


}







//Draw a representation of the vector colors used if more than 1 vector
void SpectraPlot::markCbytImageTraces()
{
long nplt, iskp, ndo, nskp, num;
long pixel_index, n1, n2, hit_end, ngrps;
long i,j;
Display *dpy      = _parent_sp->getDisplay();
long trace_num, trace_count;
long max_traces       = 0;

 if(_parent_sp->plottedFrames() > 1)
   return;//Need more work in order to post properly when we have movies

  for(i = 0; i < _parent_sp->plottedFrames(); i++)
    max_traces += _parent_sp->displayedTraces(i);


  _menu->getTracePattern(&nplt, &iskp, &ndo, &nskp);

  trace_count = nplt;
  for(i = 0; i < nplt; i++)
    if(_dead_trace_index[i])
      --trace_count;


  if(nplt < 1 || trace_count < 1)
    return;


  

  if(_cbyt_vect_ll != NULL) delete _cbyt_vect_ll;
  _cbyt_vect_ll   = new SeisVectLinkedList();
  _cbyt_vect_ll->addPlot(_parent_sp);
  _cbyt_vect_data = NULL;

  


  if(_cbyt_xdata == NULL)
    {
    _cbyt_xdata = (float *) calloc( 1, (int)4 * sizeof(float)); 
    if(_cbyt_xdata == NULL)
      {
      printf("Could not allocate data to mark cbyt image traces\n");
      return;
      }
    }
  else
    {
    _cbyt_xdata = (float *)realloc(_cbyt_xdata, (int)4 * sizeof(float)); 
    }

  if(_cbyt_ydata == NULL)
    {
    _cbyt_ydata = (float *) calloc( 1, (int)4 * sizeof(float)); 
    if(_cbyt_ydata == NULL)
      {
      printf("Could not allocate data to mark cbyt image traces\n");
      return;
      }
    }
  else
    {
    _cbyt_ydata = (float *)realloc(_cbyt_ydata, (int)4 * sizeof(float)); 
    }

  trace_count = 0;
  pixel_index = 0;
  n1          = iskp + 1;
  n2          = n1 + (ndo - 1);
  hit_end     = 0;
  ngrps       = ((nplt - 1) / ndo) + 1;

  for(i = 1, j = 0; i <= ngrps; i++)
    {
    if(n2 < 1 || n2 > max_traces) 
      hit_end = 1;
    num = ndo;
    num = (nplt - (i - 1) * num < num) ? nplt - (i - 1) * num : num;

    for(j = 0; j < num; j++)
      {
      trace_num = n1 + j;

      if(_dead_trace_index[trace_count] == 0)
        {
        _cbyt_xdata[0] = _cbyt_xdata[1] = (float)trace_num;
        _cbyt_ydata[0] = _parent_sp->plottedTmin();
        _cbyt_ydata[1] = _parent_sp->plottedTmax();
        _cbyt_vect_data = new VectData((int)2, _cbyt_xdata, _cbyt_ydata);
        _cbyt_vector = _cbyt_vect_ll->add(_cbyt_vect_data, 
                                          namedColors[pixel_index], 1);
        _cbyt_vector->makeVisible();
        ++pixel_index;
        if(pixel_index > MAX_NAMED_COLORS - 1)//Repeat colors if needed
           pixel_index = 0;
        }

      ++trace_count;
      }

    n1 += (ndo - 1) + nskp + 1;
    if(n1 < 1 || n1 > max_traces || hit_end > 0) 
       i = ngrps + 1;/*done*/
    n2 = n1 + (ndo - 1);
    }


}



void SpectraPlot::clearCbytImageMarks()
{
  if(_cbyt_vect_ll)
    {
    delete _cbyt_vect_ll;
    _cbyt_vect_ll = NULL;
    }
}

void SpectraPlot::labelPlot()
{
char plot_label_string[2048];
char temp[32];
int mode;  

  strcpy(plot_label_string,"");
  strcpy(temp,"");

  if(_menu->getSpectraType() != SpectraMenu::TIMEFREQ_TYPE)
    {
 
    if(_menu->getWindowType() == SpectraMenu::FIXED_WINDOW)
      strcat(plot_label_string, "Fixed window, ");
    else
      strcat(plot_label_string, "Follow mute, ");

    if(_menu->getNumSpectra() == SpectraMenu::SINGLE_SPECTRA)
      strcat(plot_label_string, "Single spectrum, ");

    if(_menu->getNumSpectra() == SpectraMenu::AVERAGED_SPECTRA)
      strcat(plot_label_string, "Averaged spectra, "); 

    if(_menu->normalizeFamily())
      strcat(plot_label_string, "Normalized spectra, ");

    if(_menu->getSpectraType() == SpectraMenu::PHASE_TYPE)
      {
      if(!strcmp(_menu->getPhaseType(),"NONE"))
        {
        strcat(plot_label_string, "Wrapped Phase, ");
        }
      else if(!strcmp(_menu->getPhaseType(),"UNWRAP"))
        {
        strcat(plot_label_string, "Unwrapped Phase, ");
        sprintf(temp,"Low %d ", (int)(_menu->getUnwrappedLowFreq() + .5));
        strcat(plot_label_string, temp);
        sprintf(temp,"Hi %d, ", (int)(_menu->getUnwrappedHiFreq() + .5));
        strcat(plot_label_string, temp);
        }
      else
        {
        strcat(plot_label_string, "Unwrapped and flattened Phase, ");
        sprintf(temp,"Low %d ", (int)(_menu->getFlattenedLowFreq() + .5) );
        strcat(plot_label_string, temp);
        sprintf(temp,"Hi %d, ", (int)(_menu->getFlattenedHiFreq() + .5) );
        strcat(plot_label_string, temp);
        }
      }

    if(_menu->getSpectraType() == SpectraMenu::AMPLITUDE_TYPE ||
       _menu->getSpectraType() == SpectraMenu::PHASE_TYPE)
      {
      if(_menu->getSpectraType() == SpectraMenu::AMPLITUDE_TYPE)
        {
        if(_menu->getAmplitudeType() == SpectraMenu::LINEAR_AMPLITUDE)
          strcat(plot_label_string, " Linear Amplitude, ");
        else if(_menu->getAmplitudeType() == SpectraMenu::POWER_AMPLITUDE)
          strcat(plot_label_string, " Power Amplitude, ");
        else
          strcat(plot_label_string, " dB Amplitude, ");
        }

      drawColorLabels();

      }
    }
  else//Time Frequency plots
    {
    strcat(plot_label_string,"TIME-FREQUENCY, ");
    _menu->getKernal(&mode);
    if(mode != SpectraMenu::TF_SINUSOID)
      {
      _menu->getTfMode(&mode);
      switch(mode)
        {
        case SpectraMenu::TF_DISTRIBUTION_MODE:
          strcat(plot_label_string,"Distribution, ");
        break;
        case SpectraMenu::TF_DISTRIBUTION_SLICE_MODE:
          strcat(plot_label_string,"Slice, ");
          sprintf(temp, "%8.3f  ", _menu->getTfSlice());
          strcat(plot_label_string, temp);
        break;
        case SpectraMenu::TF_IBAND_MODE:
          strcat(plot_label_string,"Instantaneous Bandwidth, ");
        break;
        case SpectraMenu::TF_ICENTERFREQ_MODE:
          strcat(plot_label_string,"Instantaneous Center Frequency, ");
        break;
        case SpectraMenu::TF_IDOMINANTFREQ_MODE:
          strcat(plot_label_string,"Instantaneous Dominant Frequency, ");
        break;
        case SpectraMenu::TF_IMAXIMALFREQ_MODE:
          strcat(plot_label_string,"Instantaneous Maximal Frequency, ");
        break;
        case SpectraMenu::TF_IQ_MODE:
          strcat(plot_label_string,"Instantaneous Q, ");
        break;
        }
      }
    
    _menu->getKernal(&mode);
    switch(mode)
      {
      case SpectraMenu::TF_ADAPTIVE_OPTIMIZED:
        strcat(plot_label_string,"Adaptive Optimized Kernal, ");
      break;
      case SpectraMenu::TF_SHORT_TIME_FOURIER:
        strcat(plot_label_string,"Short Time Kernal, ");
        strcat(plot_label_string,_menu->getFastWindowType());
      break;
      case SpectraMenu::TF_SINUSOID:
        strcat(plot_label_string,"Sinusoid Extraction, ");
        if(_menu->getSineInterpolation())
          strcat(plot_label_string,"Spline, ");
        else
          strcat(plot_label_string,"Linear, ");
        sprintf(temp, "Window: %8.3f  ", _menu->getTfWindowLength());
        strcat(plot_label_string, temp);
      break;
      }



    _menu->getKernal(&mode);
    if(mode != SpectraMenu::TF_SINUSOID)
      {
      _menu->getTfMarginal(&mode);
      switch(mode)
        {
        case SpectraMenu::TF_NONE_TFMARGINAL:
          strcat(plot_label_string,"Correct Marginal, ");
        break;
        case SpectraMenu::TF_NORMALIZE_TIMEMARGINAL:
          strcat(plot_label_string,"Normalize Time Marginal, ");
        break;
        case SpectraMenu::TF_NORMALIZE_FREQMARGINAL:
          strcat(plot_label_string,"Normalize Frequency Marginal, ");
        break;
        }
      }

    drawColorLabels();

    }//End time frequency labels

   strcat(plot_label_string, _parent_sp->filename());

   //If wanted later to this will cause in the filename area also.
   //_sp->setPlotLabel(plot_label_string, True, True, _sp->leftBorder(),
   //                 _sp->topBorder() + _sp->imageHeight() + 20); 

   setTitle(plot_label_string);

   if(_spectra_table_pop != NULL)
      _spectra_table_pop->setTitle(plot_label_string);

   drawAxisLabels();

   markCbytImageTraces();
}



//===========================================================================
//============= Methods to draw x and y axis labels  ========================
//===========================================================================
void SpectraPlot::drawAxisLabels()
{
int x, y;
int x_vis, y_vis, height, width;
char x_axis_label[25];
char y_axis_label[25];

  if(_menu->getSpectraType() == SpectraMenu::TIMEFREQ_TYPE &&
     strcmp(_menu->getTfMode(),"slice") != 0)
       return;

  strcpy(x_axis_label, "Frequency, in Hz -->");

  if(_menu->getSpectraType() == SpectraMenu::AMPLITUDE_TYPE)
     {
       switch(_menu->getAmplitudeType())
         {
         case SpectraMenu::LINEAR_AMPLITUDE:
           strcpy(y_axis_label, "Amplitude");
         break;
         case SpectraMenu::POWER_AMPLITUDE:
           strcpy(y_axis_label, "Power Spectrum");
         break;
         case SpectraMenu::DB_AMPLITUDE:
           strcpy(y_axis_label, "Amplitude dB");
         break;
         }
     }
  else//phase type
    {
    if(_menu->getSpectraType() == SpectraMenu::TIMEFREQ_TYPE)
      {
      switch(_menu->getAmplitudeType())
         {
         case SpectraMenu::LINEAR_AMPLITUDE:
           strcpy(y_axis_label, "Amplitude");
         break;
         case SpectraMenu::POWER_AMPLITUDE:
           strcpy(y_axis_label, "Power Spectrum");
         break;
         case SpectraMenu::DB_AMPLITUDE:
           strcpy(y_axis_label, "Amplitude dB");
         break;
         }
      }
    else
      {
      strcpy(y_axis_label, "Phase degrees");
      }
    }


  _sp->getClipArea(&x_vis, &y_vis, &width, &height);

  //Do the x axis label
  int hori = XTextWidth (_sp->annoFont(),x_axis_label,
                         (int) strlen(x_axis_label));

  x = (int) (x_vis + (width - hori)/2);
  if (x < (_sp->leftBorder() + 1)) x = _sp->leftBorder() + 1;
  y = (int)(_sp->plottedHeight() - (.3 * _sp->bottomBorder()));

  _sp->drawLabel(x_axis_label, 0, x, y, True, False); 
  //_sp->setExtraXHardcopyAnnotation(True, x_axis_label); later




  //Do the y
  XCharStruct charStruct;
  int direction, font_ascent, font_descent;
  XTextExtents(_sp->annoFont(),  y_axis_label,
		   (int) strlen(y_axis_label),
		   &direction, &font_ascent, &font_descent, &charStruct);
      
  int upOffset    =      (int) (charStruct.ascent*1.4);
  y = (int) (y_vis + height/2 - upOffset*(strlen(y_axis_label))/2);
  if (y < _sp->topBorder() + 5) y = _sp->topBorder() + 5;
      
  x = (int)(_sp->leftBorder() / 2.0 - 30);
  _sp->drawLabel(y_axis_label, 0, x, y, False, False);
  //_sp->setExtraYHardcopyAnnotation(True, _y_axis_label); later


}


void SpectraPlot::prepareForRedisplay()
{
   if(_spectra_table_pop != NULL)
      _spectra_table_pop->deleteAllRows();

   if(_col->cnum)
     {
/*
      XFreeColors(_sp->getDisplay(), _sp->getColormap(), _pixels, 
                  _pixel_count, 0UL);
*/
       ColorsetCollection::clear (_col);
     }

   if(_color_index)
     _color_index = 0;

   if(_cbar_pop) {delete _cbar_pop;  _cbar_pop  = NULL;}
   if(_color_pop){delete _color_pop; _color_pop = NULL;}

   _vector_count = 0;

   if(_vect_ll)
     {
     Vector *vect, *next_vect;
     void   *p;
     for(vect = _vect_ll->top(&p),  next_vect = (Vector *) 0; vect; 
                                                             vect = next_vect)
       {
       next_vect = _vect_ll->next(&p);
       _vect_ll->remove(vect);
       }
     }

   if(_vect_data)     { delete _vect_data;      _vect_data      = NULL;}
   if(_cbyt_vect_ll)  { delete _cbyt_vect_ll;   _cbyt_vect_ll   = NULL; }
   if(_cbyt_vect_data){ delete _cbyt_vect_data; _cbyt_vect_data = NULL;}

   if(_xdata)      {free(_xdata);      _xdata      = NULL;}
   if(_ydata)      {free(_ydata);      _ydata      = NULL;}
   if(_cbyt_xdata) {free(_cbyt_xdata); _cbyt_xdata = NULL;}
   if(_cbyt_ydata) {free(_cbyt_ydata); _cbyt_ydata = NULL;}

   if(_menu->getSpectraType() == SpectraMenu::TIMEFREQ_TYPE &&
      (strcmp(_menu->getTfMode(),"slice") != 0)                  )
        _next_plot_type = COLOR_TYPE;
   else
        _next_plot_type = VECTOR_TYPE;
   

   //Create, manage, or unmanage color pop and color bar buttons
   if(_plot_type == VECTOR_TYPE && _next_plot_type == COLOR_TYPE)
     {
     Widget exists = getWidget(COLOR_POP);
     if(exists)
       {
       XtManageChild(getWidget(COLOR_POP));
       XtManageChild(getWidget(COLOR_BAR));
       }
     else
       {
       addExtraButton("Colors Options", COLOR_POP);
       addExtraButton("Color Bar", COLOR_BAR);
       }
     }
   else if(_plot_type == COLOR_TYPE && _next_plot_type == VECTOR_TYPE)
     {
     XtUnmanageChild(getWidget(COLOR_POP));
     XtUnmanageChild(getWidget(COLOR_BAR));
     }

   cleanupArrays();

  
}


//=============================================================================
//===================Informs and actions area    ==============================
//=============================================================================
void SpectraPlot::newPlot(SeisPlot *sp)
{
  if(sp == _parent_sp)
    clearCbytImageMarks();
}

void SpectraPlot::destroyed(SeisPlot *sp)
{
  if(sp == _parent_sp)
    {
    if(_sp->imageIsDisplayed())
      unmanage();
    delete this;
    }
}



void SpectraPlot::removeButton()
{
  _menu->userRemovedPlot(this);
  unmanage();
  delayDelete();
}


void SpectraPlot::postScan(SeisPlot *sp, SeisPlot::ScanDir /*dir*/)
{
  if(sp == _parent_sp)
    clearCbytImageMarks();
}

void SpectraPlot::mouseOutputUpdate(SeisPlot *sp, float x, float /*y*/)
{
double val;
float nyquist;
float frequency_interval;
int trace_number, scan;
float image_x;
long half_a_trace_width;
float float_set;
int int_set;
float fraction;

  if(sp != _sp)
     return;

  half_a_trace_width = (long)( (float)_sp->traceWidth() / 2.0 );
  image_x = (float)( _sp->plottedGridX1() + 
               (float)(x - _sp->imageXorigin() - half_a_trace_width) *
               _sp->xperPix());
  image_x = max(image_x, min(_sp->plottedGridX1(),_sp->plottedGridX2()));
  image_x = min(image_x, max(_sp->plottedGridX1(),_sp->plottedGridX2()));

  trace_number = (int) (image_x + .5) +
                        ( _sp->currentFrame() * _sp->originalTraces() );


  if ((x>-1.0) && (trace_number > 0) && _num_time_freq > 1) 
    {
    nyquist = 1.0F / (2.0F * _parent_sp->sampleRate());
    frequency_interval = nyquist / _num_time_freq;
    float_set = (float)trace_number / (float)_num_time_freq;
    int_set   = trace_number / _num_time_freq;
    fraction  = float_set - int_set;
    scan      = _num_time_freq * fraction - 1;
    if(scan < 0) scan = _num_time_freq;
    val = scan * frequency_interval;
    _sp->setLocationOutputType(PlotImage::MOUSE_AUX, "Approximate Freq:", val);
    }
  else
    {
    _sp->setLocationOutputType(PlotImage::MOUSE_AUX, "Z Unknown:", 0.0F);
    }
}
