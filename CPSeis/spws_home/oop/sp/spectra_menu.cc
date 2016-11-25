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
#include <Xm/Label.h>
#include <Xm/Separator.h>
#include "sp/spectra_menu.hh"
#include "sp/spectra_plot.hh"
#include "sp/seis_plot.hh"
#include "sl/sl_push_box.hh"
#include "sl/shell_stat_msg.hh"
#include "sl/sl_quest_pop.hh"
#include "sl/sl_prim.hh"
#include "sl/sl_text_box.hh"
#include "sl/sl_error_pop.hh"
#include "sl/sl_radio_box.hh"
#include "sl/sl_push_box.hh"
#include "sl/sl_option_menu.hh"
#include "sl/shell_watch.hh"
#include "sl/sl_tog_box.hh"
#include "named_constants.h"

#include <math.h>



static String  defres[]= {
    "*popup.title:                     Spectrum Analysis Menu",
    "*SLLP_title.labelString:          ATTRIBUTE",
    //".height:                          700",
    //".width:                           700",
    "*tracelab.labelString:           Displayed Traces Selection and Plot Size",
    "*tracelab.fontList:               -*-*-bold-r-*-*-*-180-*-*-p-*-*-*",
    "*npltL.labelString:               Nplt:",
    "*iskpL.labelString:               Iskp:",
    "*ndoL.labelString:                Ndo:",
    "*nskpL.labelString:               Nskp:",
    "*nplt.value:                      1",
    "*iskp.value:                      0",
    "*ndo.value:                       1",
    "*nskp.value:                      0",
    "*plot_widthL.labelString:         Plot Width inches",
    "*plot_heightL.labelString:        Plot Height inches", 
    "*plot_width.value:                7.0",
    "*plot_height.value:               2.0",
    "*replacewindow.labelString:       Replace Last Plot Window",
    "*newwindow.labelString:           Make New Plot Window",
    "*newwindow.set:                   True",
    "*winbeginmuteL.labelString:       Add To Mute Time",
    "*windowlab.labelString:           Windowing Options",
    "*windowlab.fontList:              -*-*-bold-r-*-*-*-180-*-*-p-*-*-*",
    "*fixed_window.labelString:        Fixed--------->",
    "*fixed_window.set:                True",
    "*mute_window.labelString:         Follow Mute--->",
    "*winbeginfixedL.labelString:      Start Window Time",
    "*winbeginmuteL.labelString:       Add To Mute Time",
    "*winendfixedL.labelString:        End Window Time",
    "*winendmuteL.labelString:         Window Length",
    "*taperlengthL.labelString:        Taper Length",
    "*taperlength.value:               .1",
    "*spectralab.labelString:          Spectrum Type",
    "*displaytypelab.labelString:      Display Type",
    "*displaylab.labelString:          Display Options",
    "*displaylab.fontList:             -*-*-bold-r-*-*-*-180-*-*-p-*-*-*",
    "*amplitude_type.labelString:      Amplitude",
    "*amplitude_type.set:              True",
    "*phase_type.labelString:          Phase",
    "*amplitude_and_phase_type.labelString:      Amplitude And Phase",
    "*timefreq_type.labelString:       Time/Frequency",
    "*single_spectra.labelString:      Single Spectrum",
    "*single_spectra.set:              True",
    "*averaged_spectra.labelString:    Averaged Spectra",
    "*family_spectra.labelString:      Family of Spectra",
    "*normalize.labelString:           Norm Family Amps",
    "*normalize.set:                   False",
    "*displayedfreqlab.labelString:    Displayed Limits",
    "*display_low_freqL.labelString:   Low Hz",
    "*display_low_freq.value:          0.0",
    "*display_hi_freqL.labelString:    Hi Hz",
    "*display_hi_freq.value:           250.0",
    "*user_y_tog.labelString:          Override Data Y",
    "*user_y_tog.set:                  False",
    "*user_y_topL.labelString:         Top Y",
    "*user_y_bottomL.labelString:         Bottom Y",
    "*spectraoplab.labelString:        Spectra Operations",
    "*spectraoplab.fontList:           -*-*-bold-r-*-*-*-180-*-*-p-*-*-*",
    "*amplitudetypelab.labelString:    Amplitude Spectrum",
    "*linear.labelString:              Linear",
    "*linear.set:                      True",
    "*power.labelString:               Power",
    "*db.labelString:                  dB",
    "*phasetypelab.labelString:        Phase Spectrum",
    "*wrapped.labelString:             Wrapped",
    "*wrapped.set:                     True",
    "*unwrapped.labelString:           Unwrapped",
    "*unwrapped_flattened.labelString: Unwrapped & Flattened",
    "*freqlimitlab.labelString:        Frequency Limits",
    "*freqmaxL.labelString:            Max Hz",
    "*freqmax.value:                   250.0",
    "*unwrappedlowfreqL.labelString:   Unwrp Low Hz",
    "*unwrappedlowfreq.value:          0.0",
    "*unwrappedhifreqL.labelString:    Unwrp Hi Hz",   
    "*unwrappedhifreq.value:           250.0",
    "*flattenedlowfreqL.labelString:   Flat Low Hz",
    "*flattenedlowfreq.value:          0.0",
    "*flattenedhifreqL.labelString:    Flat Hi Hz",   
    "*flattenedhifreq.value:           250.0",
    "*timefreqlab.labelString:         Time Frequency Options",
    "*timefreqlab.fontList:            -*-*-bold-r-*-*-*-180-*-*-p-*-*-*",
    "*distributionoptlab.labelString:       Distribution Options",
    "*tfdistributionlab.labelString:   Distribution Algorithm",
    "*tfmarginaloptlab.labelString:    Time Frequency Marginal Correction",
    "*tfwindowlengthL.labelString:     Window Length",
    "*tfwindowlength.value:            0.04",
    "*tffftlengthL.labelString:        Fft Length",
    "*tffftlength.value:               0.256",
    "*tfoptlevelL.labelString:         Optimization Level",
    "*tfoptlevel.value:                  2.0",
    "*tfsliceL.labelString:            Time Slice",
    "*tfslice.value:                   0.5",
    "*tfwindowtypelab.labelString:     Short Time Fourier Window",
    "*sineinterpolationlab.labelString:Sinusoid Interpolation",
    NULL};




SpectraMenu::SpectraMenu( Widget              p,
                          char                *name,
                          HelpCtx             hctx, 
                          SeisPlot            *displayed_sp)
                : SLFPopSep(p,name,FP_DOALL,hctx,True,False,True,33),
                  SeisInform(displayed_sp),
                  _first_time(True), _displayed_sp(displayed_sp),
                  _finished_make(False)

{

static SLText traceselectbox[]  = 
  {
    { "nplt",   NULL,  NULL,  SLType_int,   NPLT },
    { "iskp",   NULL,  NULL,  SLType_int,   ISKP },
    { "ndo",    NULL,  NULL,  SLType_int,   NDO },
    { "nskp",   NULL,  NULL,  SLType_int,   NSKP },
  };
traceselectbox[0].target=&_nplt; 
traceselectbox[1].target=&_iskp; 
traceselectbox[2].target=&_ndo; 
traceselectbox[3].target=&_nskp;

static SLText plot_size_box[]  = 
  {
    { "plot_width",  NULL,  NULL,  SLType_float,   PLOT_WIDTH },
    { "plot_height", NULL,  NULL,  SLType_float,   PLOT_HEIGHT },
  };
plot_size_box[0].target=&_plot_width; 
plot_size_box[1].target=&_plot_height; 


static SLRadio windowoptions[]  = 
 {
   { "replacewindow",   REPLACE_WINDOW },
   { "newwindow",       NEW_WINDOW },
 };


static SLRadio winopts[]  = 
 {
   { "fixed_window",   FIXED_WINDOW },
   { "mute_window",    MUTE_WINDOW },
 };

static SLText window_beginning_box[]  = 
  {
    { "winbeginfixed",   NULL,  NULL,  SLType_float,   WINDOW_BEGINNING_FIXED },
    { "winbeginmute",    NULL,  NULL,  SLType_float,   WINDOW_BEGINNING_MUTE },
  };
window_beginning_box[0].target=&_window_beginning_fixed; 
window_beginning_box[1].target=&_window_beginning_mute;

static SLText window_end_box[]  = 
  {
    { "winendfixed",   NULL,  NULL,  SLType_float,   WINDOW_END_FIXED },
    { "winendmute",    NULL,  NULL,  SLType_float,   WINDOW_END_MUTE },
  };
window_end_box[0].target=&_window_end_fixed; 
window_end_box[1].target=&_window_end_mute;

static SLText taper_length_box[]  = 
  {
    { "taperlength",   NULL,  NULL,  SLType_float,   TAPER_LENGTH },
  };
taper_length_box[0].target=&_taper_length; 

static SLRadio spectratypes[]  = 
 {
   { "amplitude_type",          AMPLITUDE_TYPE },
   { "phase_type",              PHASE_TYPE },
   { "amplitude_and_phase_type",AMPLITUDE_AND_PHASE_TYPE },
   { "timefreq_type",           TIMEFREQ_TYPE },
 };

static SLRadio displayselected[]  = 
 {
   { "single_spectra",   SINGLE_SPECTRA },
   { "averaged_spectra", AVERAGED_SPECTRA },
   { "family_spectra",   FAMILY_SPECTRA },
 };

static SLTog normalize[]  = 
   {
     { "normalize", NULL, NORMALIZE },
   };

static SLText displayfreqbox[]  = 
  {
    { "display_low_freq", NULL,  NULL,  SLType_float,   DISPLAY_LOW_FREQ },
    { "display_hi_freq",  NULL,  NULL,  SLType_float,   DISPLAY_HI_FREQ },
  };
displayfreqbox[0].target=&_display_low_freq;
displayfreqbox[1].target=&_display_hi_freq; 


static SLTog user_y_tog[]  = 
   {
     { "user_y_tog", NULL, USER_Y },
   };


static SLText userybox[]  = 
  {
    { "user_y_top",     NULL,  NULL,  SLType_float,   USER_Y_TOP },
    { "user_y_bottom",  NULL,  NULL,  SLType_float,   USER_Y_BOTTOM },
  };
userybox[0].target=&_user_y_top;
userybox[1].target=&_user_y_bottom; 


static SLRadio amplitudetypes[]  = 
 {
   { "linear",   LINEAR_AMPLITUDE },
   { "power",    POWER_AMPLITUDE },
   { "db",       DB_AMPLITUDE },
 };

static SLRadio phasetypes[]  = 
 {
   { "wrapped",    WRAPPED },
   { "unwrapped",  UNWRAPPED },
   { "unwrapped_flattened",    UNWRAPPED_FLATTENED },
 };

static SLText freqlimits[]  = 
  {
    { "freqmax",          NULL, NULL,  SLType_float,   FREQ_MAX },
    { "unwrappedlowfreq", NULL, NULL,  SLType_float,   UNWRAPPED_LOW_FREQ },
    { "unwrappedhifreq",  NULL, NULL,  SLType_float,   UNWRAPPED_HI_FREQ },
    { "flattenedlowfreq", NULL, NULL,  SLType_float,   FLATTENED_LOW_FREQ },
    { "flattenedhifreq",  NULL, NULL,  SLType_float,   FLATTENED_HI_FREQ },
  };
freqlimits[0].target = &_freq_max;
freqlimits[1].target = &_unwrapped_low_freq; 
freqlimits[2].target = &_unwrapped_hi_freq;
freqlimits[3].target = &_flattened_low_freq; 
freqlimits[4].target = &_flattened_hi_freq;


static SLPush tf_distribution_opts[] = {
  {"Adaptive Optimized Kernal",       TF_ADAPTIVE_OPTIMIZED},
  {"Short Time Fourier",              TF_SHORT_TIME_FOURIER},
  {"Sinusoid Extraction",             TF_SINUSOID}
};


static SLPush tf_mode_options[] = {
  {"Time Freq Distribution",      TF_DISTRIBUTION_MODE},
  {"Time Freq Distribution Slice",TF_DISTRIBUTION_SLICE_MODE},
  {"Instantaneous Bandwidth",     TF_IBAND_MODE},
  {"Instantaneous Center Freq",   TF_ICENTERFREQ_MODE},
  {"Instantaneous Dominant Freq", TF_IDOMINANTFREQ_MODE},
  {"Instantaneous Maximal Freq",  TF_IMAXIMALFREQ_MODE},
  {"Instantaneous Q",             TF_IQ_MODE}
};

static SLPush tf_marginal_options[] = {
  {"None",                            TF_NONE_TFMARGINAL},
  {"Normalize Time Marginal",         TF_NORMALIZE_TIMEMARGINAL},
  {"Normalize Frequency Marginal",    TF_NORMALIZE_FREQMARGINAL}
};

static SLText tftextparams[]  = 
  {
    { "tfwindowlength",   NULL,  NULL,  SLType_float, TF_WINDOW_LENGTH  },
    { "tffftlength",      NULL,  NULL,  SLType_float, TF_FFT_LENGTH },
    { "tfoptlevel",       NULL,  NULL,  SLType_float, TF_OPT_LEVEL },
    { "tfslice",          NULL,  NULL,  SLType_float, TF_SLICE }
  };
tftextparams[0].target=&_tf_window_length; 
tftextparams[1].target=&_tf_fft_length; 
tftextparams[2].target=&_tf_opt_level;
tftextparams[3].target=&_tf_slice;


static SLPush tfshortwindowparams[] = {
  {"Hamming",       TF_HAMMING},
  {"Rectangular",   TF_RECTANGULAR},
  {"Hanning",       TF_HANNING},
  {"Blackman",      TF_BLACKMAN},
  {"Bartlet",       TF_BARTLET}
};


static SLPush sineparams[] = {
  {"Linear",   TF_LINEAR},
  {"Spline",   TF_SPLINE},
};


  setDefaultResources( p, name, defres);

  

  _trace_select_box = new SLTextBox( this,"trace_select_box",getHelpCtx(),
                           traceselectbox,XtNumber(traceselectbox), True,
                           1, True, False);

  
  _plot_size_box = new SLTextBox( this,"plot_size_box",
                           getHelpCtx(),
                           plot_size_box,
                           XtNumber(plot_size_box), True, 1, 
                           True, False);

  _window_option_box  = new SLRadioBox(this, "window_option_box",getHelpCtx(),
                           windowoptions, XtNumber(windowoptions),
                           NULL, True, False );


  _window_type_box  = new SLRadioBox(this, "window_type_box",getHelpCtx(),
                           winopts, XtNumber(winopts),
                           NULL, True, False );

  _window_beginning_box = new SLTextBox( this,"window_beginning_box",
                           getHelpCtx(),
                           window_beginning_box,
                           XtNumber(window_beginning_box), True, 1, 
                           False, False);
  
  _window_end_box = new SLTextBox( this,"window_end_box",
                           getHelpCtx(),
                           window_end_box,
                           XtNumber(window_end_box), True, 1, 
                           False, False);
  
  _taper_length_box = new SLTextBox( this,"taper_length_box",
                           getHelpCtx(),
                           taper_length_box,
                           XtNumber(taper_length_box), True, 1, 
                           False, False);

  _spectra_type_box  = new SLRadioBox(this, "spectra_type_box",getHelpCtx(),
                           spectratypes, XtNumber(spectratypes),
                           NULL, True, False );

  _display_selected_box  = new SLRadioBox(this, "display_selected_box",
                           getHelpCtx(),
                           displayselected, XtNumber(displayselected),
                           NULL, True, False );

  _normalize_box = new SLTogBox(this, "normalize_box",getHelpCtx(),normalize,
                            XtNumber(normalize), False, False, False );  


  _display_freq_box = new SLTextBox( this,"display_freq_box",
                           getHelpCtx(),
                           displayfreqbox,
                           XtNumber(displayfreqbox), True, 1, 
                           True, False);

  _user_y_tog = new SLTogBox(this, "user_y_tog",getHelpCtx(),user_y_tog,
                            XtNumber(user_y_tog), False, False, False );


  _user_y_box = new SLTextBox( this,"user_y_box",
                           getHelpCtx(),
                           userybox,
                           XtNumber(userybox), True, 1, 
                           True, False);

  _amplitude_type_box  = new SLRadioBox(this, "amplitude_type_box",getHelpCtx(),
                           amplitudetypes, XtNumber(amplitudetypes),
                           NULL, True, False );

  _phase_type_box  = new SLRadioBox(this, "phase_type_box",getHelpCtx(),
                           phasetypes, XtNumber(phasetypes),
                           NULL, True, False );

  _freq_limit_box = new SLTextBox( this,"unwrapped_freq_box",
                           getHelpCtx(),
                           freqlimits,
                           XtNumber(freqlimits), True, 1, 
                           True, False);

  _tf_distribution_options = new SLOptionMenu (this, "tf_distribution_options",
                           hctx, tf_distribution_opts, 
                           XtNumber(tf_distribution_opts));

  _tf_mode_options = new SLOptionMenu (this, "tf_mode_options",
                           hctx, tf_mode_options, XtNumber(tf_mode_options));

  _tf_marginal_options = new SLOptionMenu (this, "tf_marginal_options",
                           hctx, tf_marginal_options,
                           XtNumber(tf_marginal_options));

  _tf_text_params      = new SLTextBox( this,"tf_text_params",getHelpCtx(),
                           tftextparams,XtNumber(tftextparams), True,
                           1L, False, False);
  
  _tf_short_time_window = new SLOptionMenu (this, "tf_short_time_win",
                           hctx, tfshortwindowparams,
                           XtNumber(tfshortwindowparams));
 

  _tf_sine_interpolation = new SLOptionMenu (this, "tf_sine_interpolation",
                           hctx, sineparams,
                           XtNumber(sineparams));



  setComplexNotify(this);

  make(p);

  _plot_array = new SpectraPlot*[MAX_STORED_PLOTS];

  for(int i = 0; i < MAX_STORED_PLOTS; i++)
     _plot_array[i] = NULL;

}




SpectraMenu::~SpectraMenu()
{
  delete _plot_array;
}



Widget SpectraMenu::make(Widget p)
{

   if ( made() ) return topWidget();

   Widget parent = p ? p : wParent();
   ShellStatMsg bld_info(parent, "Building Spectra Menu...");

   SLFPopSep::make(p);

   _freq_limit_box->SetSensitive(FREQ_MAX,           True);
   _freq_limit_box->SetSensitive(UNWRAPPED_LOW_FREQ, False);
   _freq_limit_box->SetSensitive(UNWRAPPED_HI_FREQ,  False);
   _freq_limit_box->SetSensitive(FLATTENED_LOW_FREQ, False);
   _freq_limit_box->SetSensitive(FLATTENED_HI_FREQ,  False);

   _user_y_box->SetSensitive(USER_Y_TOP,    False);
   _user_y_box->SetSensitive(USER_Y_BOTTOM, False);


   //Trace selection area-----------------------------------------------
   Widget tracelab= XtVaCreateManagedWidget( "tracelab",
                                 xmLabelWidgetClass, topWidget(),
                                 XmNtopAttachment,   XmATTACH_FORM,
                                 XmNtopOffset,       5,
                                 XmNleftAttachment,  XmATTACH_FORM,
                                 XmNleftOffset,      125,
                                 NULL);

   XtVaSetValues( _trace_select_box->W(), 
                                 XmNtopAttachment,    XmATTACH_WIDGET,
                                 XmNtopWidget,        tracelab,
                                 XmNtopOffset,        5,
                                 XmNleftAttachment,   XmATTACH_FORM,
                                 XmNleftOffset,       10, 
                                 NULL);

   XtVaSetValues( _plot_size_box->W(), 
                                 XmNtopAttachment,    XmATTACH_WIDGET,
                                 XmNtopWidget,        tracelab,
                                 XmNtopOffset,        5,
                                 XmNleftAttachment,   XmATTACH_WIDGET,
                                 XmNleftWidget,       _trace_select_box->W(),
                                 XmNleftOffset,       250, 
                                 XmNrightAttachment,  XmATTACH_FORM,
                                 XmNrightOffset,      10,
                                 NULL);

   XtVaSetValues( _window_option_box->W(), 
                                 XmNtopAttachment,    XmATTACH_WIDGET,
                                 XmNtopWidget,        _plot_size_box->W(),
                                 XmNtopOffset,        5,
                                 XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                                 XmNleftWidget,       _plot_size_box->W(),
                                 XmNleftOffset,       0, 
                                 XmNrightAttachment,  XmATTACH_FORM,
                                 XmNrightOffset,      10,
                                 NULL);
   



   Widget sep1 = XtVaCreateManagedWidget("sep1", xmSeparatorWidgetClass,
                                 topWidget(),
                                 XmNtopAttachment,    XmATTACH_WIDGET,
                                 XmNtopWidget,        _window_option_box->W(),
                                 XmNtopOffset,        5,
                                 XmNleftAttachment,   XmATTACH_FORM,
                                 XmNrightAttachment,  XmATTACH_FORM,
                                 NULL);


   //Window options area------------------------------------------------
   Widget windowlab= XtVaCreateManagedWidget( "windowlab",
                                 xmLabelWidgetClass, topWidget(),
                                 XmNtopAttachment,   XmATTACH_WIDGET,
                                 XmNtopWidget,       sep1,
                                 XmNtopOffset,       5,
                                 XmNleftAttachment,  XmATTACH_FORM,
                                 XmNleftOffset,      220,
                                 NULL);

   XtVaSetValues( _window_type_box->W(), 
                                 XmNtopAttachment,    XmATTACH_WIDGET,
                                 XmNtopWidget,        windowlab,
                                 XmNtopOffset,        5,
                                 XmNleftAttachment,   XmATTACH_FORM,
                                 XmNleftOffset,       10, 
                                 NULL);

   XtVaSetValues( _window_beginning_box->W(), 
                                 XmNtopAttachment,    XmATTACH_WIDGET,
                                 XmNtopWidget,        windowlab,
                                 XmNtopOffset,        5,
                                 XmNleftAttachment,   XmATTACH_WIDGET,
                                 XmNleftWidget,       _window_type_box->W(), 
                                 XmNleftOffset,       5, 
                                 NULL);
   _window_beginning_box->SetRange(WINDOW_BEGINNING_FIXED, -9999999.0F, 
                                 9999999.0F, 0.000F, 3);
   _window_beginning_box->SetRange(WINDOW_BEGINNING_MUTE, -9999999.0F, 
                                 9999999.0F, 0.000F, 3);


   XtVaSetValues( _window_end_box->W(), 
                                 XmNtopAttachment,    XmATTACH_WIDGET,
                                 XmNtopWidget,        windowlab,
                                 XmNtopOffset,        5,
                                 XmNleftAttachment,   XmATTACH_WIDGET,
                                 XmNleftWidget,     _window_beginning_box->W(), 
                                 XmNleftOffset,       5, 
                                 NULL);
   _window_end_box->SetRange(WINDOW_END_FIXED, -9999999.0F, 
                                 9999999.0F, 0.001F, 3);
   _window_end_box->SetRange(WINDOW_END_MUTE, 0.001F, 
                                 9999999.0F, 1.0F, 3);


   XtVaSetValues( _taper_length_box->W(), 
                                 XmNtopAttachment,    XmATTACH_WIDGET,
                                 XmNtopWidget,      _window_beginning_box->W(),
                                 XmNtopOffset,        10,
                                 XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                                 XmNleftWidget,     _window_beginning_box->W(), 
                                 XmNleftOffset,       50, 
                                 NULL);
   _taper_length_box->SetRange ((int)TAPER_LENGTH, (double)0.001,
     (double)9999999.0, (double)0.001, (long)3);


   Widget sep2 = XtVaCreateManagedWidget("sep2", xmSeparatorWidgetClass,
                                 topWidget(),
                                 XmNtopAttachment,    XmATTACH_WIDGET,
                                 XmNtopWidget,        _taper_length_box->W(),
                                 XmNtopOffset,        5,
                                 XmNleftAttachment,   XmATTACH_FORM,
                                 XmNrightAttachment,  XmATTACH_FORM,
                                 NULL);

   //Display options area ----------------------------------------------------
   Widget displaylab= XtVaCreateManagedWidget( "displaylab",
                                 xmLabelWidgetClass, topWidget(),
                                 XmNtopAttachment,   XmATTACH_WIDGET,
                                 XmNtopWidget,       sep2,
                                 XmNtopOffset,       5,
                                 XmNleftAttachment,  XmATTACH_FORM,
                                 XmNleftOffset,      250,
                                 NULL);

   Widget spectralab= XtVaCreateManagedWidget( "spectralab",
                                 xmLabelWidgetClass, topWidget(),
                                 XmNtopAttachment,   XmATTACH_WIDGET,
                                 XmNtopWidget,       displaylab,
                                 XmNtopOffset,       5,
                                 XmNleftAttachment,  XmATTACH_FORM,
                                 XmNleftOffset,      30,
                                 NULL);

   XtVaSetValues( _spectra_type_box->W(), 
                                 XmNtopAttachment,    XmATTACH_WIDGET,
                                 XmNtopWidget,        spectralab,
                                 XmNtopOffset,        5,
                                 XmNleftAttachment,   XmATTACH_FORM,
                                 XmNleftOffset,       10, 
                                 NULL); 

   Widget displaytypelab= XtVaCreateManagedWidget( "displaytypelab",
                                 xmLabelWidgetClass, topWidget(),
                                 XmNtopAttachment,   XmATTACH_WIDGET,
                                 XmNtopWidget,       displaylab,
                                 XmNtopOffset,       5,
                                 XmNleftAttachment,  XmATTACH_WIDGET,
                                 XmNleftWidget,      spectralab,
                                 XmNleftOffset,      155,
                                 NULL);

   XtVaSetValues( _display_selected_box->W(), 
                                 XmNtopAttachment,    XmATTACH_WIDGET,
                                 XmNtopWidget,        displaytypelab,
                                 XmNtopOffset,        5,
                                 XmNleftAttachment,   XmATTACH_WIDGET,
                                 XmNleftWidget,       _spectra_type_box->W(),
                                 XmNleftOffset,       105, 
                                 NULL);

    XtVaSetValues( _normalize_box->W(), 
                                 XmNtopAttachment,    XmATTACH_WIDGET,
                                 XmNtopWidget,       _display_selected_box->W(),
                                 XmNtopOffset,        5,
                                 XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                                 XmNleftWidget,      _display_selected_box->W(),
                                 XmNleftOffset,       0, 
                                 NULL);


    Widget displayedfreqlab= XtVaCreateManagedWidget( "displayedfreqlab",
                                 xmLabelWidgetClass, topWidget(),
                                 XmNtopAttachment,   XmATTACH_WIDGET,
                                 XmNtopWidget,       displaylab,
                                 XmNtopOffset,       5,
                                 XmNleftAttachment,  XmATTACH_WIDGET,
                                 XmNleftWidget,     _display_selected_box->W(), 
                                 XmNleftOffset,      120,
                                 NULL);


   XtVaSetValues( _display_freq_box->W(), 
                                 XmNtopAttachment,    XmATTACH_WIDGET,
                                 XmNtopWidget,        displaytypelab,
                                 XmNtopOffset,        10,
                                 XmNleftAttachment,   XmATTACH_WIDGET,
                                 XmNleftWidget,      _display_selected_box->W(),
                                 XmNleftOffset,       110, 
                                 NULL);

   XtVaSetValues( _user_y_tog->W(), 
                                 XmNtopAttachment,    XmATTACH_WIDGET,
                                 XmNtopWidget,       _display_freq_box->W(),
                                 XmNtopOffset,        5,
                                 XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                                 XmNleftWidget,      _display_freq_box->W(),
                                 XmNleftOffset,       10, 
                                 NULL);

   XtVaSetValues( _user_y_box->W(), 
                                 XmNtopAttachment,    XmATTACH_WIDGET,
                                 XmNtopWidget,        _user_y_tog->W(),
                                 XmNtopOffset,        0,
                                 XmNleftAttachment,   XmATTACH_WIDGET,
                                 XmNleftWidget,      _display_selected_box->W(),
                                 XmNleftOffset,       110, 
                                 NULL);



   //Spectra operations area---------------------------------------------------
   Widget sep3 = XtVaCreateManagedWidget("sep3", xmSeparatorWidgetClass,
                                 topWidget(),
                                 XmNtopAttachment,    XmATTACH_WIDGET,
                                 XmNtopWidget,        _user_y_box->W(),
                                 XmNtopOffset,        5,
                                 XmNleftAttachment,   XmATTACH_FORM,
                                 XmNrightAttachment,  XmATTACH_FORM,
                                 NULL);

   Widget spectraoplab= XtVaCreateManagedWidget( "spectraoplab",
                                 xmLabelWidgetClass, topWidget(),
                                 XmNtopAttachment,   XmATTACH_WIDGET,
                                 XmNtopWidget,       sep3,
                                 XmNtopOffset,       5,
                                 XmNleftAttachment,  XmATTACH_FORM,
                                 XmNleftOffset,      230,
                                 NULL);

   Widget amplitudetypelab= XtVaCreateManagedWidget( "amplitudetypelab",
                                 xmLabelWidgetClass, topWidget(),
                                 XmNtopAttachment,   XmATTACH_WIDGET,
                                 XmNtopWidget,       spectraoplab,
                                 XmNtopOffset,       5,
                                 XmNleftAttachment,  XmATTACH_FORM,
                                 XmNleftOffset,      30,
                                 NULL);

   XtVaSetValues( _amplitude_type_box->W(), 
                                 XmNtopAttachment,    XmATTACH_WIDGET,
                                 XmNtopWidget,        amplitudetypelab,
                                 XmNtopOffset,        5,
                                 XmNleftAttachment,   XmATTACH_FORM,
                                 XmNleftOffset,       60, 
                                 NULL);

   Widget phasetypelab= XtVaCreateManagedWidget( "phasetypelab",
                                 xmLabelWidgetClass, topWidget(),
                                 XmNtopAttachment,   XmATTACH_WIDGET,
                                 XmNtopWidget,       spectraoplab,
                                 XmNtopOffset,       5,
                                 XmNleftAttachment,  XmATTACH_WIDGET,
                                 XmNleftWidget,      _amplitude_type_box->W(),
                                 XmNleftOffset,      150,
                                 NULL);

   XtVaSetValues( _phase_type_box->W(), 
                                 XmNtopAttachment,    XmATTACH_WIDGET,
                                 XmNtopWidget,        phasetypelab,
                                 XmNtopOffset,        5,
                                 XmNleftAttachment,   XmATTACH_WIDGET,
                                 XmNleftWidget,      _amplitude_type_box->W(), 
                                 XmNleftOffset,       105, 
                                 NULL);
   XtSetSensitive(_phase_type_box->W(),     False);


   Widget freqlimitlab= XtVaCreateManagedWidget( "freqlimitlab",
                                 xmLabelWidgetClass, topWidget(),
                                 XmNtopAttachment,   XmATTACH_WIDGET,
                                 XmNtopWidget,       spectraoplab,
                                 XmNtopOffset,       5,
                                 XmNleftAttachment,  XmATTACH_WIDGET,
                                 XmNleftWidget,      _phase_type_box->W(),
                                 XmNleftOffset,      90,
                                 NULL);

   XtVaSetValues( _freq_limit_box->W(), 
                                 XmNtopAttachment,    XmATTACH_WIDGET,
                                 XmNtopWidget,        freqlimitlab,
                                 XmNtopOffset,        5,
                                 XmNleftAttachment,   XmATTACH_WIDGET,
                                 XmNleftWidget,       _phase_type_box->W(), 
                                 XmNleftOffset,       45, 
                                 NULL);


   //Time frequency area---------------------------------------------------
   Widget sep4 = XtVaCreateManagedWidget("sep4", xmSeparatorWidgetClass,
                                 topWidget(),
                                 XmNtopAttachment,    XmATTACH_WIDGET,
                                 XmNtopWidget,        _freq_limit_box->W(),
                                 XmNtopOffset,        5,
                                 XmNleftAttachment,   XmATTACH_FORM,
                                 XmNrightAttachment,  XmATTACH_FORM,
                                 NULL);

   Widget timefreqlab= XtVaCreateManagedWidget( "timefreqlab",
                                 xmLabelWidgetClass, topWidget(),
                                 XmNtopAttachment,   XmATTACH_WIDGET,
                                 XmNtopWidget,       sep4,
                                 XmNtopOffset,       5,
                                 XmNleftAttachment,  XmATTACH_FORM,
                                 XmNleftOffset,      200,
                                 NULL);
   
   Widget tfdistributionlab= XtVaCreateManagedWidget( "tfdistributionlab",
                                 xmLabelWidgetClass, topWidget(),
                                 XmNtopAttachment,   XmATTACH_WIDGET,
                                 XmNtopWidget,       timefreqlab,
                                 XmNtopOffset,       5,
                                 XmNleftAttachment,  XmATTACH_FORM,
                                 XmNleftOffset,      20,
                                 NULL);

   XtVaSetValues( _tf_distribution_options->W(), 
                                 XmNtopAttachment,    XmATTACH_WIDGET,
                                 XmNtopWidget,        tfdistributionlab,
                                 XmNtopOffset,        2,
                                 XmNleftAttachment,   XmATTACH_FORM,
                                 XmNleftOffset,       10, 
                                 NULL);

   Widget distributionoptlab= XtVaCreateManagedWidget( "distributionoptlab",
                                 xmLabelWidgetClass, topWidget(),
                                 XmNtopAttachment,   XmATTACH_WIDGET,
                                 XmNtopWidget,   _tf_distribution_options->W(),
                                 XmNtopOffset,       5,
                                 XmNleftAttachment,  XmATTACH_FORM,
                                 XmNleftOffset,      20,
                                 NULL);
   
   XtVaSetValues( _tf_mode_options->W(), 
                                 XmNtopAttachment,    XmATTACH_WIDGET,
                                 XmNtopWidget,        distributionoptlab,
                                 XmNtopOffset,        2,
                                 XmNleftAttachment,   XmATTACH_FORM,
                                 XmNleftOffset,       10, 
                                 NULL);


   Widget tfmarginaloptlab =  XtVaCreateManagedWidget( "tfmarginaloptlab",
                                 xmLabelWidgetClass,  topWidget(),
                                 XmNtopAttachment,    XmATTACH_WIDGET,
                                 XmNtopWidget,        _tf_mode_options->W(),
                                 XmNtopOffset,        5,
                                 XmNleftAttachment,   XmATTACH_FORM,
                                 XmNleftOffset,       20,
                                 NULL);

   XtVaSetValues( _tf_marginal_options->W(), 
                                 XmNtopAttachment,    XmATTACH_WIDGET,
                                 XmNtopWidget,        tfmarginaloptlab,
                                 XmNtopOffset,        2,
                                 XmNleftAttachment,   XmATTACH_FORM,
                                 XmNleftOffset,       10, 
                                 NULL);

   Widget sep= make_attached_sep(topWidget(), "sep");
   XtVaSetValues( sep, 
                                XmNtopAttachment,     XmATTACH_WIDGET,
                                XmNtopWidget,         _tf_marginal_options->W(),
                                XmNtopOffset,         25, NULL);


   _information= XtVaCreateManagedWidget( "information", xmLabelWidgetClass, 
                                 topWidget(),
                                 XmNleftAttachment,  XmATTACH_FORM,
                                 XmNleftOffset,      5,
                                 XmNrightAttachment, XmATTACH_FORM, 
                                 XmNrightOffset,     5,
                                 XmNtopAttachment,   XmATTACH_WIDGET,
                                 XmNtopWidget,       sep,
                                 XmNtopOffset,       5,
                                 NULL);
   wprocShowMsg(_information,"   ");

   Widget tmp=  XtVaCreateManagedWidget("", xmLabelWidgetClass, topWidget(),
                                XmNtopAttachment,    XmATTACH_WIDGET,
                                XmNtopWidget,        _information,
                                XmNbottomAttachment, XmATTACH_WIDGET,
                                XmNbottomWidget,     bottomSeparator(),
                                XmNleftAttachment,   XmATTACH_FORM,
                                XmNleftOffset,       5,
                                XmNtopOffset,        5,
                                XmNbottomOffset,     25,
                                NULL);
   
   

   XtVaSetValues( _tf_text_params->W(), 
                                 XmNtopAttachment,    XmATTACH_WIDGET,
                                 XmNtopWidget,        timefreqlab,
                                 XmNtopOffset,        5,
                                 XmNrightAttachment,  XmATTACH_FORM,
                                 XmNrightOffset,      10, 
                                 NULL);
   _tf_text_params->SetRange(TF_WINDOW_LENGTH,-9999999.0F,9999999.0F, 0.000F,3);
   _tf_text_params->SetRange(TF_FFT_LENGTH,   -9999999.0F,9999999.0F, 0.000F,3);
   _tf_text_params->SetRange(TF_SLICE,        -9999999.0F,9999999.0F, 0.000F,3);


  

   XtVaSetValues( _tf_short_time_window->W(), 
                                 XmNtopAttachment,    XmATTACH_WIDGET,
                                 XmNtopWidget,        _tf_text_params->W(),
                                 XmNtopOffset,        0,
                                 XmNrightAttachment,  XmATTACH_OPPOSITE_WIDGET,
                                 XmNrightWidget,      _tf_text_params->W(),
                                 XmNrightOffset,      5, 
                                 NULL);

   Widget tfwindowtypelab =  XtVaCreateManagedWidget( "tfwindowtypelab",
                                 xmLabelWidgetClass,  topWidget(),
                                 XmNtopAttachment,    XmATTACH_WIDGET,
                                 XmNtopWidget,        _tf_text_params->W(), 
                                 XmNtopOffset,        0,
                                 XmNrightAttachment,  XmATTACH_WIDGET,
                                 XmNrightWidget,     _tf_short_time_window->W(),
                                 XmNrightOffset,      20,
                                 NULL);

   XtVaSetValues( _tf_sine_interpolation->W(), 
                                 XmNtopAttachment,    XmATTACH_WIDGET,
                                 XmNtopWidget,       _tf_short_time_window->W(),
                                 XmNtopOffset,        5,
                                 XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                                 XmNleftWidget,      _tf_short_time_window->W(),
                                 XmNrightOffset,      5, 
                                 NULL);


   Widget sineinterpolationlab =  XtVaCreateManagedWidget( 
                                 "sineinterpolationlab",
                                 xmLabelWidgetClass,  topWidget(),
                                 XmNtopAttachment,    XmATTACH_WIDGET,
                                 XmNtopWidget,        tfwindowtypelab,
                                 XmNtopOffset,        8,
                                 XmNrightAttachment,  XmATTACH_WIDGET,
                                 XmNrightWidget,    _tf_sine_interpolation->W(),
                                 XmNrightOffset,      20,
                                 NULL);


   setTitle("Spectra Analysis Menu");

   //Turn off ok button being called on carriage returns
   defaultButton(FP_OK, False);

   _finished_make = True;

   return topWidget();

}


void SpectraMenu::initValues()
{
float nyquist = 1.0F / (2.0F * _displayed_sp->sampleRate());

  _nplt               = 1;
  _iskp               = 0;
  _ndo                = 1;
  _nskp               = 0;
  _plot_width         = 7.0;
  _plot_height        = 2.0;
  _taper_length       = .10 + _displayed_sp->sampleRate();
  _display_low_freq   = 0.0;
  _display_hi_freq    = nyquist;
  _freq_max           = nyquist;
  _unwrapped_low_freq = 0.0;
  _unwrapped_hi_freq  = nyquist;
  _flattened_low_freq = 0.0;
  _flattened_hi_freq  = nyquist;
  

  _window_beginning_box->SetValue(WINDOW_BEGINNING_FIXED, 
                                  (float)_displayed_sp->plottedTmin());
  _window_end_box->SetValue(WINDOW_END_FIXED, 
                                  (float)_displayed_sp->plottedTmax());

  _window_beginning_box->SetValue(WINDOW_BEGINNING_MUTE, 0.0F);
  _window_end_box->SetValue(WINDOW_END_MUTE, 
                                  (float)_displayed_sp->plottedTmax());



  _taper_length_box->SetValue(TAPER_LENGTH,        (float)_taper_length);
  _display_freq_box->SetValue(DISPLAY_LOW_FREQ,    (float)_display_low_freq);
  _display_freq_box->SetValue(DISPLAY_HI_FREQ,     (float)_display_hi_freq);
  _freq_limit_box->  SetValue(FREQ_MAX ,           (float)_freq_max);
  _freq_limit_box->  SetValue(UNWRAPPED_LOW_FREQ,  (float)_unwrapped_low_freq);
  _freq_limit_box->  SetValue(UNWRAPPED_HI_FREQ,   (float)_unwrapped_hi_freq);
  _freq_limit_box->  SetValue(FLATTENED_LOW_FREQ,  (float)_flattened_low_freq);
  _freq_limit_box->  SetValue(FLATTENED_HI_FREQ,   (float)_flattened_hi_freq);
  _tf_text_params->  SetValue(TF_SLICE,  (float)_displayed_sp->plottedTmin());
  _tf_text_params->  SetValue(TF_WINDOW_LENGTH,  
                               (float)(10.0 * _displayed_sp->sampleRate()));
  _tf_text_params->  SetValue(TF_FFT_LENGTH,  
                               (float)(63.0 * _displayed_sp->sampleRate()));
  XtSetSensitive(_tf_sine_interpolation->W(),   False);


  setWindowSensitivity();

  setTimeFreqState();

  checkTracePattern();

  setNormalizeState();




  wprocVAShowMsg(_information, 
"Displayed Info--> Total nplt: %d,   tmin: %4.3f,   tmax: %4.3f   srval: %4.3f",
                 _displayed_sp->plottedNplt(), _displayed_sp->minTmin(),
                 _displayed_sp->maxTmax(), _displayed_sp->sampleRate());

}


float SpectraMenu::getLargestMute()
{
float *hd = _displayed_sp->getHeaderArrayForUpdate();
long mute_val = -999999999L;

  for(long i = 0; i < _displayed_sp->plottedNplt(); i++)
    if(hd[LAV_HDR] != 0.0F)//dead trace will have record length for mute val
        mute_val = (long)((mute_val < hd[MUTE_HDR]) ? hd[MUTE_HDR] : mute_val);

  return (float) (mute_val * _displayed_sp->sampleRate());
}


float SpectraMenu::getSmallestMute()
{
float *hd = _displayed_sp->getHeaderArrayForUpdate();
long mute_val = 999999999L;

  for(long i = 0; i < _displayed_sp->plottedNplt(); i++)
    if(hd[LAV_HDR] != 0.0F)//dead trace will have record length for mute val
       mute_val = (long)((mute_val > hd[MUTE_HDR]) ? hd[MUTE_HDR] : mute_val);

  return (float) (mute_val * _displayed_sp->sampleRate());
}


int SpectraMenu::validate()
{
ShellWatch watch= topWidget();
SLErrorPop *error_pop;
int stat = 1;
int i, num_plots = 1;

  if(checkTracePattern() != 0)
   return stat = 0;   

  if(checkWindow(False)       != 0)
   return stat = 0;

  //If user wants and amplitude and phase set spectra type for each request
  if(_spectra_type_box->WhichSelected() == AMPLITUDE_AND_PHASE_TYPE)
    {
    num_plots = 2;
    _spectra_type_box->SetRadio(AMPLITUDE_TYPE);
    }

  for(i = 0; i < num_plots; i++)
    {
    if(i == 1)//User has requested amp and phase and we are ready for phase
      _spectra_type_box->SetRadio(PHASE_TYPE);

    if(_window_option_box->WhichSelected() == REPLACE_WINDOW &&
       _plot_array[i] != NULL                              )
      {
       _plot_array[i]->prepareForRedisplay();
      }
    else
      {
        //if(_plot_array[i]) 
        // _plot_array[i]->clearCbytImageMarks();

      _plot_array[i] = new SpectraPlot(topWidget(), "Spectra Analysis",
                                       getHelpCtx(), _displayed_sp,   
                                       this, 33);
      }


    _plot_array[i]->manage();

    XSync(XtDisplay(topWidget()),False);

    stat = _plot_array[i]->runAnalysis();

    if(stat)
      {
      delete(_plot_array[i]);
      _plot_array[i] = NULL;
      error_pop = new SLErrorPop(topWidget(), "Error running analysis",
         "Please recheck your input values.");
      if(num_plots == 2)
         _spectra_type_box->SetRadio(AMPLITUDE_AND_PHASE_TYPE);
      return stat = 0;
      }
    else
      {
      stat = _plot_array[i]->plot();
      if(stat)
        {
        delete(_plot_array[i]);
        _plot_array[i] = NULL;
        error_pop = new SLErrorPop(topWidget(), "Error in plotting",
                                   "Please recheck your input values");
        if(num_plots == 2)
           _spectra_type_box->SetRadio(AMPLITUDE_AND_PHASE_TYPE);
        return stat = 0;
        }
      }

    if(num_plots == 2)
         _spectra_type_box->SetRadio(AMPLITUDE_AND_PHASE_TYPE);

    }

  return  stat = 1;

} 



void SpectraMenu::okButton()
{

  if(validate())
    {
    DoAction();
    unmanage();
    }

}

void SpectraMenu::applyButton()
{

  if( validate() ) 
    DoAction();

}

void SpectraMenu::DoAction()
{

  SLFPopSep::DoAction();
  
}



void SpectraMenu::manage()
{
  if(!_displayed_sp->imageIsDisplayed())
    {
    SLErrorPop *error_pop = new SLErrorPop(topWidget(),
                 "Spectra Analysis Warning",
                 "Main window traces must be displayed");
    return;
    }
 
  if(_first_time)
    initValues();

  SLFPopSep::manage();

  _first_time = False;


}


void SpectraMenu::notCurrentInWindow(SeisPlot *sp)
{

  if(_displayed_sp != sp->currentSPInWindow())
    {
    addSeisPlot(sp->currentSPInWindow());
    _displayed_sp = sp->currentSPInWindow();
    }
}


void SpectraMenu::newPlot(SeisPlot *sp)
{

  if(sp == _displayed_sp)
    {
    if(_displayed_sp->imageIsDisplayed())
       initValues();
    }

}

void SpectraMenu::userRemovedPlot(SpectraPlot *plot)
{

  for(int i = 0; i < MAX_STORED_PLOTS; i++)
     if(plot == _plot_array[i])
        _plot_array[i] = NULL;

}

void SpectraMenu::destroyed(SeisPlot *sp)
{
  if(sp == _displayed_sp)
    {
    unmanage();
    }
}


void SpectraMenu::reloadDefaults(Boolean)
{
  SLFPopSep::reloadDefaults();
  _trace_select_box->reloadDefaults();
  DoAction();
}



void SpectraMenu::reloadSystemDefaults(Boolean do_method)
{
  SLFPopSep::reloadSystemDefaults(do_method);
  DoAction();
}


//============================================================================
//====================== Handle specific widget actions ======================
//============================================================================
Boolean SpectraMenu::notifyComplex(SLDelay *obj, int /*ident*/)
{
Boolean stat = 1;

  if(obj == _trace_select_box) 
    {
    checkTracePattern();
    setTimeFreqState();//So plot size will adjust if necessary
    }
  else if(obj == _plot_size_box)
    {
    checkPlotSize();
    }
  else if(obj == _window_type_box)
    {
    setWindowSensitivity();
    }
  else if(obj == _window_beginning_box || obj == _window_end_box)
    {
    checkWindow();
    }
  else if(obj == _taper_length_box)
    {
    checkTaper();
    }
  else if(obj == _spectra_type_box)
    {
    setSpectraType();
    }
  else if(obj == _phase_type_box)
    {
    setPhaseType();
    }
  else if(obj == _tf_distribution_options)
    {
    setTimeFreqState();
    }
  else if(obj == _tf_mode_options || obj == _tf_text_params)
    {
    setTimeFreqState(False);
    }
  else if(obj == _display_selected_box)
    {
    setNormalizeState();
    }
  else if(obj == _user_y_tog)
    {
    _user_y_box->SetSensitive(USER_Y_TOP,    _user_y_tog->IsSelected(USER_Y));
    _user_y_box->SetSensitive(USER_Y_BOTTOM, _user_y_tog->IsSelected(USER_Y));
    }

  return stat;
}

int SpectraMenu::checkTracePattern()
{
SLErrorPop *error_pop = NULL;
long ndo              = _trace_select_box->GetInt(NDO);
long nskp             = _trace_select_box->GetInt(NSKP);
long iskp             = _trace_select_box->GetInt(ISKP);
long old_nplt         = _trace_select_box->GetInt(NPLT);
long max_traces       = 0;
int new_nplt;
int error             = 0;


 for(int i = 0; i < _displayed_sp->plottedFrames(); i++)
   max_traces += _displayed_sp->displayedTraces(i);

  if( _trace_select_box->GetInt(NPLT) > max_traces) 
    {
    _nplt = max_traces;
    _trace_select_box->SetValue(NPLT,(long)_nplt);
    }

  //If the user is doing more than one trace at a time in his pattern
  //make sure that his number to skip is set also
  if(ndo > 1 && nskp < 1) 
    {
    nskp = _nskp = 1;
    _trace_select_box->SetValue(NSKP,(long)_nskp);
    }

  int pattern_len = ndo + nskp;

  new_nplt = (max_traces - iskp) /  pattern_len * ndo;

  int left_over = (max_traces - iskp) % pattern_len;

  if(left_over > ndo)
    new_nplt += ndo;
  else
    new_nplt += left_over;

  if(new_nplt < 1) 
    {
    _trace_select_box->SetValue(NPLT,max_traces);
    _trace_select_box->SetValue(NDO, 1L);
    _trace_select_box->SetValue(NSKP,0L);
    _trace_select_box->SetValue(ISKP,0L);
    error_pop = new SLErrorPop(topWidget(),"Trace Pattern Error",
          "Sorry the trace pattern requested will not plot\n\
           any traces... please start over");

    if(_nplt > 1) //Turn off single spectrum option
      XtSetSensitive(_display_selected_box->GetRadioWidget(SINGLE_SPECTRA), 
                      False);
    else
      XtSetSensitive(_display_selected_box->GetRadioWidget(SINGLE_SPECTRA), 
                      True);

    return error = 1;
    }

  if(new_nplt < old_nplt)
    {
    error_pop = new SLErrorPop(topWidget(),"Trace Pattern Warning",
                               "Total traces to plot reset");
    _nplt = new_nplt;
    _trace_select_box->SetValue(NPLT,(long)_nplt);
    error = 1;
    }

  if(_nplt > 1 && _display_selected_box->WhichSelected() == SINGLE_SPECTRA)
    _display_selected_box->SetRadio(FAMILY_SPECTRA);//could also set to averaged

  if(_nplt > 1) //Turn off single spectrum option
    {
    XtSetSensitive(_display_selected_box->GetRadioWidget(SINGLE_SPECTRA), 
                      False);
    }
  else//Only one trace so set single spectrum option
    {
    XtSetSensitive(_display_selected_box->GetRadioWidget(SINGLE_SPECTRA), 
                      True);
    _display_selected_box->SetRadio(SINGLE_SPECTRA);
    }


  return error;

}



void SpectraMenu::checkPlotSize()
{
SLErrorPop *error_pop = NULL;

  if(_plot_size_box->GetFloat(PLOT_WIDTH)  <= 0.0F)
    {
    _plot_size_box->SetValue(PLOT_WIDTH, 0.1F);
    error_pop = new SLErrorPop(topWidget(),"Plot Size Error",
          "Plot width must be greater than 0.0");
    return;
    }

  if(_plot_size_box->GetFloat(PLOT_HEIGHT)  <= 0.0F)
    {
    _plot_size_box->SetValue(PLOT_HEIGHT, 0.1F);
    error_pop = new SLErrorPop(topWidget(),"Plot Size Error",
          "Plot height must be greater than 0.0");
    }

}


void SpectraMenu::setWindowSensitivity()
{
  _window_beginning_box->SetSensitive( WINDOW_BEGINNING_FIXED,
                        (_window_type_box->WhichSelected() == FIXED_WINDOW) );
  _window_end_box->SetSensitive      ( WINDOW_END_FIXED,
                        (_window_type_box->WhichSelected() == FIXED_WINDOW) );

  _window_beginning_box->SetSensitive( WINDOW_BEGINNING_MUTE,
                        (_window_type_box->WhichSelected() == MUTE_WINDOW) );
  _window_end_box->SetSensitive      ( WINDOW_END_MUTE,
                        (_window_type_box->WhichSelected() == MUTE_WINDOW) );

}

//Selected windows must reside within the displayed times (not the file times)
int SpectraMenu::checkWindow(Boolean check_size)
{
SLErrorPop *error_pop;
float fixed_start, fixed_end;
float mute_add,  mute_win;
float mute_min,  mute_max;
int error = 1;

  fixed_start = _window_beginning_box->GetFloat(WINDOW_BEGINNING_FIXED);
  fixed_end   = _window_end_box->GetFloat(WINDOW_END_FIXED);
  mute_add    = _window_beginning_box->GetFloat(WINDOW_BEGINNING_MUTE);
  mute_win    = _window_end_box->GetFloat(WINDOW_END_MUTE);
  mute_min    = getSmallestMute() + mute_add;
  mute_max    = getLargestMute()  + mute_win + mute_add; 
  

   if(_window_type_box->WhichSelected() == FIXED_WINDOW)
     {

     if(fixed_start >= fixed_end)
       {
       _window_beginning_box->SetValue(WINDOW_BEGINNING_FIXED,
                                       (float) _displayed_sp->plottedTmin());
       _window_end_box->SetValue(WINDOW_END_FIXED,
                                       (float) _displayed_sp->plottedTmax());
       error_pop = new SLErrorPop(topWidget(),
           "Fixed Window Error",
           "Start of fixed window greater than or equal to end... resetting");
       return error;
       }

     if(fixed_start < _displayed_sp->plottedTmin())
       {
       _window_beginning_box->SetValue(WINDOW_BEGINNING_FIXED,
                                       (float) _displayed_sp->plottedTmin());
       error_pop = new SLErrorPop(topWidget(),
           "Fixed Window Error",
           "Start of fixed window less than displayed time min... resetting");
       return error;
       }

     if(fixed_start > _displayed_sp->plottedTmax())
       {
       _window_beginning_box->SetValue(WINDOW_BEGINNING_FIXED,
                                        (float)_displayed_sp->plottedTmin()); 
       error_pop = new SLErrorPop(topWidget(),
           "Fixed Window Error",
           "Start of fixed window greater than displayed time max...resetting");
       return error;
       }

     if(fixed_end > _displayed_sp->plottedTmax())
       {
       _window_end_box->SetValue(WINDOW_END_FIXED,
                                        (float)_displayed_sp->plottedTmax());
       error_pop = new SLErrorPop(topWidget(),
           "Fixed Window Error",
           "End of fixed window greater than displayed time max... resetting");
       return error;
       }

     if(fixed_end < _displayed_sp->plottedTmin())
       {
       _window_end_box->SetValue(WINDOW_END_FIXED,
                          (float)(fixed_start +_displayed_sp->sampleRate()) );
       error_pop = new SLErrorPop(topWidget(),
           "Fixed Window Error",
           "End of fixed window > than displayed time max... resetting");
       return error;
       }

     }
   else//MUTE_WINDOW
     {

     if(mute_min < _displayed_sp->plottedTmin())
       {
       _window_beginning_box->SetValue(WINDOW_BEGINNING_MUTE, 0.0F);
                                       
       _window_end_box->SetValue(WINDOW_END_MUTE,
                                       _displayed_sp->sampleRate());
       error_pop = new SLErrorPop(topWidget(),
        "Mute Window Error",
        "The add to mute + smallest mute header is less than the plotted tmin");
       return error;
       }
     
     if(mute_max > _displayed_sp->plottedTmax())
       {
       _window_beginning_box->SetValue(WINDOW_BEGINNING_MUTE, 0.0F);
                                       
       _window_end_box->SetValue(WINDOW_END_MUTE,
                      (float)(_displayed_sp->plottedTmax() - getLargestMute()));
       error_pop = new SLErrorPop(topWidget(),
        "Mute Window Error",
        "The add to mute + largest mute header + window length is \n\
         greater than the plotted tmax");
       return error;
       }

     }

  setTimeFreqState(check_size);
  return error = 0;

}

void SpectraMenu::checkTaper()
{
SLErrorPop *error_pop;

  if(_taper_length_box->GetFloat(TAPER_LENGTH) < 0.0F)
    {
    _taper_length_box->SetValue(TAPER_LENGTH, 0.1F);
    error_pop = new SLErrorPop(topWidget(),
           "Taper Length Error",
           "You entered a taper length less than 0.0, resetting to default");
    return;
    }
}



void SpectraMenu::setSpectraType()
{
   if(_spectra_type_box->WhichSelected() == AMPLITUDE_TYPE ||
      _spectra_type_box->WhichSelected() == AMPLITUDE_AND_PHASE_TYPE)
     {
     XtSetSensitive(_amplitude_type_box->W(), True);
     if(_spectra_type_box->WhichSelected() == AMPLITUDE_TYPE)
        XtSetSensitive(_phase_type_box->W(),     False);
     else
        XtSetSensitive(_phase_type_box->W(),     True);
     _freq_limit_box->SetSensitive(UNWRAPPED_LOW_FREQ, False);
     _freq_limit_box->SetSensitive(UNWRAPPED_HI_FREQ,  False);
     _freq_limit_box->SetSensitive(FLATTENED_LOW_FREQ, False);
     _freq_limit_box->SetSensitive(FLATTENED_HI_FREQ,  False);
     setTimeFreqState();
     }

   else if(_spectra_type_box->WhichSelected() == PHASE_TYPE ||
           _spectra_type_box->WhichSelected() == AMPLITUDE_AND_PHASE_TYPE)
     {
     XtSetSensitive(_amplitude_type_box->W(), False);
     XtSetSensitive(_phase_type_box->W(),     True);
     setPhaseType();
     setTimeFreqState();
     }

   else//time frequency
     {
     setTimeFreqState();
     }

   if(_spectra_type_box->WhichSelected() == AMPLITUDE_TYPE || 
      _spectra_type_box->WhichSelected() == PHASE_TYPE        )
     {
     XtSetSensitive(_user_y_tog->W(), True);
     _user_y_box->SetSensitive(USER_Y_TOP,    _user_y_tog->IsSelected(USER_Y));
     _user_y_box->SetSensitive(USER_Y_BOTTOM, _user_y_tog->IsSelected(USER_Y));
     }
   else
     {
      _user_y_tog->SetTog( USER_Y, False );
     XtSetSensitive(_user_y_tog->W(), False);
     }




   setNormalizeState();

}


void SpectraMenu::setTimeFreqState(Boolean /*check_size*/)
{
SLErrorPop *error_pop;
int m, mm, n;
int min_fftlen;
int old_fftlen;
int new_fftlen;
double exponent;

  if(!_finished_make) return;

  
  if(_spectra_type_box->WhichSelected() == TIMEFREQ_TYPE)
    {
    /* Disabling this user convenience of setting the plot size for now
    //set a plot size appropriate for time freq
    if(_tf_mode_options->whichSelected() != TF_DISTRIBUTION_SLICE_MODE)
       {
       //Approximate the number of traces that will be output
       int num_time_freq;
       if(_tf_mode_options->whichSelected() == TF_DISTRIBUTION_MODE)
         num_time_freq =  getNumTimeFreqTraces() * _nplt;
       else
         num_time_freq = _nplt;
       if(check_size)
         {
         _plot_size_box->SetValue(PLOT_WIDTH, 
                                (float)(num_time_freq /
                                        _displayed_sp->ti()));
         _plot_size_box->SetValue(PLOT_HEIGHT, 
                                (float)(_displayed_sp->is() *
                                        (_displayed_sp->tmax() -
                                         _displayed_sp->tmin())));
         }
       }
    else//Slice, so set up like a vector graph plot
       {
       if(check_size)
         {
         _plot_size_box->SetValue(PLOT_WIDTH, 7.0F);
         _plot_size_box->SetValue(PLOT_HEIGHT,2.0F);
         }
       }
    */




    //Set sensitivities
    XtSetSensitive(_phase_type_box->W(),          False);
    XtSetSensitive(_taper_length_box->W(),        False);
    XtSetSensitive(_display_freq_box->W(),        False);    
    XtSetSensitive(_freq_limit_box->W(),          False);  
    XtSetSensitive(_tf_distribution_options->W(), True);
    XtSetSensitive(_tf_text_params->W(),          True);
    XtSetSensitive(_display_selected_box->W(),    False);
    if(_tf_mode_options->whichSelected() == TF_DISTRIBUTION_SLICE_MODE)
      {
      _tf_text_params->SetSensitive(TF_SLICE, True);
      XtSetSensitive(_display_freq_box->W(),  True);  
      }
    else
      {
      _tf_text_params->SetSensitive(TF_SLICE, False);
      XtSetSensitive(_display_freq_box->W(),  False);  
      }
    if(_tf_distribution_options->whichSelected() == TF_SHORT_TIME_FOURIER)
      {
      XtSetSensitive(_tf_short_time_window->W(),    True);
      _tf_text_params->SetSensitive(TF_OPT_LEVEL,   False);
      }
    else
      {
      XtSetSensitive(_tf_short_time_window->W(),    False);
      _tf_text_params->SetSensitive(TF_OPT_LEVEL,   True);
      }
    int sinusoid=(_tf_distribution_options->whichSelected() == TF_SINUSOID);
    XtSetSensitive(_tf_mode_options->W(),         !sinusoid);  
    XtSetSensitive(_tf_marginal_options->W(),     !sinusoid);
    _tf_text_params->SetSensitive(TF_FFT_LENGTH,  !sinusoid);
    _tf_text_params->SetSensitive(TF_OPT_LEVEL,   !sinusoid);
    XtSetSensitive(_tf_sine_interpolation->W(),   sinusoid);




    //Insure slice is in range
    if(_window_type_box->WhichSelected() == FIXED_WINDOW)
      {
      if(_tf_slice < _window_beginning_fixed)
        {
        //Only notify user if the slice mode is selected, else set it secretly
        if(_tf_mode_options->whichSelected() == TF_DISTRIBUTION_SLICE_MODE)
          error_pop = new SLErrorPop(topWidget(), "Error in slice",
              "Time slice is less than Fixed Start Window Time");

        _tf_text_params->SetValue(TF_SLICE,(float)_window_beginning_fixed);
        }
      if(_tf_slice > _window_end_fixed)
        {
        //Only notify user if the slice mode is selected, else set it secretly
        if(_tf_mode_options->whichSelected() == TF_DISTRIBUTION_SLICE_MODE)
          error_pop = new SLErrorPop(topWidget(), "Error in slice",
              "Time slice is more than Fixed End Window Time");
        _tf_text_params->SetValue(TF_SLICE,(float)_window_end_fixed);
        }
      }
    else//Mute window
      {
      if(_tf_slice < _window_beginning_mute + getSmallestMute())
        {
        //Only notify user if the slice mode is selected, else set it secretly
        if(_tf_mode_options->whichSelected() == TF_DISTRIBUTION_SLICE_MODE)
          error_pop = new SLErrorPop(topWidget(), "Error in slice",
              "Time slice is less than mute window");

        _tf_text_params->SetValue(TF_SLICE,
                             (float)_window_beginning_mute + getSmallestMute());
        }
      if(_tf_slice > _window_end_mute + getLargestMute())
        {
        //Only notify user if the slice mode is selected, else set it secretly
        if(_tf_mode_options->whichSelected() == TF_DISTRIBUTION_SLICE_MODE)
          error_pop = new SLErrorPop(topWidget(), "Error in slice",
              "Time slice is more than mute window");

        _tf_text_params->SetValue(TF_SLICE,
                             (float)_window_end_mute + getLargestMute());
        }
      }

    //Insure window length is an odd number of points
    if(_tf_distribution_options->whichSelected() != TF_SINUSOID)
      {
      int nsamp_length = (int)(getTfWindowLength() / 
                               _displayed_sp->sampleRate() + 1.5);
      int odd          = nsamp_length % 2;
      if(!odd)
        {
        error_pop = new SLErrorPop(topWidget(), "Error in window length",
              "Resetting window length (must be odd)");
        _tf_text_params->SetValue(TF_WINDOW_LENGTH, _tf_window_length +
                                  _displayed_sp->sampleRate() );
        }

      //Make sure the fft length is not smaller than the 
      //smallest power of two >= window length
      n = (int)(getTfWindowLength() / _displayed_sp->sampleRate() + 1.5);
      m = 0; mm = 1;
      while (mm < n) 
        {
        ++m;
        mm = 2 * mm;
        }
      min_fftlen = (int) pow(2.0,(double) m);
      old_fftlen = (int)(getTfFftLength() / _displayed_sp->sampleRate() + 1.5);
      if(old_fftlen < min_fftlen)
        {
        error_pop = new SLErrorPop(topWidget(), "Error in fft length",
          "           Resetting Fft length (cannot be less than the \n\
           next power of two of the window length");
        _tf_text_params->SetValue(TF_FFT_LENGTH, 
                           (min_fftlen - 1)* _displayed_sp->sampleRate());
        }

      //Make sure the fft length is a power of 2
      old_fftlen = (int)(getTfFftLength() / _displayed_sp->sampleRate() + 1.5);
      exponent = log(old_fftlen) / log(2.0);
      exponent = ceil(exponent);
      new_fftlen = (int) pow(2.0,(double) exponent);
      if(old_fftlen != new_fftlen)
        {
        error_pop = new SLErrorPop(topWidget(), "Error in fft length",
              "Resetting fft length (must be a power of 2)");
        _tf_text_params->SetValue(TF_FFT_LENGTH, 
                               (new_fftlen - 1)* _displayed_sp->sampleRate());
        }
      }//End non-sinusoid fft and window length checks
    else//Sinusoid, make sure window length is odd
      {
      int nsamp_length = (int)(getTfWindowLength() / 
                               _displayed_sp->sampleRate() + 1.5);
      int odd          = nsamp_length % 2;
      if(!odd)
        {
        error_pop = new SLErrorPop(topWidget(), "Error in window length",
              "Resetting window length (must be odd)");
        _tf_text_params->SetValue(TF_WINDOW_LENGTH, _tf_window_length +
                                  _displayed_sp->sampleRate() );
        }
      }


    //Insure optimization level is between 1 and 5
    if(_tf_opt_level < 1.0 || _tf_opt_level > 5.0)
      {
      error_pop = new SLErrorPop(topWidget(), "Error in optimization level",
            "Optimization level must be 1.0 to 5.0");
      _tf_text_params->SetValue(TF_OPT_LEVEL, 1.0F);
      }
    }

  else//Time freq not selected
    {
    //Set sensitivities
    if(_spectra_type_box->WhichSelected() == AMPLITUDE_TYPE)
      XtSetSensitive(_phase_type_box->W(),        False);
    XtSetSensitive(_taper_length_box->W(),        True);
    XtSetSensitive(_display_freq_box->W(),        True);    
    XtSetSensitive(_freq_limit_box->W(),          True);  
    XtSetSensitive(_tf_distribution_options->W(), False);
    XtSetSensitive(_tf_mode_options->W(),         False);  
    XtSetSensitive(_tf_marginal_options->W(),     False);
    XtSetSensitive(_tf_text_params->W(),          False);
    XtSetSensitive(_display_selected_box->W(),    True);
    XtSetSensitive(_tf_short_time_window->W(),    False);
    //_plot_size_box->SetValue(PLOT_WIDTH, 7.0F);
    //_plot_size_box->SetValue(PLOT_HEIGHT,2.0F);
    }

  
}


void SpectraMenu::setNormalizeState()
{
  if( (_spectra_type_box->WhichSelected() == AMPLITUDE_TYPE ||
       _spectra_type_box->WhichSelected() == AMPLITUDE_AND_PHASE_TYPE) &&
      _display_selected_box->WhichSelected() == FAMILY_SPECTRA             )
     XtSetSensitive(_normalize_box->W(),    True);
  else
     XtSetSensitive(_normalize_box->W(),    False);
}


int SpectraMenu::normalizeFamily()
{
int can_do_normalize;

  if( (_spectra_type_box->WhichSelected() == AMPLITUDE_TYPE ||
       _spectra_type_box->WhichSelected() == AMPLITUDE_AND_PHASE_TYPE) &&
      _display_selected_box->WhichSelected() == FAMILY_SPECTRA             )
    can_do_normalize = 1;
  else
    can_do_normalize = 0; 

  if(can_do_normalize && _normalize_box->IsSelected(NORMALIZE))
    return 1;
  else
    return 0;
}


void SpectraMenu::setPhaseType()
{
   if(_phase_type_box->WhichSelected() == WRAPPED)
     {
     _freq_limit_box->SetSensitive(UNWRAPPED_LOW_FREQ, False);
     _freq_limit_box->SetSensitive(UNWRAPPED_HI_FREQ,  False);
     _freq_limit_box->SetSensitive(FLATTENED_LOW_FREQ, False);
     _freq_limit_box->SetSensitive(FLATTENED_HI_FREQ,  False);
     }

   else if(_phase_type_box->WhichSelected() == UNWRAPPED)
     {
     XtSetSensitive(_freq_limit_box->W(), True);
     _freq_limit_box->SetSensitive(UNWRAPPED_LOW_FREQ, True); 
     _freq_limit_box->SetSensitive(UNWRAPPED_HI_FREQ,  True); 
     _freq_limit_box->SetSensitive(FLATTENED_LOW_FREQ, False);
     _freq_limit_box->SetSensitive(FLATTENED_HI_FREQ,  False);
     }

   else//time frequency.
     {
     _freq_limit_box->SetSensitive(UNWRAPPED_LOW_FREQ, True); 
     _freq_limit_box->SetSensitive(UNWRAPPED_HI_FREQ,  True); 
     _freq_limit_box->SetSensitive(FLATTENED_LOW_FREQ, True);
     _freq_limit_box->SetSensitive(FLATTENED_HI_FREQ,  True);
     }

}



//============================================================================
//============= General routine to set plot type parameters ==================
//============================================================================
Boolean SpectraMenu::setPlotParameters()
{
Boolean stat = 1;

  return(stat);
}


//============================================================================
//============= Public methods to get the values            ==================
//============================================================================
float SpectraMenu::getPlotWidth()
{
  return _plot_width;
}

float SpectraMenu::getPlotHeight()
{
  return _plot_height;
}

float SpectraMenu::getDisplayLowFreq()
{
  return _display_low_freq;
}

float SpectraMenu::getDisplayHiFreq()
{
  return _display_hi_freq;
}


int SpectraMenu::getSpectraType()
{
  return _spectra_type_box->WhichSelected();
}

int SpectraMenu::getAmplitudeType()
{
  return _amplitude_type_box->WhichSelected();
}


char *SpectraMenu::getPhaseType()
{
static char type[10];

  strcpy(type,"INVALID");

  switch(_phase_type_box->WhichSelected())
    {
    case WRAPPED:
        strcpy(type,"NONE");  //Fortran expects this string
        break;
 
    case UNWRAPPED:
        strcpy(type,"UNWRAP"); //Fortran expects this string
        break;
    
    case UNWRAPPED_FLATTENED:
        strcpy(type,"FLATTEN"); //Fortran expects this string
        break;

    default:
        assert(0);
        break;
    }

  return type;
    
}


float SpectraMenu::getFreqMax()
{
  return _freq_max;
}

float SpectraMenu::getFlattenedLowFreq()
{
  return _flattened_low_freq;
}

float SpectraMenu::getFlattenedHiFreq()
{
  return _flattened_hi_freq;
}

float SpectraMenu::getUnwrappedLowFreq()
{
  return _unwrapped_low_freq;
}

float SpectraMenu::getUnwrappedHiFreq()
{
  return _unwrapped_hi_freq;
}

float SpectraMenu::getTaperLength()
{
  return _taper_length;
}

float SpectraMenu::getFixedWindowBeginningTime()
{
  return _window_beginning_fixed;
}

float SpectraMenu::getFixedWindowEndingTime()
{
  return _window_end_fixed;
}

float SpectraMenu::getMuteWindowBeginningTime()
{
  return _window_beginning_mute;
}

float SpectraMenu::getMuteWindowEndingTime()
{
  return _window_end_mute;
}


int SpectraMenu::getNumSpectra()
{
  return _display_selected_box->WhichSelected();
}


int SpectraMenu::getWindowType()
{
  return _window_type_box->WhichSelected();
}

void SpectraMenu::getTracePattern(long *nplt, long *iskp, 
                                  long *ndo,  long *nskp) 
{
  *nplt = _trace_select_box->GetInt(NPLT);
  *iskp = _trace_select_box->GetInt(ISKP);
  *ndo  = _trace_select_box->GetInt(NDO);
  *nskp = _trace_select_box->GetInt(NSKP);
}


char *SpectraMenu::getTfMode(int *index)
{
char *mode;


  switch(_tf_mode_options->whichSelected())
    {
      case TF_DISTRIBUTION_MODE:
        mode = "tfdist";
      break;
      case TF_DISTRIBUTION_SLICE_MODE:
        mode = "slice";
      break;
      case TF_IBAND_MODE:
        mode = "band";
      break;
      case TF_ICENTERFREQ_MODE:
        mode = "cfreq";
      break;
      case TF_IDOMINANTFREQ_MODE:
        mode = "dfreq";
      break;
      case TF_IMAXIMALFREQ_MODE:
        mode = "maxfreq";
      break;
      case TF_IQ_MODE:
        mode = "q";
      break;
    }

  if(index) *index = _tf_mode_options->whichSelected();

  return mode;
}

char *SpectraMenu::getKernal(int *index)
{
char *mode;


  switch(_tf_distribution_options->whichSelected())
    {
      case TF_ADAPTIVE_OPTIMIZED:
        mode = "aok";
      break;
      case TF_SHORT_TIME_FOURIER:
        mode = "stft";
      break;
      case TF_SINUSOID:
        mode = "sinusoid";
      break;
    }

  if(index) *index = _tf_distribution_options->whichSelected();

  return mode;
}


char *SpectraMenu::getTfMarginal(int *index)
{
char *marginal;

  switch(_tf_marginal_options->whichSelected())
    {
      case TF_NONE_TFMARGINAL:
        marginal = "none";
      break;
      case TF_NORMALIZE_TIMEMARGINAL:
        marginal = "time";
      break;
      case TF_NORMALIZE_FREQMARGINAL:
        marginal = "freq";
      break;
    }
  
  if(index) *index = _tf_marginal_options->whichSelected();

  return marginal;
}



long SpectraMenu::getNumTimeFreqTraces()
{
int fftlen;
long num_time_freq;
float nyquist = 1.0F / (2.0F * _displayed_sp->sampleRate());

    //If not a time frequency plot
    if(getSpectraType() != TIMEFREQ_TYPE)
      {
      num_time_freq = 1;
      return num_time_freq;
      }

    //If a non-sinusoid time frequency distribution plot
    if(_tf_distribution_options->whichSelected() != TF_SINUSOID)
      {
      if(_tf_mode_options->whichSelected() == TF_DISTRIBUTION_MODE ||
         _tf_mode_options->whichSelected() == TF_DISTRIBUTION_SLICE_MODE )
        {
        fftlen = (int)(getTfFftLength() / _displayed_sp->sampleRate() + 1.5);
        num_time_freq = fftlen / 2 + 1;
        return num_time_freq;
        }
      else//Instantaneous time frequency analysis or slice
        {
        num_time_freq = 1;
        return num_time_freq;
        }
      }
    else//Sinusoid type
      {
      num_time_freq = (NearestInteger(_tf_window_length / 
                       _displayed_sp->sampleRate()) + 1.0F) / 2;
      return num_time_freq;
      }

}


char *SpectraMenu::getFastWindowType(int *index)
{
char *mode;

  switch(_tf_short_time_window->whichSelected())
    {
      case TF_HAMMING:
        mode = "Hamming";
      break;
      case TF_RECTANGULAR:
        mode = "Rectangular";
      break;
      case TF_HANNING:
        mode = "Hanning";
      break;
      case TF_BLACKMAN:
        mode = "Blackman";
      break;
      case TF_BARTLET:
        mode = "Bartlet";
      break;

    }

  if(index) *index = _tf_distribution_options->whichSelected();

  return mode;
}


int SpectraMenu::getSineInterpolation()
{
int interp = 0;

   switch(_tf_sine_interpolation->whichSelected())
     {
       case TF_LINEAR:
         interp = 0;
       break;
    
       case TF_SPLINE:
         interp = 1;
       break;
     }

   return interp;
}


int SpectraMenu::userSettingDisplayedY(float *top, float *bottom)
{
  *top = _user_y_top;
  *bottom = _user_y_bottom;
  return _user_y_tog->IsSelected(USER_Y);
}
