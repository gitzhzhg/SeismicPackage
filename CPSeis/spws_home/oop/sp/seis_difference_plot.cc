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
//***             Creates a difference plot in a                       ***
//***             separate window Author:Michael L. Sherrill 12/2001   ***
//************************************************************************

#include "sp/seis_difference_plot.hh"
#include "sp/seis_difference_pop.hh"
#include "sp/seis_plot.hh"
#include "sp/seis_loc_out.hh"
#include "sp/seis_cbar_pop.hh"
#include "sp/seis_color_pop.hh"
#include "image_amplitude_recovery.hh"
#include "sl/sl_client_message.hh"
#include "sl/shell_stat_msg.hh"
#include "sl/sl_push_box.hh"
#include "sl/shell_watch.hh"
#include "sl/sl_error_pop.hh"
#include "minimax.h"

#include <stdlib.h>
#include <math.h>

static String  defres[]= 
    {
    ".resizePolicy:              RESIZE_NONE",
    //".height:                    375",
    //".width:                     1100",
    "*Active.background:         red",
    NULL
    };

//============================================================================
//====================== Constructor with own window =========================
//============================================================================
SeisDifferencePlot::SeisDifferencePlot(Widget               p,
                                       char                 *name,
                                       HelpCtx              hctx, 
                                       SeisPlot             *first_sp,
                                       SeisPlot             *second_sp,
                                       SeisDifferencePop    *pop,
                                       int                  numcolors)

                   : SLFormHelpPop(p, name, FP_DOREMOVE | FP_DOHELP,
	                           hctx, True, 2, False, True, numcolors, True),

                     SeisInform(first_sp), 

                     _first_sp(first_sp), 
                     _second_sp(second_sp),
                     _numcolors(numcolors),
                     _been_managed(False), _pop(pop) 
                    
{
  
  _sp                  = NULL;
  _loc                 = NULL;
  _cbar_pop            = NULL;
  _color_pop           = NULL;

  setDefaultResources( p, name, defres);

  make(p);

}



//============================================================================
//====================== Destructor               =========================
//============================================================================
SeisDifferencePlot::~SeisDifferencePlot()
{
float *traces  =  _sp->getFloatArrayForUpdate();
float *headers = _sp->getHeaderArrayForUpdate();

  if(traces  != NULL) free(traces);
  if(headers != NULL) free(headers);
  delete _sp;
  if(_loc)            delete _loc;
  if(_cbar_pop)       delete _cbar_pop;
  if(_color_pop)      delete _color_pop;
}


//============================================================================
//====================== Make Popup                  =========================
//============================================================================
Widget SeisDifferencePlot::make(Widget p)
{
char temp[256];
char junk[256];

   if ( made() ) return topWidget();

   Widget parent = p ? p : wParent();
   ShellStatMsg bld_info(parent, "Building Spectra Plot...");


   SLFormHelpPop::make(p);

   strcpy(_title,"");
   parse_file_(_first_sp->filename(), temp, junk);
   strcat(_title,temp);
   strcat(_title," - ");
   parse_file_(_second_sp->filename(),temp, junk);
   strcat(_title,temp);

   setTitle(_title);

   _sp = new SeisPlot(topWidget(), "Difference");
   _sp->setLoadableColors(max(33,_numcolors));

   _loc = new SeisLocOut(topWidget(),"diff_loc_out", _sp, getHelpCtx());

   _color_pop = new SeisColorPop( topWidget(), "Spectra Color", _sp,
                                  getHelpCtx(), True, True);
   _color_pop->make();

   _cbar_pop  = new SeisCbarPop(topWidget(), "Color Bar", 
                                   _sp, getHelpCtx());

   return topWidget();
  
}


//============================================================================
//====================== Manage Popup                =========================
//============================================================================
void SeisDifferencePlot::manage()
{
int pixels_wide, pixels_high;


  //The following attachements were put into this area because we get
  //a circular dependency error if it is destroyed and never managed
  //and the attachments have been registered.
  if(_been_managed == False)
   { 
   addExtraButton("Colors Options", COLOR_POP);
   addExtraButton("Color Bar", COLOR_BAR);

   XtVaSetValues( topWidget(), XmNresizePolicy, XmRESIZE_NONE, NULL);

   pixels_wide = 700;
   pixels_high = 700;

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


void SeisDifferencePlot::extraButton(int ident)
{

  switch(ident)
    {
    case COLOR_POP:
      _color_pop->makeAndManage();
      _color_pop->setTitle(_title);
      _color_pop->dontPlotYet(False);
      _sp->setPlotType(PlotImage::PlotCOLOR);
    break;

    case COLOR_BAR:
      _cbar_pop->makeAndManage();
      _cbar_pop->setTitle(_title);
    break;

    default:
      assert(0);
    break;
    }     
}


void SeisDifferencePlot::getMinAndMaxOfData(long n, float *data, float *amp_min,
                                            float *amp_max)
{
float lav_min;
float lav_max;

  *amp_min = *amp_max = data[0];
  for(long i = 0; i < n; i++)
    {
    *amp_min = (data[i] < *amp_min) ? data[i] : *amp_min;
    *amp_max = (data[i] > *amp_max) ? data[i] : *amp_max;
    }

  lav_min = fabs(*amp_min);
  lav_max = fabs(*amp_max);
  if(lav_max > lav_min)
    _lav = *amp_max;
  else
    _lav = *amp_min;
  
}


int SeisDifferencePlot::computeDifference()
{
int error = 0, ok = 0;
float *first_traces = _first_sp->getFloatArrayForUpdate();
float *second_traces = _second_sp->getFloatArrayForUpdate();
float *first_headers = _first_sp->getHeaderArrayForUpdate();
float *diff_traces;
float *diff_headers;
long i, j, k, first_offset, second_offset;
long first_nsamp, second_nsamp, diff_nsamp;



  diff_nsamp = (long)((_pop->_tmax - _pop->_tmin)
                / _first_sp->srval() + 1);
  _sp->setGridY1(_pop->_tmin);
  _sp->setGridY2(_pop->_tmax);
  _sp->setSrval(_first_sp->srval());
  _sp->setMatchHeader(_first_sp->matchHeader());

  ok = _sp->initArrayTypeData( 0, 1, _pop->_nplt, diff_nsamp,
                               NULL);
  if(!ok) return error = 1;

  _sp->pointToArrayTypeData(_sp);//So SeisPlot will not try to read a file

  diff_traces = _sp->getFloatArrayForUpdate();
  diff_headers= _sp->getHeaderArrayForUpdate();
  first_nsamp = _first_sp->samplesPerTrace();
  second_nsamp= _second_sp->samplesPerTrace();
  first_offset = (long)((_pop->_tmin - _first_sp->tmin()) /
                     _first_sp->srval());
  second_offset= (long)((_pop->_tmin - _second_sp->tmin()) /
                     _second_sp->srval());

  for(i = 0; i < _pop->_nplt; i++)
    {
    for(j = 0; j < diff_nsamp; j++)
      {
      diff_traces[i*diff_nsamp+j] = 
                       first_traces[ i * first_nsamp  + first_offset + j] -
                       second_traces[i * second_nsamp + second_offset+ j]; 
      }
    for(k = 0; k < _sp->numHeaders(); k++)
      {
      diff_headers[i * _sp->numHeaders() + k] = 
                                    first_headers[i * _sp->numHeaders() + k];
      }
    }


  //Populate the PlotImage class' amplitude recovery scale factor array.
  //This array holds the largest value of each trace and will be used
  //only if the user has requested normalization of each trace
  if(_pop->_norm_type->WhichSelected() == PlotImage::NORM)
    {
    _sp->setNorm(_pop->_norm_type->WhichSelected());
    _sp->setMemoryTraces(_pop->_nplt);
    _sp->setSamplesPerTrace(diff_nsamp);
    ok = _sp->getAmplitudeRecovery()->allocateScaleFactorArray(_pop->_nplt);
    if(!ok) return error = 1;
    _sp->getAmplitudeRecovery()->setScaleFactorForReadArray(0, _pop->_nplt);
    }

  return error;
}


int  SeisDifferencePlot::plot(char *errmsg)
{
int error = 0;
int stat;
float amp_min;
float amp_max;
long total_traces;
int nsamp;


  error = computeDifference();
  if(error)
    {
    strcpy(errmsg,"Not enough memory for computing difference plot.\n");
    return error;
    }

  // later _pop->getTracePattern(&nplt, &iskp, &ndo, &nskp);
  total_traces = _pop->_nplt;
  nsamp = (int)((_pop->_tmax - _pop->_tmin) /
           _first_sp->srval() + 1);
  getMinAndMaxOfData(nsamp * total_traces, _sp->getFloatArrayForUpdate(),
                     &amp_min, &amp_max);

  if(amp_min == 0.0F && amp_max == 0.0F)
    {
    strcpy(errmsg,"Found no difference in your files.\n");
    return error = 1;
    }

  if(_pop->_plot_mode_radios->WhichSelected() > PlotImage::PlotWFILL)//Color
    {
    _color_pop->setAmplitudes(amp_min, amp_max);
    _sp->setDoAmplitude(True);
    _sp->setDoPercent(False);
    _sp->setExternalAmp(amp_max);
    _sp->setMinColorAmp(amp_min);
    _sp->setMaxColorAmp(amp_max);
    }

  _sp->setNegativeFill(False);
  _sp->setMaxDataAmp(_lav);
  _sp->setTI(_pop->_traces_per_inch); 
  _sp->setIS(_pop->_inches_per_second);
  _sp->setCT(_pop->_ct);
  _sp->setNorm(_pop->_norm_type->WhichSelected());
  if (_pop->_norm_type->WhichSelected() == PlotImage::EXTERNALNORM)
    _sp->setExternalAmp(_pop->_external_amp);

  if(_pop->_plot_mode_radios->WhichSelected() ==
      SeisDifferencePop::NEGATIVE_FILL           )
    {
    _sp->setPlotType(PlotImage::PlotWFILL);
    _sp->setNegativeFill(True);
    }
  else
    {
    _sp->setPlotType(_pop->_plot_mode_radios->WhichSelected());
    _sp->setNegativeFill(False);
    }

  //Annotation
  _sp->setFirstLbl(_first_sp->firstLbl());
  _sp->setLblInc(_first_sp->lblInc());
  _sp->setTminTmax(_pop->_tmin, _pop->_tmax);
  _sp->setTimingLines(_first_sp->primTimingLine(),
                      _first_sp->secTimingLine()); 
  _sp->setHeader1(_first_sp->header1());
  _sp->setHeader2(_first_sp->header2());


  _sp->setSrval(_first_sp->srval());
  strcpy(_sp->filename(),"difference");

  manage();

  _sp->backingStore(True);

  stat = _sp->plot();

  if(stat < 1)
    {
    unmanage();
    error = 1;
    strcpy(errmsg,"Error in generating the difference plot.\n");
    }


  return error;

}





//=============================================================================
//===================Informs and actions area, may need later =================
//=============================================================================
void SeisDifferencePlot::newPlot(SeisPlot *sp)
{
}

void SeisDifferencePlot::destroyed(SeisPlot *sp)
{
}



void SeisDifferencePlot::removeButton()
{
  _pop->userRemovedPlot(this);
  unmanage();
  delayDelete();
}


void SeisDifferencePlot::postScan(SeisPlot *sp, SeisPlot::ScanDir /*dir*/)
{
}

