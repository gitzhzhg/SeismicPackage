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
/*------------------------------------------------------------------
 *USER DOC
 *Name   : modifyPlot
 *Purpose: Tests to see if data needs to reread and calls appropriate 
 *         plotting routine.      
 *Author : Michael L. Sherrill
 *Date   : 02/93 (C++ version 4/97)
 *
 * Function Definition:
 * long modify_plot ()
 *
 *
 *NOTES:
 * 1.
 *
 *
 *Revisions:
 * DATE      WHO         DESCRIPTION
 * --------  --------    --------------------------------------------
 *
 *END DOC
 *------------------------------------------------------------------*/

#include <X11/Xlib.h>
#include "plot_image.hh"
#include "image_amplitude_recovery.hh"
#include "sl/sl_error_pop.hh"



#define CT_WARN "Warning you have requested a ct less than 1.0\n\
A new ct has been recalculated based on the\n\
median data value similar to splt cctf processing.\n\
This is a one time warning that will\n\
not appear on subsequent plots."


long PlotImage::modifyPlot()


{
int screen;
Screen *scr;
Display *dpy;
long stat, delta;
long reread = False;
float vpixels_per_inch; 
float hpixels_per_inch;
double zfact = 1.0;
float diff1, diff2;
long new_width;
long is_zoomed;
int nin, nout;
long AryOffset = 0;
float median_value;
float srval;
float scale_amp;
long trace_index;
static Boolean show_warning = True;
SLErrorPop *slpop;


 //recompute scale amps for image because movie will have  
 //set these to the last image created

 AryOffset = _cpixm * getTracesInMemory() * getSamplesInMemory();

 if(_user->_mode != PlotGRID && _user->_mode != PlotHEADER && _frames > 1)
   {           
  _amp_recovery->setScaleAmp(_cpixm);
  _amp_recovery->computeDisplayedAmps(AryOffset);    
  _max_amplitude = _amp_recovery->getMaxDisplayedAmplitude();
  _min_amplitude = _amp_recovery->getMinDisplayedAmplitude();
  }

 if(_user->getNorm() == PANELNORM)
   scale_amp = _amp_recovery->getScaleAmp(_cpixm);
 else
   scale_amp = _amp_recovery->getScaleAmp(0);
 
 scr = XtScreen(_graphic);
 dpy = XtDisplay(_graphic);
 screen = DefaultScreen(XtDisplay(_graphic));


/*see if new image size will be larger than original. if so reread data*/
 diff1 = _user->_tmin - _tmin;
 diff1 = (diff1 < 0) ? -diff1 : diff1;
 diff2 = _user->_tmax - _tmax;
 diff2 = (diff2 < 0) ? -diff2 : diff2;
 srval = _user->_G.srval;
 srval = (srval < 0) ? -srval : srval;
 if(diff1 >= srval || diff2 >= srval) reread = True;

/*Currently set the reread flag so check_size gets called on array data*/
 if(_user->_mode == PlotARRAY) reread = True;

 if(_user->_mode == PlotWONLY || _user->_mode == PlotWFILL)
    {
    _user->_vel_min = _vel_min;
    _user->_vel_max = _vel_max;
    }

 if(_user->_mode != PlotCOLOR) 
    {
    if(   _user->_vel_min != _vel_min 
       || _user->_vel_max != _vel_max) reread = True;
    }

 if(_user->_mode == PlotSEMB || _user->_mode == PlotCONTOUR)
    {
    hpixels_per_inch =  horizontalPixelsPerInch(dpy, screen);
    vpixels_per_inch =  verticalPixelsPerInch(dpy, screen);
    delta = (long)((_user->_ti * hpixels_per_inch) / _ntot);
    if(!_zoomed)
        new_width =  (long)(_user->_ti * hpixels_per_inch);
    else
        new_width = (long)((_user->_vel_max - _user->_vel_min) 
                     * _velfactor);
    if(new_width  != _ximage.width) reread = True;
    if(_user->_mode == PlotCONTOUR && _float_array == NULL)
       reread = True;
    if(_user->_contours && _float_array == NULL) reread = True;
    if(_user->_is != _is) reread = True;
    if(_user->_ti != _ti) reread = True;
    }

 if(_user->_mode == PlotWONLY || _user->_mode == PlotWFILL ||
    _user->_mode == PlotCOLOR)
    {
    if((_user->_movie == True) && (_user->_frames != _frames)) 
      reread = True; 

    /*if movies and a fixed lav was not used we need to reset the 
     *scaling amp parameter used to scale individual plots, so force a read
     */
    if(_user->_mode == PlotCOLOR && 
       _user->getNorm() != FILENORM &&
       _user->getNorm() != EXTERNALNORM  &&
       _user->_movie == True) reread = True;

    if(_user->_skip_frames != _skip_frames ||             
       _user->_nskp        != _Cl.nskp     ||
       _user->_nplt        != _ntot        ||
       _user->_iskp        != _Cl.iskp     ||
       _user->_is          != _is          ||
       _user->_ti          != _ti) reread = True;
    }


/*currently reread the data on all color images to insure proper scaling*/
 //  if(_user->_mode != PlotGRID  && _user->_mode != PlotHEADER &&
 //    _user->_mode != PlotWONLY && _user->_mode != PlotWFILL) reread = True;    
 

/*if we need to reread data*/
 if(reread)
    {
    stat = checkSize();
    if(stat == PlotSuccess) stat = plot();
    return(stat);
    }
 else  /*call appropriate plot generation without reread*/
    {
    is_zoomed = _zoomed;
    /*following copied from setup_plot so that a replot will re-calculate
     the ct when user has asked it to do so by specifying less than a
     ct of 1.0 like cps does. 09/96
     */
    if(_user->_mode != PlotGRID   && _user->_mode != PlotISO &&
       _user->_mode != PlotHEADER && _user->_mode != PlotCOLOR )
       {
       if(_user->_ct < 1.0 && _user->_ct > 0.0)
          {
          nin = nout = (int)(_ntot * _nsamp);
          trace_index = AryOffset / getSamplesInMemory();
          imageMedian( scale_amp, nin, 
                       trace_index,
                       &_median_scale,&median_value);
          _user->_ct = (_user->_ct * scale_amp) 
                          / median_value;
          if(show_warning)
            slpop = new SLErrorPop(graphicWidget(),"warning",CT_WARN);
          show_warning = False;
          }
       if(_user->_ct < 0.0)
          _user->_ct = (-_user->_ct) * scale_amp;
       }
    if(_user->_mode == PlotWONLY || _user->_mode == PlotWFILL)
       {
       stat = zoom(&zfact); 
       }
    else
       {
       if(!is_zoomed) _zoomed = True; /*so check_size will not free data*/
       if(!_zoom_factor)_zoom_factor = 1.0;
       stat = checkSize();
       if(stat == PlotSuccess) stat = colorReplot();
       }
    _zoomed = is_zoomed;
    } 

 return(stat);
 
}
