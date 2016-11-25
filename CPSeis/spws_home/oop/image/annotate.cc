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
 *Name   : annotatePlot
 *Purpose: Draw all of the labeling information of an image using Xlib
 *         drawing routines.
 *Author : Michael L. Sherrill
 *Date   : 07/91 (C++ version 4/97)
 *
 * Function Definition:
 * void annotate_plot (Widget   w, struct PlotImage *image, long HdrOffset)
 *
 * w         in         ID of active widget.
 * image     in         Structure defined in image.h that contains information 
 *                      such as traces per inch etc. Used for setting user
 *                      plotting parameters.
 * HdrOffset in         Index to annotate by the correct header while
 *                      doing movies.
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
 
#include <stdio.h>
#include <stdlib.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <math.h>
#include <limits.h>
#include "cprim.h"
#include "str.h"
#include "plot_image.hh"


void PlotImage::annotatePlot(long   HdrOffset)

{
 long invert_y;
 long i, j, l, m, t, labellen;
 long labellen2 = 0;
 long HeaderOne = 1, HeaderTwo = 2;
 long half_len;
 long NumToLabel;
 long TraceLabelIncr, TracesToLabel, firstlabel;
 long PrimaryLineInc, PrimaryTimingLine;
 long SecondaryTimingLine;
 long ist, iftl ;
 long doTwo = False;
 long x1, x2, y1, y2;
 long firstpixel;
 float addfraction;
 double PixelIncr, LabelValue, TotalTime, IncrOfLabels; 
 double yperpix;
 float first_timing_line, first_y;
 char label[80];
 char label2[80];
 Display *dpy;
 GC gc1, gc2;
 float depth_factor;
 long hdindex, oldy;
 long skip_traces = 0;
 long hoffset;
 float tmin,tmax;
 long junk_int;
 float junk_float;
 double *ypoints;
 long diff;
 long num_y_points;
 int can_draw;
 int do_right = False;
 int do_left  = True;
 float xfact = 1000.0;
 long frames;


  frames = max(1, _user->getFrames());

  if(!_user->getRightToLeft())
    firstpixel = getFirstTraceLocation();
  else
    firstpixel = _ntot * _trace_delta; 

  dpy = XtDisplay(_graphic);

  if(_user->getMode() >= PlotCOLOR) /*gray scale*/
     {
     gc1 = _gc1;
     gc2 = _gc2;
     }
  else   /*wiggles*/
     {
     gc1 = _bitmap_gc1;
     gc2 = _bitmap_gc2;
     }

  if(_can_overlay && _over_image != NULL)
     {
     gc1 = _gc1;
     gc2 = _gc2;
     XSetForeground(dpy,gc1, _black_pixel);
     XSetBackground(dpy,gc1, _white_pixel);
     XSetForeground(dpy,gc2, _black_pixel);
     XSetBackground(dpy,gc2, _white_pixel);
     }



/*********************** DRAW PLOT LABELS *****************************/
  drawPlotLabel (&(_pixmary[_cpixm]));
  if (_user->getLabelPlotOnly()) return;
/********************** END PLOT LABEL *******************************/



/*************************** START Y LABELS ***************************/
 if(_manual_annotate)
   {
   tmin = _manual_grid_y1;
   tmax = _manual_grid_y2;
   yperpix = _manual_y_value_per_pixel;
   annotateGridY(HdrOffset);
   }
 else
  {/*if this is an underlay make sure labels dont obstruct the overlay image*/
  if(_over_image)
     {
     if( (getLeftImageX() > getOverlayLeftImageX()) &&
         (getLeftImageX() + _ximage.width < 
          _over_image->getLeftImageX() + _over_image->_ximage.width) ) 
        goto no_y_labels;
     } 
  tmin = _tmin;
  tmax = _tmax;
  yperpix = _y_value_per_pixel;


  if(_user->getMode() == PlotGRID && tmin > tmax)
     _user->setInvertedYaxis(True);
  if(_user->getMode() == PlotHEADER && tmin > tmax)
     _user->setInvertedYaxis(True);
  if(_user->getMode() == PlotHEADER && tmin < tmax)
     _user->setInvertedYaxis(False);
  invert_y = _user->getInvertedYaxis();

  if(_user->getMode() == PlotISO && tmin > tmax)
     invert_y = True;

  if(!_user->getYannotationHeader()) /*use data to label by*/
    {
    if(!_user->getDepthMode())
       depth_factor = 1.0;
    else/*this is not a safe default, but cannot get from data*/
       depth_factor = xfact; 
    if(!_user->getPrimaryTimingLine())_user->setPrimaryTimingLine(tmax);
    if(!_user->getSecondaryTimingLine())
       _user->setSecondaryTimingLine(_user->getPrimaryTimingLine());

    /*make sure multiplier will not exceed size of a long*/
    while( max(tmin,tmax) * xfact > LONG_MAX ) xfact /= 10.0;

    PrimaryLineInc = PrimaryTimingLine = 
                       (long)(_user->getPrimaryTimingLine() * xfact + .5);
    SecondaryTimingLine = (long)(_user->getSecondaryTimingLine() * xfact + .5);
    addfraction = (tmin < 0.0) ? -.5 : .5;
    ist = (long)(tmin * xfact + addfraction);
                   /****do the bold primary y annotation****/
    if(tmin < 0.0 || invert_y)
       iftl = ist / PrimaryTimingLine * PrimaryTimingLine;
    else
       iftl = ((ist + PrimaryTimingLine - 1) / PrimaryTimingLine) 
            * PrimaryTimingLine;
    first_timing_line = (float) iftl / xfact;
    TotalTime    = tmax - tmin;
    PixelIncr    = (((float)PrimaryTimingLine) * (1.0 / xfact))
                 / yperpix;
    IncrOfLabels = _user->getPrimaryTimingLine();
    if(invert_y)
       {
       TotalTime         = -TotalTime;
       PixelIncr         = -PixelIncr;
       IncrOfLabels      = -IncrOfLabels;
       }
    NumToLabel = (long)((TotalTime / _user->getPrimaryTimingLine())
               - ((first_timing_line - tmin) / _user->getPrimaryTimingLine()));
    if(NumToLabel < 0) NumToLabel = (-NumToLabel);
    LabelValue   = first_timing_line; 
    sprintf(label,"%8.3f",LabelValue);
    labellen = strlen(label);
    x1 = _left_border;
    x2 = (_user->getMode() <  PlotCOLOR) ? _ximage.width + 
                                       _left_border
                                     : _ximage.width + 
                                       _left_border+5;
    y1 = y2 = (long)((first_timing_line - tmin)/yperpix + _top_border + .5); 
    first_y = y1 - _top_border;
    junk_int = (int)(tmin / _user->getPrimaryTimingLine());
    junk_float =  tmin / _user->getPrimaryTimingLine();
    if( (junk_float - junk_int) )
      PrimaryTimingLine = (long)(_user->getPrimaryTimingLine() * xfact + .5);
    else
      PrimaryTimingLine = (long)(tmin * xfact + .5);
    if(invert_y)
       {
       PrimaryTimingLine = (long)((tmin * xfact + .5));
       PrimaryLineInc    = -PrimaryLineInc;
       }     
    oldy = 0;
    num_y_points = NumToLabel + 1;
    ypoints = NULL;
    ypoints = (double *) calloc( 1, (int)((num_y_points * sizeof(double))) );
    if(ypoints == NULL)
       {
       printf("not enough memory for ypoints in annotate_plot\n");
       num_y_points = 0;
       }     
    for(i=0;i<=NumToLabel;i++)
       {
          if( (y1 >= _top_border) && 
              (y1 <= _top_border + _ximage.height))
             {
             if(_user->getMode() != PlotHEADER)
               {
               if(_user->getDrawYlines())
                 {
                 drawLine(dpy, _pixmary[_cpixm], 
                          gc2,(int)x1-5,(int)y1,(int)x2,(int)y2);
                 }
               else
                 {
                 drawLine(dpy, _pixmary[_cpixm], 
                           gc2,(int)x1-5,(int)y1,(int)x1,(int)y2);
                 drawLine(dpy, _pixmary[_cpixm], 
                           gc2,(int)x2-5,(int)y1,(int)x2,(int)y2);
                 }
               }
             else
               {
               drawLine(dpy, _pixmary[_cpixm], gc1,(int)x1-5,(int)y1,
                         (int)x2,(int)y2);
               }
             if(!invert_y)
                PrimaryTimingLine += PrimaryLineInc; 
             else
                PrimaryTimingLine = (long)(LabelValue * xfact + 
                                           PrimaryLineInc + .5);
             sprintf(label,"%8.3f",(LabelValue*depth_factor));
             labellen = strlen(label);
             /*see if we should draw the left side annotation*/
             if(_can_overlay && _over_image != NULL)
               {
               if(_dest_x >= _over_image->_dest_x )
                 do_left = False;
	       }
             if(do_left)
               drawString(dpy,_pixmary[_cpixm],gc2,
                          (int)(x1 - (_boldcharwidth * labellen + 5)),
                          (int)y1+5, label,(int)labellen);
             /*see if we should draw the right side annotation*/
             if(_can_overlay == False || _underlay_only)
                do_right = True;
             if(_can_overlay && _over_image != NULL)
	       {
               if( _ximage.width + _dest_x > 
                   _over_image->_ximage.width+_over_image->_dest_x
                  + _over_image->getLeftBorder() )
                do_right = True;
	       }
             if(do_right)
                  drawString(dpy,_pixmary[_cpixm],gc2, (int)x2,(int)y1+5,
                             label,(int)labellen);
             if(ypoints)ypoints[i] = LabelValue;
             }
       LabelValue += IncrOfLabels;
       y1 = y2 = (long)(((i + 1)*PixelIncr) + _top_border + .5 + first_y);
       }

            /*********** DO THE SECONDARY NON-BOLD ANNOTATION *********/
    if(tmin < 0.0 || invert_y)
       iftl = ist / SecondaryTimingLine * SecondaryTimingLine;
    else
       iftl = ((ist + SecondaryTimingLine - 1) / SecondaryTimingLine) 
            * SecondaryTimingLine;
    first_timing_line = (float) iftl / xfact;
    TotalTime    = tmax - tmin;
    PixelIncr    = (((float)SecondaryTimingLine) * (1.0 / xfact)) / yperpix;
    IncrOfLabels = _user->getSecondaryTimingLine();
    if(invert_y)
       {
       TotalTime         = -TotalTime;
       PixelIncr         = -PixelIncr;
       IncrOfLabels      = -IncrOfLabels;
       }
    NumToLabel = (long)((TotalTime / _user->getSecondaryTimingLine())
             - ((first_timing_line - tmin) / _user->getSecondaryTimingLine()));
    LabelValue   = first_timing_line; 
    sprintf(label,"%8.3f",LabelValue);
    labellen = strlen(label); 
    x1 = _left_border - 5;
    x2 = (_user->getMode() <  PlotCOLOR) ? _ximage.width + 
                                       _left_border
                                     : _ximage.width + 
                                       _left_border+5;
    y1 = y2 = (long)((first_timing_line - tmin) 
            / yperpix + _top_border + .5); 
    first_y = y1 - _top_border;
    junk_int = (int)(tmin / _user->getSecondaryTimingLine());
    junk_float =  tmin / _user->getSecondaryTimingLine();
    if( (junk_float - junk_int) )
      SecondaryTimingLine=(long)(_user->getSecondaryTimingLine() * xfact + .5);
    else
      SecondaryTimingLine = (long)(tmin * xfact + .5);
    if(invert_y)
       {
       SecondaryTimingLine = (long)(tmin * xfact + .5);
       }     
    oldy = 0;
    for(i=0;i<=NumToLabel;i++)
       {
       if( (y1 >= _top_border) &&
           (y1 <= _top_border + _ximage.height)) 
           /***** add this conditional to prevent overlap
            && (y1 - oldy > _fixedcharheight))
           ******/
          {
          can_draw = True;
          if(ypoints)/*make sure there is not already a primary label here*/
	     {
             for(m = 0; m < num_y_points; m++)
                if( (int)(LabelValue*xfact+.5) == (int)(ypoints[m]*xfact+.5) )
                   can_draw = False;
	     }
          oldy = y1;
          if(can_draw)
	     {
             if(_user->getDrawYlines())
               {
               drawLine(dpy, _pixmary[_cpixm], 
                          gc1,(int)x1-5,(int)y1,(int)x2,(int)y2);
               }
             else
               {
               drawLine(dpy, _pixmary[_cpixm], 
                         gc1,(int)x1-5,(int)y1,(int)x1,(int)y2);
               drawLine(dpy, _pixmary[_cpixm], 
                         gc1,(int)x2-5,(int)y1,(int)x2,(int)y2);
               }
             sprintf(label,"%8.3f",(LabelValue*depth_factor));
             labellen = strlen(label);
             if(_can_overlay && _over_image != NULL)
               {
               if(_dest_x >= _over_image->_dest_x)
                 do_left = False;
	       }
             if(do_left)
               drawString(dpy,_pixmary[_cpixm],gc1,
                          (int)(x1 - (_fixedcharwidth * labellen + 5)),
                          (int)y1+5,label,(int)labellen);
             if(_can_overlay && _over_image != NULL )
	       {
               do_right = False;
               if( _ximage.width + _dest_x > 
                   _over_image->_ximage.width+_over_image->_dest_x
                  +_over_image->getLeftBorder() )
               do_right = True;
	       }
             if(do_right)
                 drawString(dpy,_pixmary[_cpixm],gc1,(int)x2,(int)y1+5,
                            label,(int)labellen);
	     }
          }
       LabelValue += IncrOfLabels;
       y1 = y2 = (long)(((i + 1)*PixelIncr) + _top_border + .5 + first_y);
       }
    if(ypoints) free(ypoints);
    } /***** END NON-BOLD SECONDARY ANNOTATION ***********/
 else/***USE A Y HEADER TO LABEL BY, BOLD ANNOTATION NOT YET SPPORTED HERE****/
    {
    x1 = _left_border - 5;
    x2 = (_user->getMode() <  PlotCOLOR) ? _ximage.width +
                                       _left_border
                          	     : _ximage.width +
                                       _left_border + 5;
    oldy = 0;
    hdindex = _user->getYannotationHeader();
    for(i=0;i<_user->getNumberYlabels();i++)
       {
       y1 = y2 = (long)((_hd[hdindex] - tmin)
               / yperpix + _top_border + .5);
       if(_user->getDrawYlines())
         {
         drawLine(dpy, _pixmary[_cpixm], gc1,(int)x1-5,(int)y1,(int)x2,
                   (int)y2);
         }
       else
         {
         drawLine(dpy, _pixmary[_cpixm], gc1,(int)x1-5,(int)y1,(int)x1,
                   (int)y2);
         drawLine(dpy, _pixmary[_cpixm], gc1,(int)x2-5,(int)y1,(int)x2,
                   (int)y2);
         }
       /***** use this to prevent overlap
       if( (abs(y1 - oldy)) > _fixedcharheight)
          {
       ******/
          sprintf(label,"%8.3f",_hd[hdindex]);
          labellen = strlen(label);
          drawString(dpy,_pixmary[_cpixm],gc1,
                     (int)(x1 - (_fixedcharwidth * labellen + 5)),(int)y1+5,
                     label,(int)labellen);
          drawString(dpy,_pixmary[_cpixm],gc1,(int)x2+5,(int)y1+5,
                     label,(int)labellen);
          oldy = y1;
        /****
          }
        *****/
       hdindex += _nhdrs; 
       }
    }
  }
 no_y_labels:
/*************************** END Y LABELS ********************************/



/*************************** START X LABELS ******************************/
 switch(_user->getMode()) 
 {
 /*----- Trace type labels -----*/
 case PlotWONLY:
 case PlotWFILL:
 case PlotCOLOR:
 case PlotHEADER:
    if(_user->getXlabelIncrement() < 1) break;
    if(_user->getFirstTraceToAnnotate() < 1) _user->setFirstTraceToAnnotate(1);
    TraceLabelIncr = _user->getXlabelIncrement() * _trace_delta;
    /******this loop will prevent label overlap
    while(TraceLabelIncr < 8*_boldcharwidth) 
       {
       _user->getXlabelIncrement() *= 2;
       TraceLabelIncr = _user->getXlabelIncrement() * _trace_delta;
       }
    ********/
    TracesToLabel=(long)(((float)_ntot-(float)_user->getFirstTraceToAnnotate())
                   / (float)_user->getXlabelIncrement() + 1.0);
    if(_user->getMode() <  PlotCOLOR || _user->getMode() == PlotHEADER)
       if(!_user->getRightToLeft())
          x1 = x2 = (firstpixel + _left_border)
                  + ((_user->getFirstTraceToAnnotate() - 1) * _trace_delta);
       else
          x1 = x2 = (firstpixel + getRightBorder()
                  - (_trace_delta / 2))
                  - ((_user->getFirstTraceToAnnotate() - 1) * _trace_delta);
    else
       if(!_user->getRightToLeft())
          x1 = x2 = (firstpixel + _left_border
                  + (_trace_delta / 2)) 
                  + ((_user->getFirstTraceToAnnotate() - 1) * _trace_delta);
       else
          x1 = x2 = (firstpixel + getRightBorder()
                  - (_trace_delta / 2))
                  - ((_user->getFirstTraceToAnnotate() - 1) * _trace_delta);
    y1 = _top_border - 5;        
    y2 = _top_border;
    t = _user->getFirstTraceToAnnotate() - 1;
    for(i=0;i<TracesToLabel;i++) 
       {
       if( (x1 >= 1) &&
           (x1 <= _left_border + _ximage.width 
                                         + getRightBorder()))
	  {
          drawLine(dpy, _pixmary[_cpixm], gc1,(int)x1,(int)y1,
                   (int)x2,(int)y2,t);
	  }
       if(!_user->getRightToLeft())
          x1 = x2 = x1 + TraceLabelIncr;
       else
          x1 = x2 = x1 - TraceLabelIncr;
       if(x1 > _ximage.width + _left_border)
          i = TracesToLabel + 1; /*stop loop*/
       t += _user->getXlabelIncrement();
       }
    if(_user->getSecondaryAnnotationHeader())doTwo = True; /*2nd header word*/
    HeaderOne = (_first_trace_in_image - 1 
              + _user->getFirstTraceToAnnotate() - 1) 
              * _nhdrs + _user->getPrimaryAnnotationHeader() - 1;
    if(doTwo)HeaderTwo =(_first_trace_in_image - 1
                       + _user->getFirstTraceToAnnotate() -1)*_nhdrs 
                       + _user->getSecondaryAnnotationHeader() - 1;
    if(_user->getMode() <  PlotCOLOR || _user->getMode() == PlotHEADER)
       {
       if(!_user->getRightToLeft())
          firstlabel = (firstpixel + _left_border - 3) 
                     + ((_user->getFirstTraceToAnnotate() - 1) * _trace_delta);
       else
          firstlabel = (firstpixel + _left_border - 3
                     - (_trace_delta / 2))
                     - ((_user->getFirstTraceToAnnotate() - 1) * _trace_delta);
       }
    else
       {
       if(!_user->getRightToLeft())
          firstlabel = (firstpixel + _left_border - 3 
                     + (_trace_delta / 2))
                     + ((_user->getFirstTraceToAnnotate() - 1) * _trace_delta);
       else
          firstlabel = (firstpixel + _left_border - 3
                     - (_trace_delta / 2))
                     - ((_user->getFirstTraceToAnnotate() - 1) * _trace_delta);
       }
    sprintf(label,"%8.3f",_hd[HeaderOne+HdrOffset]);/*length header 1*/
    i = 0;  
    while(label[i] != '.')i++;
    labellen = i + 3;
    x1 = firstlabel - ((labellen - 1) * _boldcharwidth / 2); 
    t  = _user->getFirstTraceToAnnotate() - 1;
    if(doTwo)
       { 
       sprintf(label2,"%8.3f",_hd[HeaderTwo+HdrOffset]);/*length header 2*/
       j = 0;      
       while(label2[j] != '.')j++;
       labellen2 = j + 3;
       x2 = firstlabel - ((labellen2 - 1) * _boldcharwidth / 2);
       }
    y1 = _top_border - 31;
    if(doTwo) y2 = _top_border - 18;
    /*Now draw trace labels*/
    for(l=0;l<TracesToLabel;l++)
       {
       if( (x1 >= 1) &&
           (x1 <= _left_border + _ximage.width 
                                         + getRightBorder()))
	 {
         drawString(dpy, _pixmary[_cpixm], gc2,
                    (int)x1, (int)y1, label, (int)labellen, t);
         }
       if(!_user->getRightToLeft())
          x1 += (labellen * _boldcharwidth);
       else
          x1 -= (labellen * _boldcharwidth);
       if(doTwo)
          {
          if( (x1 >= 1) &&
           (x1 <= _left_border + _ximage.width 
                                         + getRightBorder()))
	    {
            drawString(dpy, _pixmary[_cpixm], gc2,
                       (int)x2, (int)y2, label2, (int)labellen2, t);
	    }
          if(!_user->getRightToLeft())
             x2 += (labellen2 * _boldcharwidth);
          else
             x2 -= (labellen2 * _boldcharwidth);
          }
       if(l + 1 < TracesToLabel)
         HeaderOne += (_user->getXlabelIncrement() * _nhdrs);
       if(doTwo)
         {
          if(l + 1 < TracesToLabel)
            HeaderTwo += (_user->getXlabelIncrement() * _nhdrs);
         }
       sprintf(label,"%8.3f",_hd[HeaderOne+HdrOffset]);
       i = 0;
       while(label[i] != '.')i++;
       labellen = i + 3;
       if(!_user->getRightToLeft())
          x1 = ((l+1) * TraceLabelIncr) + firstlabel 
             - ((labellen-1) * _boldcharwidth / 2);
       else
          x1 = firstlabel - ((l+1) * TraceLabelIncr)
             - ((labellen-1) * _boldcharwidth / 2);
       if(doTwo)
          {
          sprintf(label2,"%8.3f",_hd[HeaderTwo+HdrOffset]);
          j = 0;
          while(label2[j] != '.')j++;
          labellen2 = j + 3;
          if(!_user->getRightToLeft())
             x2 = ((l+1) * TraceLabelIncr) + firstlabel
                - ((labellen2-1) * _boldcharwidth / 2);
          else
             x2 = firstlabel - ((l+1) * TraceLabelIncr)
                - ((labellen2-1) * _boldcharwidth / 2);
          }
       t += _user->getXlabelIncrement();
       }/*end for*/ 
  break;
 /*---- End trace type x labels ----*/



 /*---- Semblance grid or contour type x labels ------*/
 case PlotCONTOUR:
 case PlotSEMB:
 case PlotGRID:
      annotateGridX(HdrOffset);
      break;


 /*---- end semblance grid or contour type x labels ----*/


 
 /*---- draw iso velocity and header type x labels ----*/
 case PlotISO:
      if(!_user->getUserHeaderAnnotation())
	{
        annotateGridX(HdrOffset);
        break;
	}
      for(i=1;i<=_cpixm;i++) skip_traces += _tpnl[i-1];
      hoffset = _nhdrs * skip_traces; 
      labellen = 8;
      half_len = (labellen / 2) * _boldcharwidth;
      y1 = _top_border - 5;
      y2 = _top_border + _ximage.height;
      if(!_user->getDrawXlines()) y2 -= _ximage.height;
      x2 = 0; /*for first time init*/
      for(i=_user->getFirstTraceToAnnotate()-1;i<_tpnl[_cpixm];
                                           i+=_user->getXlabelIncrement())
         {
         x1 = (long)((_hd[
                       i*_nhdrs+_user->getPrimaryAnnotationHeader()-1+hoffset]
                       - _grid_x1)/_x_value_per_pixel + _left_border + .5);
         if(!_user->getRightToLeft())
            diff = x1 - x2;
         else
            diff = x2 - x1;
         /*make sure 1st time will work*/
         if(i == _user->getFirstTraceToAnnotate()-1)
            diff = _boldcharwidth * labellen + _boldcharwidth + 1;
         if(diff > (_boldcharwidth * labellen + _boldcharwidth) )
            {
            if(x1 >= _left_border && 
               x1 <= _ximage.width + _left_border)
               {
               drawLine(dpy, _pixmary[_cpixm], gc1, (int)x1,(int)y1,(int)x1,
                         (int)y2, i);
               sprintf(label,"%8.3f",
                   _hd[i*_nhdrs+_user->getXLabelHeader()-1+hoffset]);
               drawString(dpy, _pixmary[_cpixm], gc2,
                          (int)(x1 - half_len), (int)(y1 - _boldcharheight),
                          label, (int)labellen, i);
               x2 = x1;
               }
            }//not enough room for label but need to draw all vertical iso lines
         else if(_user->getDrawXlines() && _user->getMode() == PlotISO)
           {
           drawLine(dpy, _pixmary[_cpixm], gc1, (int)x1,(int)y1,(int)x1,
                         (int)y2, i);
           //x2 = x1;
           }
         } 
 break;
 /*---- end iso velocity type x labels ----*/
      
 } 

/************************* END OF ALL X LABELING ********************/



/***draw a bounding rectangle around image if this is a user defined grid*/

 if(_user->getMode() == PlotGRID || _user->getMode() == PlotHEADER)
 {
  x1 = _left_border-1;
  y1 = _top_border-1;
  x2 = _ximage.width+1;
  y2 = _ximage.height+1;
  XDrawRectangle(dpy, _pixmary[_cpixm],gc2,(int)x1,(int)y1,(int)x2,(int)y2);
  /*_user->getInvertedYaxis() =  False;*/
 }
  


}










/**************************************************************************
 *********** A user defined x and y axis annotation function **************
 **************************************************************************/

void PlotImage::annotateGrid( float            user_x1,
                              float            user_x2,
                              float            user_y1,
                              float            user_y2)

{
 Display *dpy;
 long labellen;
 char label[20];
 long x1,x2,y1,y2;
 long i;
 long data_samples;

    if(_can_overlay) return;

    dpy = XtDisplay(_graphic);

/*If no annotation return*/
    if(_user->getPrimaryTimingLine() == 0.0 && 
       _user->getSecondaryTimingLine() == 0.0) return;

    data_samples = (long)((_tmax-_tmin)/_user->getGlobals().srval);
    if(data_samples < 0) data_samples = (-data_samples);

/*draw white grid rectangle on black background*/
    XSetForeground(dpy, _gc2, _black_pixel);
    XFillRectangle(dpy, _pixmary[_cpixm], _gc2, 0,0,
                   (_ximage.width
                   +(int)(_left_border+getRightBorder())),
                   (_ximage.height
                   +(int)(_top_border+getBottomBorder())) );

    XSetForeground(dpy, _gc2, _white_pixel);
    XDrawRectangle(dpy, _pixmary[_cpixm],_gc2,
                   (int)_left_border-7,(int)_top_border-7,
                   _ximage.width+15,_ximage.height+14);

/************** x annotation **************/
/*upper left x tic mark*/
    x1 = x2 = _left_border;
    y1 = _top_border - _boldcharheight;
    y2 = _top_border;
    drawLine(dpy, _pixmary[_cpixm],_gc2,(int)x1,(int)y1,(int)x2,(int)y2);

/*upper right x tic mark*/
    x1 = x2 = x1 + _ximage.width;
    drawLine(dpy, _pixmary[_cpixm],_gc2,(int)x1,(int)y1,(int)x2,(int)y2);

/*upper left x label*/
    sprintf(label,"%8f",user_x1);
    i = 0;
    while(label[i] != '.')i++;
    labellen = i + 3;
    x1 = _left_border - ((labellen - 1) * _boldcharwidth / 2);
    y1 = _top_border -  (long)(1.5*_boldcharheight);
    drawString(dpy,_pixmary[_cpixm],_gc2,(int)x1,(int)y1,
                label,(int)labellen);

/*upper right x label*/
    sprintf(label,"%8f",user_x2);
    i = 0;
    while(label[i] != '.')i++;
    labellen = i + 3;
    x1 = _left_border + _ximage.width
       - ((labellen - 1) * _boldcharwidth / 2);
    drawString(dpy,_pixmary[_cpixm],_gc2,(int)x1,(int)y1,
                label,(int)labellen);

/************* y annotation ***********/
/*upper left tic mark*/
    x1 = _left_border - (2*_boldcharwidth);
    x2 = x1 + (2*_boldcharwidth);
    y1 = y2 = _top_border;
    drawLine(dpy, _pixmary[_cpixm],_gc2,(int)x1,(int)y1,(int)x2,(int)y2);

/*lower left tic mark*/
    y1 = y2 = y1 + _ximage.height;
    if(data_samples > 1)
      drawLine(dpy, _pixmary[_cpixm],_gc2,(int)x1,(int)y1,(int)x2,(int)y2);

/*lower right tic mark*/
    x1 = _left_border + _ximage.width;
    x2 = x1 + (2*_boldcharwidth);
    if(data_samples > 1)
      drawLine(dpy, _pixmary[_cpixm],_gc2,(int)x1,(int)y1,(int)x2,(int)y2);

/*upper right tic mark*/
    y1 = y2 = y1 - _ximage.height;
    drawLine(dpy, _pixmary[_cpixm],_gc2,(int)x1,(int)y1,(int)x2,(int)y2);

/*upper left label*/
    sprintf(label,"%8f",user_y1);
    i = 0;
    while(label[i] != '.')i++;
    labellen = i + 3;
    x1 = _left_border - (long)((labellen+2.5) * _boldcharwidth);
    y1 = _top_border + (long)(0.25 * _boldcharheight);
    drawString(dpy,_pixmary[_cpixm],_gc2,(int)x1,(int)y1,
                label,(int)labellen);

/*upper right label*/
    sprintf(label,"%8f",user_y1);
    i = 0;
    while(label[i] != '.')i++;
    labellen = i + 3;
    x1 = _left_border + _ximage.width 
                                + (long)(2.5*_boldcharwidth);
    drawString(dpy,_pixmary[_cpixm],_gc2,(int)x1,(int)y1,
                label,(int)labellen);

/*lower left label*/
    if(data_samples > 1)
      {
      sprintf(label,"%8f",user_y2);
      i = 0;
      while(label[i] != '.')i++;
      labellen = i + 3;
      x1 = _left_border - (long)((labellen+2.5) * _boldcharwidth);
      y1 = y1 + _ximage.height;
      drawString(dpy,_pixmary[_cpixm],_gc2,(int)x1,(int)y1,
                  label,(int)labellen);
      }

/*lower right label*/
    if(data_samples > 1)
      {
      sprintf(label,"%8f",user_y2);
      i = 0;
      while(label[i] != '.')i++;
      labellen = i + 3;
      x1= _left_border + _ximage.width + (long)(2.5*_boldcharwidth);
      drawString(dpy,_pixmary[_cpixm],_gc2,(int)x1,(int)y1,
                  label,(int)labellen);
      }


}







/**************************************************************************
 *********** A grid defined x axis annotation method         **************
 **************************************************************************/
void PlotImage::annotateGridX(long HdrOffset)
{
 long labellen2 = 0;
 Display *dpy;
 long labellen = 0;
 int max_char = 20;
 int num_decimals = 3;
 char label[20];
 long x1,x2,y1,y2,oldx;
 long i;
 GC gc1, gc2;
 float vel_sign;
 long temp_log;
 double vel_log;
 float fraction;
 float vel_interval;
 float s1;
 float an;
 float vel[3];
 int n[5];
 long max_log;
 float VelLabelIncr;
 float vmin, vmax;
 double vel_range;
 long NumToLabel;
 long half_len;
 float vel_loc;
 char label2[30];
 float ivel, tempx1;
 float tempivel;

  if(_vel_min == _vel_max)  return;

  dpy = XtDisplay(_graphic);

  if(_user->getMode() >= PlotCOLOR) /*gray scale*/
     {
     gc1 = _gc1;
     gc2 = _gc2;
     }
  else   /*wiggles*/
     {
     gc1 = _bitmap_gc1;
     gc2 = _bitmap_gc2;
     }

  if(_can_overlay)
     {
     gc1 = _gc1;
     gc2 = _gc2;
     XSetForeground(dpy,gc1, _black_pixel);
     XSetBackground(dpy,gc1, _white_pixel);
     XSetForeground(dpy,gc2, _black_pixel);
     XSetBackground(dpy,gc2, _white_pixel);
     }

  vel[0] = _vel_min;
  vel[1] = _vel_max;
  vel_sign = (_vel_max - _vel_min > 0.0) ? 1.0 : -1.0;
  if(_user->getSymetricalAnnotation())
    vel_range = max(_tmin,_tmax) 
              - min(_tmin,_tmax);
  else
    vel_range = max(_vel_min,_vel_max) 
              - min(_vel_min,_vel_max);
  vel_log = log10( vel_range );
  temp_log = (long)vel_log;
  if( (float)temp_log > vel_log) temp_log = temp_log - 1;   
  fraction = vel_log - temp_log;
  if(fraction < 0.23)          
    {
    temp_log = temp_log - 1;
    vel_interval = 2.0 * (pow(10.0, (float)temp_log));
    }
  else if (fraction < 0.57)          
    {
    temp_log = temp_log - 1;
    vel_interval = 5.0 * (pow(10.0, (float)temp_log));
    }
  else if (fraction < 0.90)          
    {
    vel_interval = pow(10.0, (float)temp_log);
    }
  else
    {
    vel_interval = 2.0 *(pow(10.0, (float)temp_log));
    }
  max_log = max(-temp_log,0);

  if(_vel_min > _vel_max) vel_interval = (-vel_interval);

  for(i=0;i<2;i++)
     {
     s1 = 2.0 * (1.5-(i+1))*vel_sign;
     an = vel[i]  / vel_interval;
     n[i] = (int)an;
     if (s1*n[i] < s1*an) n[i] = (int)(n[i] + s1);       
     }

  NumToLabel = (abs(n[1]-n[0])) + 1;
  vmin  = _vel_min +( n[0]*vel_interval-_vel_min);
  vmax  = _vel_min +( n[1]*vel_interval-_vel_min);
  VelLabelIncr = vel_interval * _velfactor; 

  if(!useLogarithmicX())
    {
//  x1 = (long)((vmin-_vel_min) * _velfactor +_left_border);
//  ehs 03may99
    tempx1 = (vmin-_vel_min) * _velfactor + _left_border;
    x1 = (long) floor(tempx1 + 0.5);
    }
  else
    {
    if(vmin > 0.0)
      {
      if(!_manual_annotate)
        x1 = getXpixelFromX(vmin);
      else
        x1 = getManualXpixelFromX(vmin);
      }
    }

  y1 = _top_border - 6;
  x2 = _left_border;
  y2 = _top_border - 35;

    /*vel location annotation*/
  i = (_nhdrs * (_first_trace_in_image - 1)) 
    + (_user->getPrimaryAnnotationHeader() - 1);
  vel_loc = _hd[i+HdrOffset];
  if(_user->getMode() != PlotGRID && _user->getMode() != PlotISO )
    {
    sprintf(label2,"HDR# %d ID = %8.3f",_user->getPrimaryAnnotationHeader(),
            vel_loc);
    labellen2 = strlen(label2);
    drawString(dpy, _pixmary[_cpixm], gc2,
                (int)x2, (int)y2, label2, (int)labellen2);
    y2 += 10;
    i = (_nhdrs * (_first_trace_in_image - 1))
           + (_user->getSecondaryAnnotationHeader() - 1);
    vel_loc = _hd[i+HdrOffset];
    sprintf(label2,"HDR# %d ID = %8.3f",_user->getSecondaryAnnotationHeader(),
            vel_loc);
    labellen2 = strlen(label2);
    drawString(dpy, _pixmary[_cpixm], gc2,
                     (int)x2, (int)y2, label2, (int)labellen2);
    }

  oldx = 0;
  for(i=0;i<NumToLabel;i++) 
    {
    if(!_user->getDrawXlines())
      {
      y1 = _top_border-5;
      y2 = _top_border;  
      }
    else
      {
      y1 = _top_border-5;
      y2 = y1 + 5 + _ximage.height;
      } 
    if(!i)
      {
/*
      convert_ff2ss(&vmin, label, &max_char, &num_decimals);
*/
      str_ff2ss(vmin, label, max_char, num_decimals);
      }
    else
      {
      tempivel = vmin + i * vel_interval;
/*
      convert_ff2ss(&tempivel, label, &max_char, &num_decimals);
*/
      str_ff2ss(tempivel, label, max_char, num_decimals);
      }
      
    labellen = strlen(label);
    half_len = (long) ( ((float)labellen / 2.0) * _boldcharwidth);
    
    if((x1 - oldx > 2 * (half_len+1)) && 
       (x1 < _ximage.width + _left_border) &&
       (x1 >= _left_border) )
      {
      ivel = vmin + i * vel_interval;
/*
      convert_ff2ss(&ivel, label, &max_char, &num_decimals);
*/
      str_ff2ss(ivel, label, max_char, num_decimals);
      labellen = strlen(label);
      half_len = (long)( ((float)labellen / 2.0) * _boldcharwidth);
      drawLine(dpy, _pixmary[_cpixm],gc1,(int)x1,(int)y1,(int)x1,(int)y2);
      drawString(dpy, _pixmary[_cpixm], gc2,
                 (int)(x1 - half_len), (int)y1, label, (int)labellen);
      oldx = x1;
      }
    if(!useLogarithmicX())
      {
      tempx1 += VelLabelIncr;
      //if(!_user->getRightToLeft())
      x1 = x2 = (long) floor(tempx1 + 0.5);
        //else
        // x1 = x2 = (long) (tempx1 - 0.5);
      }
    else
      {
      if(vmin + ((i+1) * vel_interval) > 0.0)
        {
        if(!_manual_annotate)
          x1 = getXpixelFromX(vmin + ((i+1) * vel_interval));
        else
          x1 = getManualXpixelFromX(vmin + ((i+1) * vel_interval));
        }
      }     
    }
}


/**************************************************************************
 *********** A grid defined y axis annotation method         **************
 **************************************************************************/
void PlotImage::annotateGridY(long /*HdrOffset*/)
{
 Display *dpy;
 long labellen = 0;
 int max_char = 20;
 int num_decimals = 3;
 char label[20];
 long y1,y2,oldy;
 long line_x1, line_x2, label_x1;
 long i;
 GC gc1, gc2;
 float tsign;
 long temp_log;
 double tlog;
 float fraction;
 float interval;
 float s1;
 float an;
 float vel[3];
 int n[5];
 long max_log;
 float label_incr;
 float ymin, ymax;
 double range;
 long NumToLabel;
 float ival, tempy1;
 float tmin, tmax;
 int half_height;
 int line_offset = 8;

  if(!_manual_annotate)
    {
    tmin = _tmin;
    tmax = _tmax;
    }
  else
    {
    tmin = _manual_grid_y1;
    tmax = _manual_grid_y2;
    }
  if(tmin == tmax)  return;

  dpy = XtDisplay(_graphic);

  if(_user->getMode() >= PlotCOLOR) /*gray scale*/
     {
     gc1 = _gc1;
     gc2 = _gc2;
     }
  else   /*wiggles*/
     {
     gc1 = _bitmap_gc1;
     gc2 = _bitmap_gc2;
     }

  if(_can_overlay)
     {
     gc1 = _gc1;
     gc2 = _gc2;
     XSetForeground(dpy,gc1, _black_pixel);
     XSetBackground(dpy,gc1, _white_pixel);
     XSetForeground(dpy,gc2, _black_pixel);
     XSetBackground(dpy,gc2, _white_pixel);
     }

  vel[0] = tmin;
  vel[1] = tmax;

  tsign = (tmax - tmin > 0.0) ? 1.0 : -1.0;
  if(_user->getSymetricalAnnotation())
    range = max(_vel_min,_vel_max) - min(_vel_min,_vel_max);
  else
    range = max(tmin, tmax) - min(tmin,tmax);
  tlog = log10( range );
  temp_log = (long)tlog;
  if( (float)temp_log > tlog) temp_log = temp_log - 1;   
  fraction = tlog - temp_log;
  if(fraction < 0.23)          
    {
    temp_log = temp_log - 1;
    interval = 2.0 * (pow(10.0, (float)temp_log));
    }
  else if (fraction < 0.57)          
    {
    temp_log = temp_log - 1;
    interval = 5.0 * (pow(10.0, (float)temp_log));
    }
  else if (fraction < 0.90)          
    {
    interval = pow(10.0, (float)temp_log);
    }
  else
    {
    interval = 2.0 *(pow(10.0, (float)temp_log));
    }
  max_log = max(-temp_log,0);

  if(tmin > tmax) interval = (-interval);

  for(i=0;i<2;i++)
     {
     s1 = 2.0 * (1.5-(i+1))*tsign;
     an = vel[i]  / interval;
     n[i] = (int)an;
     if (s1*n[i] < s1*an) n[i] = (int)(n[i] + s1);       
     }

  NumToLabel = (abs(n[1]-n[0])) + 1;
  ymin  = tmin +( n[0]*interval-tmin);
  ymax  = tmin +( n[1]*interval-tmin);
  label_incr = interval / _manual_y_value_per_pixel;
  if(!useLogarithmicY())
    {
//  y1 = (long)((ymin-tmin) / _manual_y_value_per_pixel +_top_border);
//  ehs 03may99
    tempy1 = (ymin-tmin) / _manual_y_value_per_pixel + _top_border;
    y1 = (long)(floor(tempy1 + 0.5));
    }
  else
    {
    if(ymin > 0.0)
      {
      if(!_manual_annotate)
        y1 = getYpixelFromY(ymin);
      else
        y1 = getManualYpixelFromY(ymin);
      }
    }
  y2 = y1;
  line_x1  = _left_border - line_offset;
  ival = max(ymin,ymax); 
/*
  convert_ff2ss(&ival, label, &max_char, &num_decimals);
*/
  str_ff2ss(ival, label, max_char, num_decimals);
  labellen = strlen(label);
  label_x1 = _left_border - (labellen * _boldcharwidth);
  line_x2  = _left_border;
  half_height = (int)((float)_boldcharheight / 2.0);
  
  oldy = 0;
  for(i=0;i<NumToLabel;i++) 
    {
    if(y1 >= getTopBorder() && y1 <= getTopBorder() + _ximage.height)
      {
      drawLine(dpy, _pixmary[_cpixm],gc1,(int)line_x1,(int)y1,
              (int)line_x2,(int)y2);
      drawLine(dpy, _pixmary[_cpixm],gc1,(int)(line_x2 + _ximage.width),
              (int)y1,(int)(line_x2 + _ximage.width + line_offset - 2),
              (int)y2);
      if(_user->getDrawYlines())
        drawLine(dpy, _pixmary[_cpixm],gc1,(int)_left_border,(int)y1,
                (int)(_left_border + _ximage.width),(int)y2);
      }
    if((y1 - oldy >  _boldcharheight ) && 
       (y1 <= _ximage.height + _top_border) &&
       (y1 >= _top_border) )
      {
      ival = ymin + i * interval;
/*
      convert_ff2ss(&ival, label, &max_char, &num_decimals);
*/
      str_ff2ss(ival, label, max_char, num_decimals);
      labellen = strlen(label);
      label_x1 = _left_border - (labellen * _boldcharwidth) - line_offset;
      drawString(dpy, _pixmary[_cpixm], gc2,
                 (int)label_x1, (int)y1 + half_height, label, (int)labellen);
      drawString(dpy, _pixmary[_cpixm], gc2,
                  (int)(_left_border + _ximage.width + line_offset),
                  (int)y1+half_height, label, (int)labellen);
      oldy = y1;
      }
    if(!useLogarithmicY())
      {
      tempy1 += label_incr;
      y1 = y2 = (long) floor(tempy1 + 0.5);
      }
    else
      {
      if(ymin + ((i+1) * interval) > 0.0)
        {
        if(!_manual_annotate)
          y1 = y2 = getYpixelFromY(ymin + ((i+1) * interval));
        else
          y1 = y2 = getManualYpixelFromY(ymin + ((i+1) * interval));
        }
      } 
   }

}



void PlotImage::drawString(Display *dpy, Pixmap pixmap, GC gc, int x1, int y1,
                           char *string, int length, long trace_index)
{

  if(trace_index > -1 && usingSelector())
    {
    if(trace_index + 1 > getSelectorNumTraces(getCurrentPanel()))
       return;
    }

    XDrawString(dpy, pixmap, gc, x1, y1, string, length);

}


void PlotImage::drawLine(Display *dpy, Pixmap pixmap, GC gc, int x1, int y1,
                          int x2, int y2, long trace_index)
{

  if(trace_index > -1 && usingSelector())
    {
    if(trace_index + 1 > getSelectorNumTraces(getCurrentPanel()))
      return;
    }

  XDrawLine(dpy, pixmap, gc, x1, y1, x2, y2); 
}


//Method to draw a label in the annotation area
void PlotImage::drawLabel(char *label, long frame, int x, int y,
                         Boolean is_horizontal, 
                         Boolean auto_position)
{
  if(is_horizontal)
    drawHorizontalLabel(label, frame, x, y, auto_position);
  else
    drawVerticalLabel(label, frame, x, y, auto_position);
    
}


//Method to draw a horizontal label in the annotation area
//Note this method is not very smart about erasing a previous
//label. All it does is assume subsequent labels will start at the
//same position.
void PlotImage::drawHorizontalLabel(char *label, long frame, int x, int y,
                                    Boolean auto_position)
{
  GC temp_gc;
  Screen *scr;
  Display *dpy;
  int x1, y1, ynext;
  int affected_x1, affected_width;
  int affected_y1, affected_height;
  int labellen;
  long i;

  scr = XtScreen(_graphic);
  dpy = XtDisplay(_graphic);

  if(_ximage.depth == 1)
     {
     temp_gc = _bitmap_gc2;
     }
  else   
     {
     temp_gc = _gc2;
     }

  
  


  // auto position means to draw the label in the upper left annotation area
  if(auto_position)
    {
    x1 = (int)max(0,_left_border);
    y1 = (int)_boldcharheight; 
    }
  else// use specified location
    {
    x1 = x;
    y1 = y;
    }


  //Erase the entire horizontal area in case of previous label
  if(_ximage.depth == 1)
    {
    XSetForeground(dpy, temp_gc, 0);
    XSetBackground(dpy, temp_gc, 1);
    }
  else
    {
    XSetForeground(dpy, temp_gc, _white_pixel);
    XSetBackground(dpy, temp_gc, _black_pixel);
    }
  XFillRectangle(dpy, _pixmary[frame], temp_gc, 0, 
                 y1 - (_boldcharheight / 2),
                 _graph_width, _boldcharheight);
  if(_ximage.depth == 1)
    {
    XSetForeground(dpy, temp_gc, 1);
    XSetBackground(dpy, temp_gc, 0);
    }
  else
    {
    XSetForeground(dpy, temp_gc, _black_pixel);
    XSetBackground(dpy, temp_gc, _white_pixel);
    }


  labellen = strlen(label);

  drawString(dpy,_pixmary[frame],temp_gc, x1, y1, label, labellen);

  //compute the affected region to redraw
  affected_x1     = (int)(max(0, x1 - _boldcharwidth));
  affected_y1     = (int)(max(0, y1 - _boldcharheight));
  affected_width  = (int)(labellen * _boldcharwidth + _boldcharwidth);
  affected_height = (int)(2 * _boldcharheight);

   /*copy the annotation to screen*/
  if(_user->getMode() >= PlotCOLOR) /*color*/
     {
     XCopyArea(  dpy, _pixmary[frame], XtWindow(_graphic),
                 temp_gc, affected_x1, affected_y1, affected_width,
                 affected_height, affected_x1, affected_y1);
     }
  else   /*wiggles*/
     {
     XCopyPlane(  dpy, _pixmary[frame], XtWindow(_graphic),
                  temp_gc, affected_x1, affected_y1, affected_width,
                  affected_height, affected_x1, affected_y1, 1);
  
     }

}



//Method to draw a vertical label in the annotation area
//Note this method is not very smart about erasing a previous
//label. All it does is assume subsequent labels will start at the
//same position.
void PlotImage::drawVerticalLabel(char *label, long frame, int x, int y,
                                  Boolean auto_position)
{
  GC temp_gc;
  Screen *scr;
  Display *dpy;
  int x1, y1, ynext;
  int affected_x1, affected_width;
  int affected_y1, affected_height;
  int labellen;
  long i;

  scr = XtScreen(_graphic);
  dpy = XtDisplay(_graphic);

  if(_ximage.depth == 1)
     {
     temp_gc = _bitmap_gc2;
     }
  else   
     {
     temp_gc = _gc2;
     }



  // auto position means to draw the label in the upper left annotation area
  if(auto_position)
    {
    x1 = (int)max(0,_left_border);
    y1 = (int)_boldcharheight; 
    }
  else// use specified location
    {
    x1 = x;
    y1 = y;
    }

  //Erase the entire vertical area in case a previous label exists
  if(_ximage.depth == 1)
       {
       XSetForeground(dpy, temp_gc, 0);
       XSetBackground(dpy, temp_gc, 1);
       }
    else
       {
       XSetForeground(dpy, temp_gc, _white_pixel);
       XSetBackground(dpy, temp_gc, _black_pixel);
       }
    XFillRectangle(dpy, _pixmary[frame], temp_gc, x1 - _boldcharwidth,
                   0, 3 * _boldcharwidth, _graph_height);
    if(_ximage.depth == 1)
       {
       XSetForeground(dpy, temp_gc, 1);
       XSetBackground(dpy, temp_gc, 0);
       }
    else
       {
       XSetForeground(dpy, temp_gc, _black_pixel);
       XSetBackground(dpy, temp_gc, _white_pixel);
       }



  labellen = strlen(label);

   ynext = y1;
   for(i = 0; i < labellen; i++)
      {
      drawString(dpy,_pixmary[frame],temp_gc, x1, ynext, &label[i], 1);
      ynext += (int)(_boldcharheight + 1);
      }

  //compute the affected region to redraw
  affected_x1     = (int)(max(0, x1 - _boldcharwidth));
  affected_y1     = (int)(max(0, y1 - _boldcharheight));
  affected_width  = (int)(_boldcharwidth * 2);
  affected_height = (int)( ((labellen + 1)* _boldcharheight));

   /*copy the annotation to screen*/
  if(_user->getMode() >= PlotCOLOR) /*color*/
     {
     XCopyArea(  dpy, _pixmary[frame], XtWindow(_graphic),
                 temp_gc, affected_x1, affected_y1, affected_width,
                 affected_height, affected_x1, affected_y1);
     }
  else   /*wiggles*/
     {
     XCopyPlane(  dpy, _pixmary[frame], XtWindow(_graphic),
                  temp_gc, affected_x1, affected_y1, affected_width,
                  affected_height, affected_x1, affected_y1, 1);
  
     }

}

void PlotImage::drawPlotLabel (Pixmap *pixmap)
{
  long labellen;
  long x1, y1;
  Display *dpy;
  GC gc;

  char label[200];

  dpy = XtDisplay (_graphic);

  if (_user->getMode() >= PlotCOLOR) { /*gray scale*/
     gc = _gc2;
  }
  else { /*wiggles*/
     gc = _bitmap_gc2;
  }

  if (_can_overlay && _over_image != NULL) {
     XSetForeground (dpy, gc, _black_pixel);
     XSetBackground (dpy, gc, _white_pixel);
  }

  labellen = strlen (_user->getPlotLabel());
  assert (labellen < 200);
  x1 = _user->getPlotLabelX ();
  y1 = _user->getPlotLabelY ();

  if (!labellen && _can_overlay && _over_image != NULL) {
    labellen = strlen (_over_image->_user->getFilename());
    assert (labellen < 200);
    if (labellen > 0) {
      strcpy (label, _over_image->_user->getFilename());
    }
  }
  else {
    strcpy (label, _user->getPlotLabel());
  }

  if (x1 == 0 && y1 == 0) { /*default, put both labels at bottom left*/
    x1 = _left_border;
    y1 = _ximage.height + _top_border + _boldcharheight;
    if (_can_overlay && _over_image != NULL) y1 += _boldcharheight;
    drawString (dpy, *pixmap, gc, (int)x1, (int)y1, label, (int)labellen);
    labellen = strlen (_user->getFilename());
    y1 += _boldcharheight;
    drawString (dpy, *pixmap, gc, (int)x1, (int)y1, _user->getFilename(),
      (int)labellen);
  }
  else { /*put plotlabel elsewhere and filename at bottom left*/
    drawString (dpy, *pixmap, gc, (int)x1, (int)y1, label, (int)labellen);
    x1 = _left_border;
    y1 = _ximage.height + _top_border + 2*_boldcharheight;
    labellen = strlen (_user->getFilename());
    drawString (dpy, *pixmap, gc, (int)x1, (int)y1, _user->getFilename(),
      (int)labellen);
  }
}
