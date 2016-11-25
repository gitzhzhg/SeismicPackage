//========================= COPYRIGHT NOTICE ================================
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
//====  CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.      ========
//====   PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK       ========
//======================== COPYRIGHT NOTICE =================================


//=========================================================================
//========== Hardcopy class that draws the semblance contours    ==========
//========== Author Michael L. Sherrill 03/98                    ==========
//=========================================================================


#include <math.h>
#include "tfdefs.h"
#include "image_amplitude_recovery.hh"
#include "hardcopy/hardcopy_raster.hh"
#include "hardcopy/hardcopy_plot.hh"
#include "hardcopy/hardcopy_color_bar.hh"
#include "sp/seis_plot.hh"
#include "hardcopy/hardcopy_contour.hh"


#define MAX_LABEL_LENGTH 11.0

//===========================================================================
//========================== Constructor                          ===========
//========================== If frame_number is set               ===========
//========================== contouring occurs on construction    ===========
//===========================================================================
HardCopyContour::HardCopyContour( HardCopyPlot  *hc,
                                  SeisPlot      *sp,
                                  float         dots_per_inch,
                                  long          frame_number)
{
int cgm_index = 0;
long i; 



  _hc         = hc;
  _sp         = sp;
  _status     = False;
  _dots_per_inch = dots_per_inch;
  _amp_recovery = _sp->getAmplitudeRecovery();
  _hd = _sp->getHeaderArrayForUpdate();
  setAnnotationSizes();
  fillRGB();

  //load in cgm assigned colors
  for(i = 0; i < 4 * MAX_CONTOURS; i+=4)
    {
    _cgm_colors[cgm_index] = 
                   _hc->defineColorIndex(_rgbs[i],_rgbs[i+1],_rgbs[i+2]);
    ++cgm_index;
    }


  //CGM does not handle metric so we will set all metric parameters to
  //the equivalent english system.
  //if(_sp->units() != PlotEnglish) _dots_per_inch /= .39;

  _coordinate_system  = HardCopyPlot::INCHES;

                        
  if(frame_number)
    {
    _status = drawContours(frame_number);
    }

}


//===========================================================================
//============================ Destructor ===================================
//===========================================================================
HardCopyContour::~HardCopyContour()
{
}


//===========================================================================
//===================== Fill in contour color rgbs ==========================
//===========================================================================
void HardCopyContour::fillRGB()
{
int i = 0;

  _rgbs[i]=  0.000; _rgbs[++i]=1.000; _rgbs[++i]=0.000; _rgbs[++i]=0.00;
  _rgbs[++i]=0.000; _rgbs[++i]=0.000; _rgbs[++i]=1.000; _rgbs[++i]=0.00;
  _rgbs[++i]=1.000; _rgbs[++i]=1.000; _rgbs[++i]=0.000; _rgbs[++i]=0.00;
  _rgbs[++i]=1.000; _rgbs[++i]=0.000; _rgbs[++i]=1.000; _rgbs[++i]=0.00;
  _rgbs[++i]=0.000; _rgbs[++i]=1.000; _rgbs[++i]=1.000; _rgbs[++i]=0.00;
  _rgbs[++i]=1.000; _rgbs[++i]=0.620; _rgbs[++i]=0.000; _rgbs[++i]=0.00;
  _rgbs[++i]=1.000; _rgbs[++i]=0.000; _rgbs[++i]=0.000; _rgbs[++i]=0.00;
  _rgbs[++i]=0.000; _rgbs[++i]=0.400; _rgbs[++i]=0.000; _rgbs[++i]=0.00;
  _rgbs[++i]=0.000; _rgbs[++i]=0.500; _rgbs[++i]=0.500; _rgbs[++i]=0.00;
  _rgbs[++i]=0.700; _rgbs[++i]=0.700; _rgbs[++i]=1.000; _rgbs[++i]=0.00;
  _rgbs[++i]=0.410; _rgbs[++i]=0.450; _rgbs[++i]=0.280; _rgbs[++i]=0.00;
  _rgbs[++i]=0.000; _rgbs[++i]=0.430; _rgbs[++i]=0.370; _rgbs[++i]=0.00;
  _rgbs[++i]=0.310; _rgbs[++i]=0.550; _rgbs[++i]=0.500; _rgbs[++i]=0.00;
  _rgbs[++i]=0.319; _rgbs[++i]=0.740; _rgbs[++i]=0.630; _rgbs[++i]=0.00;
  _rgbs[++i]=0.500; _rgbs[++i]=0.000; _rgbs[++i]=0.312; _rgbs[++i]=0.00;
  _rgbs[++i]=0.748; _rgbs[++i]=0.000; _rgbs[++i]=0.463; _rgbs[++i]=0.00;
  _rgbs[++i]=1.000; _rgbs[++i]=0.000; _rgbs[++i]=0.497; _rgbs[++i]=0.00;
  _rgbs[++i]=1.000; _rgbs[++i]=0.750; _rgbs[++i]=1.000; _rgbs[++i]=0.00;
  _rgbs[++i]=0.497; _rgbs[++i]=0.870; _rgbs[++i]=1.000; _rgbs[++i]=0.00;
  _rgbs[++i]=0.770; _rgbs[++i]=1.000; _rgbs[++i]=1.000; _rgbs[++i]=0.00;
}




//===========================================================================
//===================== Compute annotation sizes.                ============
//===================== Some assumptions...                      ============
//===================== Currently assume that the hardcopy       ============
//===================== class is setting a border width that     ============
//===================== will make the character sizes asthetic   ============
//===================== Also assume that the width of a character ===========
//===================== is 75 percent of the height which is the ============
//===================== case with the helvetica font we now use  ============
//===========================================================================
void HardCopyContour::setAnnotationSizes()
{

  _bold_char_width    = (_hc->leftBorderWidth() / MAX_LABEL_LENGTH) 
                      * 0.80;
  _normal_char_width  = (_hc->leftBorderWidth() / MAX_LABEL_LENGTH) 
                      * 0.60;
  _bold_char_height   = _bold_char_width * 1.333;
  _normal_char_height = _normal_char_width * 1.333;
  _bold_line_width    = 4.0;
  _normal_line_width  = 2.0;

  //CGM does not handle metric so we will set all metric parameters to
  //the equivalent english system.
  //if(_sp->units() != PlotEnglish)
  //  {
  //  _bold_char_width    /= .39;
  //  _normal_char_width  /= .39;
  //  _bold_char_height   /= .39;
  //  _normal_char_height /= .39;
  //  _bold_line_width    /= .39;
  //  _normal_line_width  /= .39;
  //  }


}



//===========================================================================
//=================== Draw the contours   ===================================
//===========================================================================
int HardCopyContour::drawContours(long frame_number)
{
 static float scr[110];
 float xs[10], ys[10];
 float spcnvl[20];
 long  indivl;
 long  start_index,end_index;
 float gpiy, zminm, zmaxm;
 long i, j, k, l, m, n, s;
 float val;
 long nval, is;
 float sum, smin, smax, conint, conval;
 long jpt, i1;
 float z1, z2, z3, z4, di, dj;
 float *floatvals;
 float dx, dy;
 float timefactor; 
 float X1, X2, Y1, Y2;
 float XL, YL;
 long firsthdr,lasthdr;
 float *xarray, LabelValue;
 char label[8];
 long intval, numt;
 long trace_index;
 long offset;
 unsigned char *scaled_bytes;
 long data_offset;
 long HdrOffset, AryOffset;
 float x_min, x_max;
 float xfactor;
 HCpoint     rec_pts[5];
 int x_hdr = 5;
 float scaler;
 long iflg = 0, iflg1 = 0;



  scaler = (float)_sp->getAmplitudeRecovery()->getScaleAmp(frame_number - 1);

  if(scaler <= 0.0) scaler = 1.0;

 //black fill test
  _hc->setFillType(HardCopyPlot::SolidFill);

  rec_pts[0].x=rec_pts[4].x = _hc->leftBorderWidth();
  rec_pts[0].y=rec_pts[4].y = _hc->topBorderHeight();

  rec_pts[1].x = _hc->leftBorderWidth() + _hc->drawingAreaWidth();
  rec_pts[1].y = _hc->topBorderHeight();

  rec_pts[2].x = _hc->leftBorderWidth() + _hc->drawingAreaWidth();
  rec_pts[2].y = _hc->topBorderHeight() + _hc->drawingAreaHeight();

  rec_pts[3].x = _hc->leftBorderWidth();
  rec_pts[3].y = _hc->topBorderHeight() + _hc->drawingAreaHeight();

  _hc->setFillColor(HardCopyPlot::blackColor());

  if (_sp->plotType() != PlotImage::PlotSEMB)
    {
    //_hc->drawFilledPolygon( rec_pts, 5, HardCopyPlot::INCHES); 
    x_hdr = _sp->matchHeader() - 1;
    }



  HdrOffset = (frame_number - 1) * _sp->plottedNplt() * _sp->numHeaders();
  AryOffset = (frame_number - 1) * _sp->plottedNplt() * 
                                                     _sp->displayedSamples();
  floatvals = _sp->getFloatArrayForUpdate();

      
  if(_sp->isZoomed())
     data_offset = _sp->currentFrame() * (_sp->memoryTraces() * 
                   _sp->samplesPerTrace());
  else
     data_offset = AryOffset;

  trace_index = _sp->currentFrame() * _sp->memoryTraces();
  offset = data_offset;


  start_index      = (long)(_sp->zoomXstart() - 1);
  end_index        = (long)(_sp->zoomXend()   - 1);
  firsthdr         = (long)((_sp->zoomXstart() - 1)*_sp->numHeaders()
                      + x_hdr);
  lasthdr          = (long)((_sp->zoomXend()   - 1)*_sp->numHeaders()
                      + x_hdr);
  x_min          = _hd[firsthdr+HdrOffset];
  x_max          = _hd[lasthdr+HdrOffset];
  gpiy             = _sp->srval();
  zminm            = 0.0;
  zmaxm            = _sp->plottedTmin();
  timefactor       = (_hc->drawingAreaHeight())/(_sp->plottedTmax()-zmaxm);
  xfactor = (_hc->drawingAreaWidth()) / (x_max - x_min);

  xarray = (float *) malloc((int)(_sp->memoryTraces() * sizeof(float)));
  if(xarray == NULL) return(PlotImage::ResourceFail);
  j = x_hdr; //xarray header index
  for(i=0;i<_sp->memoryTraces();i++)
     {
     xarray[i] = _hd[j+HdrOffset];
     j += _sp->numHeaders();
     }
  numt = _sp->memoryTraces();

  indivl= _sp->contours();
 
 
  for(m=0;m<indivl;m++) spcnvl[m]=0.0;
                 
  //if zoomed need to compute the next as if it were the
  //original image so contour levels will be the same
  if(_sp->isZoomed())
    data_offset = _sp->currentFrame() * 
                  (_sp->memoryTraces() * _sp->samplesPerTrace());
  else
    data_offset = AryOffset;


  
  l=0;
  for(m=0;m<102;m++) scr[m]=0.0;
  for(i=0;i<_sp->memoryTraces();i++)
    {
    for(j=0;j<_sp->samplesPerTrace();j++)
      {
      val  = floatvals[l+data_offset]/scaler*100.0;
      nval = (long)(val + 1.0);
      scr[nval] = scr[nval] + 1.0;
      l++;
      }
   }


  
  if(_sp->plotType() != PlotImage::PlotISO)
    {
    sum   = 0.0;
    iflg  = 0;
    iflg1 = 0;
    if (_sp->minP() < 0.0) 
        _sp->setMinP(0.0);                
    for(j=0;j<101;j++)
       {
       sum = sum + scr[j]/(_sp->samplesPerTrace()*_sp->memoryTraces());
       if(sum > _sp->minP() && iflg == 0)
         iflg = j + 1;        
       if(sum > _sp->maxP() && iflg1== 0)
         iflg1= j + 1;           
       }
    if(iflg1 == 0) iflg1 = 100;    
    if(_sp->maxP() >= 1.0)iflg1 = 100;
    smin = scaler*iflg*0.01;
    smax = scaler*iflg1*0.01;
    conint=0.0;            
    if(indivl > 1) conint =  (smax-smin)/(indivl-1);
    for(j=0;j<indivl;j++) spcnvl[j] = smin + (conint*j);
    }
  else
    {
    smin = _sp->minP();
    smax = _sp->maxP();
    conint=0.0;            
    if(indivl > 1) conint =  _sp->getContourIncrement();
    for(j=0;j<indivl;j++) spcnvl[j] = smin + (conint*j);     
    }

  dx = dy = 1.0;
  

  // select a contour value. scan from top to bottom left to right.
  // determine the min and max values on the grid square
  _hc->setLineType(HardCopyPlot::SolidLine);
  _hc->setLineWidth(_normal_line_width * 3);
  for(k=0;k<indivl;k++)
    {
    conval = spcnvl[k];
    intval = (long)((conval/scaler)*100.0);
    LabelValue = intval/100.0;
    sprintf(label,"%1.2f",LabelValue);
    XL = .85 * _hc->drawingAreaWidth() + _hc->leftBorderWidth();
    YL =  (k + 2) * _bold_char_height + _hc->topBorderHeight();
    _hc->setLineColor(_cgm_colors[k]);
    _hc->setTextColor(_cgm_colors[k]);
    _hc->setTextHeight(_bold_char_height);
    _hc->writeConstrainedText(XL, YL, 1.0, .25, label, _coordinate_system);
    for(i=start_index,j=0; i<end_index; i++, j++)
      {
      jpt = j * _sp->samplesPerTrace();     
      for(s=0;s<_sp->displayedSamples()-1;s++)
        {
        i1 = jpt + s;
        z1 = floatvals[i1+AryOffset];
        z2 = floatvals[i1+1+AryOffset];
        z3 = floatvals[i1+1+_sp->samplesPerTrace()+AryOffset];
        z4 = floatvals[i1+_sp->samplesPerTrace()+AryOffset];
        di = i;
        dj = s;
        is = 0;
        _sp->contourSemblanceData(&di,&dj,&conval,&z1,&z2,&z3,&z4,
                            &dx, &dy, xs, ys, xarray, &conint,
                            &zminm, &zmaxm, &gpiy, &is, &numt);
        if(is >= 2)      
          {
          for(m=1;m<is;m+=2) 
            {
            n  = m + 1;
            X1 = ((xs[m] - x_min) * xfactor + _hc->leftBorderWidth()); 
            X2 = ((xs[n] - x_min) * xfactor + _hc->leftBorderWidth());
            Y1 = ((ys[m] - _sp->plottedTmin()) * timefactor + 
                  _hc->topBorderHeight());
            Y2 = ((ys[n] - _sp->plottedTmin()) * timefactor +
                     _hc->topBorderHeight());
            if(Y1 < _hc->drawingAreaHeight()+_hc->topBorderHeight() &&
               Y2 < _hc->drawingAreaHeight()+_hc->topBorderHeight())
              _hc->drawLine(X1,Y1,X2,Y2,_coordinate_system);
            }
          }        
        }
      }         
    is = 0;
  }

  _hc->setLineColor(HardCopyPlot::blackColor());
  _hc->setTextColor(HardCopyPlot::blackColor());
  if(xarray != NULL){free(xarray);xarray=NULL;}
  return(PlotImage::PlotSuccess);
}



