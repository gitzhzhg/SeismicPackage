//===========================================================================
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
//===================== Hardcopy annotation class ===========================
//===================== M.L. Sherrill 03/96       ===========================
//===========================================================================
 
#include <limits.h>
#include <stdlib.h>
#include <math.h>
#include "hardcopy/hardcopy_annotate.hh"
#include "hardcopy/hardcopy_plot.hh"
#include "sp/seis_plot.hh"
#include "cprim.h"
#include "str.h"


#define MAX_LABEL_LENGTH 11.0

enum{OVERLAY, UNDERLAY};
//===========================================================================
//================= Constructor. If annotation is not desired ===============
//================= when constructed set frame_number to 0    ===============
//===========================================================================
HardCopyAnnotation::HardCopyAnnotation( HardCopyPlot *hc_over,
                                        SeisPlot     *sp_over,
                                        HardCopyPlot *hc_under,
                                        SeisPlot     *sp_under,
                                        float        over_x1,
                                        float        over_x2,
                                        float        over_y1,
                                        float        over_y2,
                                        float        under_x1,
                                        float        under_x2,
                                        float        under_y1,
                                        float        under_y2,
                                        long         frame_number)

                                        
{

  if(hc_under == NULL && over_x1 == 0.0 && over_x2 == 0.0 && 
     over_y1  == 0.0  && over_y2 == 0.0 )//no underlay image, use hardcopy coords
    {
    over_x1 = hc_over->leftBorderWidth();
    over_x2 = hc_over->leftBorderWidth() + hc_over->drawingAreaWidth();
    over_y1 = hc_over->topBorderHeight();
    over_y2 = hc_over->topBorderHeight() + hc_over->drawingAreaHeight();
    }

  _hc_active = hc_over;
  _hc_over   = hc_over;
  _hc_under  = hc_under;

  _sp_active = sp_over;
  _sp_over   = sp_over;
  _sp_under  = sp_under;

  _over_x1  = over_x1;
  _over_x2  = over_x2;
  _over_y1  = over_y1;
  _over_y2  = over_y2;
  _under_x1 = under_x1;
  _under_x2 = under_x2;
  _under_y1 = under_y1;
  _under_y2 = under_y2;

  _type = OVERLAY;

  if(hc_under) 
    _over_under = True;
  else
    _over_under = False;

  _metric = _sp_active->units();
  _coordinate_system  = HardCopyPlot::INCHES;
  

  _xoffset     =  _over_x1;
  _yoffset     =  _over_y1;
  _x2          =  _over_x2;
  _image_width = _over_x2 - _over_x1;
  _image_height= _over_y2 - _over_y1;
  _ylabel_xoffset = _hc_active->leftBorderWidth();
  setAnnotationSizes();


  if(frame_number) 
    {
    annotate(frame_number);
    if(hc_under)//annotate underlay 
      {
      _hc_active = hc_under;
      _sp_active = sp_under;
      _type = UNDERLAY;
      _xoffset =  _under_x1;
      _yoffset =  _under_y1;
      _x2      =  _under_x2;
      _image_width = _under_x2 - _under_x1;
      _image_height= _under_y2 - _under_y1;
      _hc_active->setLineType(HardCopyPlot::SolidLine);
      annotate(frame_number);
      }
    }

}


//===========================================================================
//===================== Destructor  =========================================
//===========================================================================
HardCopyAnnotation::~HardCopyAnnotation()
{

}




//===========================================================================
//===================== Main method for annotation   ========================
//===================== Frame number is movie number ========================
//===========================================================================
void HardCopyAnnotation::annotate(long frame_number)
{
char junk[256];

  _hc_active->setLineType(HardCopyPlot::SolidLine);
  _hc_active->setLineColor(HardCopyPlot::blackColor());

  if(!_sp_active->getSeisAnnotation() && _type != UNDERLAY)//grid types etc.
    {
    gridWithHeaders(frame_number);
    if(_sp_active->getExtraXHardcopyAnnotation(junk) || 
       _sp_active->getExtraYHardcopyAnnotation(junk)     )
          drawExtraLabels(frame_number);
    }
  else
    {
    drawPlotLabel(frame_number);
    
    //if( (_type == OVERLAY && _hc_under == 0) || (_type == UNDERLAY) )
    drawYannotation(frame_number);

    if(_sp_active->getExtraXHardcopyAnnotation(junk) || 
       _sp_active->getExtraYHardcopyAnnotation(junk)     )
          drawExtraLabels(frame_number);

    if(!_sp_active->getSeisAnnotation() && _type == UNDERLAY)//dont annotate x
      {
      drawBorder(frame_number);
      }
    else//annotate x axis
      {
      drawXannotation(frame_number);
      drawBorder(frame_number);
      }
    }
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
void HardCopyAnnotation::setAnnotationSizes()
{

/*
  _bold_char_width    = (_hc_active->leftBorderWidth() / MAX_LABEL_LENGTH) 
                      * 0.80;
  _normal_char_width  = (_hc_active->leftBorderWidth() / MAX_LABEL_LENGTH) 
                      * 0.60;
 */
  _bold_char_width    = (2 / MAX_LABEL_LENGTH) 
                      * 0.80;
  _normal_char_width  = (2 / MAX_LABEL_LENGTH) 
                      * 0.60;
  _bold_char_height   = _bold_char_width * 1.333;
  _normal_char_height = _normal_char_width * 1.333;
  _bold_line_width    = 4.0;
  _normal_line_width  = 2.0;

  /* CGM does not handle metric so we will set all metric parameters to
     the equivalent english system.
  if(_metric)
    {
    _bold_char_width    /= .39;
    _normal_char_width  /= .39;
    _bold_char_height   /= .39;
    _normal_char_height /= .39;
    _bold_line_width    /= .39;
    _normal_line_width  /= .39;
    }
  */

}

//===========================================================================
//===================== Character size.                ======================
//===========================================================================
float HardCopyAnnotation::charHeight(unsigned char bold)
{

  if(bold)
    return _bold_char_height;
  else
    return _normal_char_height;

}




//===========================================================================
//===================== Character size                 ======================
//===========================================================================
float HardCopyAnnotation::charWidth(unsigned char bold)
{

  if(bold)
    return _bold_char_width;
  else
    return _normal_char_width;

}

//===========================================================================
//===================== Draw plot label at bottom    ========================
//===================== left of plot.                ========================
//===========================================================================
void HardCopyAnnotation::drawPlotLabel(long /*frame_number*/)
{
float x1, y1;

  x1 = _sp_active->plotLabelX();
  y1 = _sp_active->plotLabelY();
  if(x1 == 0 && y1 == 0) 
    {
    x1 = _hc_active->leftBorderWidth();
    if(!_over_under)
      y1 = _image_height + _hc_active->topBorderHeight() 
         +  charHeight(True);
    else
      y1 = max( (_over_y2 - _over_y1), (_under_y2 - _under_y1) )
         + _hc_active->topBorderHeight() +  charHeight(True);
    if(_sp_active->canOverlay()) y1 += charHeight(True);
    }
//will need to do a pixel to inch conversion later if x and y != default of 0
  _hc_active->setLabelPlacement(HardCopyPlot::Normal);
  _hc_active->setTextHeight(charHeight(True));
  if( strlen(_sp_active->plotLabel()) )
    _hc_active->writeConstrainedText(x1, y1, 4.5, .25,
                                     _sp_active->plotLabel(),
                                     _coordinate_system);
  y1 += charHeight(True);
  if( strlen(_sp_active->filename()) )
    _hc_active->writeConstrainedText(x1, y1, 4.5, .25,
                                     _sp_active->filename(),
                                     _coordinate_system);
}

//===========================================================================
//===================== Main method for drawing extra =======================
//===================== x and y labels like geopress  =======================
//===================== Frame number is movie number ========================
//===========================================================================
void HardCopyAnnotation::drawExtraLabels(long /*frame_number*/)
{
float x1, y1;
char label[256];
char letter[2];
int labellength;

   letter[1] ='\0';

  //Do an extra x label
  if(_sp_active->getExtraXHardcopyAnnotation(label))
    {
    labellength = strlen(label);
    x1 = (_image_width / 2) -  ((labellength * charWidth(True)) / 2.0)
       + _hc_active->leftBorderWidth();
    y1 = 2 * charHeight(True);
    _hc_active->setLabelPlacement(HardCopyPlot::Normal);
    _hc_active->setTextHeight(charHeight(True));
    _hc_active->writeConstrainedText(x1, y1, 4.5, .25,
                                   label, _coordinate_system);
    }

  //Do an extra y label
  if(_sp_active->getExtraYHardcopyAnnotation(label))
    {
    labellength = strlen(label);
    x1 = 2 * charWidth(True);
    y1 = (_image_height / 2) - ((labellength * 2 * charHeight(True)) / 2.0)
       + _hc_active->topBorderHeight();
    _hc_active->setLabelPlacement(HardCopyPlot::Normal);
    _hc_active->setTextHeight(charHeight(True));
    for(int i = 0; i < labellength; i++)
      {
      strncpy(letter,&label[i],1);
      _hc_active->writeConstrainedText(x1, y1, 4.5, .25,
                                       letter, _coordinate_system);
      y1 += (2 * charHeight(True));
      }
   }
  
}



//===========================================================================
//===================== Draw the y axis annotation   ========================
//===================== method. Frame number not     ========================
//===================== needed at this time since    ========================
//===================== all plots have same y axis   ========================
//===========================================================================
void HardCopyAnnotation::drawYannotation(long frame_number)
{
long invert_y;
float labellen, x_offset;
long NumToLabel;
long PrimaryLineInc, PrimaryTimingLine;
long SecondaryTimingLine;
long i, m, ist, iftl;
float x1, x2, y1, y2;
char label[256];
float addfraction;
float y_inc, label_inc, LabelValue;
float TotalTime;
float IncrOfLabels;
float first_timing_line, first_y;
float depth_factor;
long hdindex;
float oldy;
float tmin,tmax;
long junk_int;
float junk_float;
float *ypoints;
long num_y_points;
int can_draw;
float xfact = 1000.0;
const float *headary = _sp_active->firstMemoryHeaderData();
int inverted = False;
float height, width;
float over_under_min_x;
float over_under_max_x;
float over_under_min_y;
float over_under_max_y;



  _hc_active->setLabelPlacement(HardCopyPlot::Normal);

  if(!_over_under)
    {
    height = _image_height;
    width  = _image_width;
    }
  else
    {
    over_under_min_x = min(_over_x1, _under_x1);
    over_under_max_x = max(_over_x2, _under_x2);
    over_under_min_y = min(_over_y1, _under_y1);
    over_under_max_y = max(_over_y2, _under_y2);
    height = over_under_max_y - over_under_min_y;
    width  = over_under_max_x - over_under_min_x;
    } 

 if(_sp_active->manualTransformY())
    {
    annotateManualGridY(frame_number);
    return;
    }
  else
    {
    if(_sp_active->plottedGridY1() > _sp_active->plottedGridY2()) 
      inverted = True;   
    if(!_over_under)
      {
      tmin = _sp_active->plottedGridY1();
      tmax = _sp_active->plottedGridY2();
      y_inc = (tmax - tmin)  / height;
      }
    else
      {
      if(!inverted)
        {
        tmin = min(_sp_over->plottedGridY1(),_sp_under->plottedGridY1()); 
        tmax = max(_sp_over->plottedGridY2(),_sp_under->plottedGridY2());
        }
      else
        {
        tmin = max(_sp_over->plottedGridY1(),_sp_under->plottedGridY1());
        tmax = min(_sp_over->plottedGridY2(),_sp_under->plottedGridY2());
        }
      y_inc = (tmax - tmin) / height;
      }
    }


  if(_sp_active->plotType() == PlotImage::PlotGRID && inverted == True)
     _sp_active->setInvert(True);
  if(_sp_active->plotType() == PlotImage::PlotHEADER && inverted == True)
     _sp_active->setInvert(True);
  if(_sp_active->plotType() == PlotImage::PlotHEADER && inverted == False)
    _sp_active->setInvert(False);
  invert_y = _sp_active->invert();  

  if(_sp_active->plotType() == PlotImage::PlotARRAY && inverted)
     invert_y = True;


  //if we have an underlay without annotation parameters, set them to 
  //the overlay's annotation parameters
  if(_type == UNDERLAY && _sp_active->getSeisAnnotation() == False)
    {
    if(!_sp_over->yAxisHeader()) /*use data to label by*/
      {
      if(!_sp_over->depth())
         depth_factor = 1.0;
      else/*this is not a safe default, but cannot get from data*/
         depth_factor = xfact; 
      if(!_sp_over->primTimingLine())
          _sp_over->setPrimTimingLine(tmax);
      if(!_sp_over->secTimingLine()) 
          _sp_over->setSecTimingLine(_sp_over->primTimingLine());
      _sp_under->setPrimTimingLine(_sp_over->primTimingLine());
      _sp_under->setSecTimingLine(_sp_over->secTimingLine());
      }
    }
  else
    {
    if(!_sp_active->yAxisHeader()) /*use data to label by*/
      {
      if(!_sp_active->depth())
         depth_factor = 1.0;
      else/*this is not a safe default, but cannot get from data*/
         depth_factor = xfact; 
      if(!_sp_active->primTimingLine())
          _sp_active->setPrimTimingLine(tmax);
      if(!_sp_active->secTimingLine()) 
          _sp_active->setSecTimingLine( _sp_active->primTimingLine());
      }
    }

  if(!_sp_active->yAxisHeader()) /*use data to label by*/
    {  
    /*make sure multiplier will not exceed size of a long*/
    while( max(tmin,tmax) * xfact > LONG_MAX ) xfact /= 10.0;

    PrimaryLineInc = PrimaryTimingLine = 
                          (long)(_sp_active->primTimingLine() * xfact + .5);
    SecondaryTimingLine = (long)(_sp_active->secTimingLine()  * xfact + .5);
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
    label_inc    = (((float)PrimaryTimingLine) * (1.0 / xfact)) / y_inc;
    IncrOfLabels = _sp_active->primTimingLine();
    if(invert_y)
       {
       TotalTime         = -TotalTime;
       label_inc         = -label_inc;
       IncrOfLabels      = -IncrOfLabels;
       }
    NumToLabel = (long)((TotalTime / _sp_active->primTimingLine())
               - ((first_timing_line - tmin) / _sp_active->primTimingLine()));
    if(NumToLabel < 0) NumToLabel = (-NumToLabel);
    LabelValue   = first_timing_line; 
    sprintf(label,"%8.3f",LabelValue);
    labellen = (float) strlen(label);
    x1 = _ylabel_xoffset;
    x2 = width + _hc_active->leftBorderWidth();
    y1 = y2 = ((first_timing_line - tmin) / y_inc ) 
                 + _hc_active->topBorderHeight(); 
    first_y = y1 - _hc_active->topBorderHeight();
    junk_int = (int)(tmin / _sp_active->primTimingLine());
    junk_float =  tmin / _sp_active->primTimingLine();
    if( (junk_float - junk_int) )
      PrimaryTimingLine = (long)(_sp_active->primTimingLine() * xfact + .5);
    else
      PrimaryTimingLine = (long)(tmin * xfact + .5);
    if(invert_y)
       {
       PrimaryTimingLine = (long)(tmin * xfact + .5);
       PrimaryLineInc    = -PrimaryLineInc;
       }     
    num_y_points = NumToLabel + 1;
    ypoints = NULL;
    ypoints = (float *) calloc( 1, (((unsigned int)num_y_points) * 
                                     sizeof(float)) );
    if(ypoints == NULL)
       {
       printf("not enough memory for ypoints in HardCopyAnnotate\n");
       num_y_points = 0;
       }     
    oldy = 0.0;
    for(i=0;i<=NumToLabel;i++)
       {
          if( (y1 >= _hc_active->topBorderHeight() - charHeight(True)) && 
              (y1 <= _hc_active->topBorderHeight() + height) &&
              (y1 - oldy > charHeight(True) ))
             {
             oldy = y1;
             if(_sp_active->plotType() != PlotImage::PlotHEADER)
                _hc_active->setLineWidth(_bold_line_width);
             else
                _hc_active->setLineWidth(_normal_line_width);
             _hc_active->drawLine(x1-charWidth(True),y1,x2 + charWidth(True),
                                  y2, _coordinate_system);
             if(!invert_y)
                PrimaryTimingLine += PrimaryLineInc; 
             else
                PrimaryTimingLine = 
                        (long)(LabelValue * xfact + PrimaryLineInc + .5);
             sprintf(label,"%8.3f",(LabelValue*depth_factor));
             labellen = (float)strlen(label);
             _hc_active->setTextHeight(charHeight(True));
             x_offset = x1 -  (labellen * charWidth(True));
             _hc_active->writeConstrainedText(x_offset,
                                              y1 + .5 * charHeight(True), 
                                              1.0, .25, label,
                                              _coordinate_system);
             _hc_active->writeConstrainedText(x2,
                                              y1 + .5 * charHeight(True), 
                                              1.0, .25, label,
                                              _coordinate_system); 
          
             if(ypoints)ypoints[i] = LabelValue;
             }
       LabelValue += IncrOfLabels;
       y1 = y2 = (i + 1) * label_inc + _hc_active->topBorderHeight() + first_y;
       }

            /*********** DO THE SECONDARY NON-BOLD ANNOTATION *********/
    if(tmin < 0.0 || invert_y)
       iftl = ist / SecondaryTimingLine * SecondaryTimingLine;
    else
       iftl = ((ist + SecondaryTimingLine - 1) / SecondaryTimingLine) 
            * SecondaryTimingLine;
    first_timing_line = (float) iftl / xfact;
    TotalTime    = tmax - tmin;
    label_inc    = (((float)SecondaryTimingLine) * (1.0 / xfact)) / y_inc;
    IncrOfLabels = _sp_active->secTimingLine();
    if(invert_y)
       {
       TotalTime         = -TotalTime;
       label_inc         = -label_inc;
       IncrOfLabels      = -IncrOfLabels;
       }
    NumToLabel = (long)((TotalTime / _sp_active->secTimingLine())
               - ((first_timing_line - tmin) / _sp_active->secTimingLine()));
    LabelValue   = first_timing_line; 
    sprintf(label,"%8.3f",LabelValue);
    x1 = _ylabel_xoffset;
    x2 = width + _hc_active->leftBorderWidth();
    y1 = y2 = ((first_timing_line - tmin) / y_inc ) 
                                        + _hc_active->topBorderHeight(); 
    first_y = y1 - _hc_active->topBorderHeight();
    junk_int = (int)(tmin / _sp_active->secTimingLine());
    junk_float =  tmin / _sp_active->secTimingLine();
    if( (junk_float - junk_int) )
      SecondaryTimingLine = (long)(_sp_active->secTimingLine() * xfact);
    else
      SecondaryTimingLine = (long)(tmin * xfact);
    if(invert_y)
       {
       SecondaryTimingLine = (long)(tmin * xfact);
       }     
    oldy = 0;
    for(i=0;i<=NumToLabel;i++)
       {
         if((y1 >= _hc_active->topBorderHeight() - charHeight(False)) &&
           (y1 <= _hc_active->topBorderHeight() + height)             && 
           (y1 - oldy > charHeight(False))                            && 
            (_sp_active->plotType() != PlotImage::PlotHEADER))
           //do only bold on header plots since the char height of false
           //will cause the next ypoints loop to fail
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
             _hc_active->setLineWidth(_normal_line_width);
             _hc_active->drawLine(x1-charWidth(False),y1,x2 + charWidth(False),
                                  y2,  _coordinate_system);
             sprintf(label,"%8.3f",(LabelValue*depth_factor));
             _hc_active->setTextHeight(charHeight(False));
             x_offset = x1 - (labellen * charWidth(False));
             _hc_active->writeConstrainedText(x_offset,
                                              y1 + .5 * charHeight(False), 
                                              .70, .18, label,
                                              _coordinate_system); 
             _hc_active->setTextHeight(charHeight(False));
             _hc_active->writeConstrainedText(x2,
                                              y1 + .5 * charHeight(False), 
                                              .70, .18, label,
                                              _coordinate_system); 
	     }
          }
         //LabelValue = (int)(LabelValue*xfact) + (int)(IncrOfLabels*xfact);
         //LabelValue = LabelValue / xfact;
       LabelValue += IncrOfLabels;
       y1 = y2 = ((i + 1)*label_inc) + _hc_active->topBorderHeight() + first_y;
       }
    if(ypoints) free(ypoints);
    } /***** END NON-BOLD SECONDARY ANNOTATION ***********/
 else/***USE A Y HEADER TO LABEL BY, BOLD ANNOTATION NOT YET SUPPORTED HERE****/
    {
    x1 = _xoffset;
    x2 = width + _xoffset;
    oldy = 0.0;
    hdindex = _sp_active->yAxisHeader();
    for(i=0; i<_sp_active->numYlabels(); i++)
       {
       y1 = y2 = (headary[hdindex] - tmin) + _hc_active->topBorderHeight();
       _hc_active->setLineWidth(_normal_line_width);
       _hc_active->drawLine(x1,y1,x2,y2,_coordinate_system);
       if( (fabs(y1 - oldy)) > charHeight(False))
         {
         sprintf(label,"%8.3f",headary[hdindex]);
         labellen = (float)strlen(label);
         _hc_active->setTextHeight(charHeight(False));
         x_offset = x1 - (labellen * charWidth(False));
         _hc_active->writeConstrainedText(x_offset,
                                          y1 + .5 * charHeight(False), 
                                         .70, .18, label,
                                          _coordinate_system); 
         _hc_active->writeConstrainedText(x2,
                                          y1 + .5 * charHeight(False), 
                                          .70, .18, label,
                                          _coordinate_system); 
         oldy = y1;
         }
       hdindex += _sp_active->numHeaders(); 
       }
    }


}




//===========================================================================
//===================== Draw the x axis annotation   ========================
//===================== method.                      ========================
//===========================================================================
void HardCopyAnnotation::drawXannotation(long frame_number)
{
long i,j,l;
long frames;
float labellen;
long HeaderOne = 1, HeaderTwo = 2;
float half_len;
long TracesToLabel;
float firstlabel;
float TraceLabelIncr;
float traceloc;
Boolean doTwo = False;
float x1, x2, y1, y2;
char label[256];
char label2[256];
long skip_traces = 0;
long hoffset;
long header_offset;
float diff;
const float *headary = _sp_active->getHeaderArrayForUpdate();
float trace_width;
long ntrc_div;
float label_inc = (float)_sp_active->lblInc();
Boolean raise_xlabel = False;
float x_perpix;

  frames = max(1, _sp_active->frames());

  header_offset = (frame_number - 1) * _sp_active->memoryTraces() *
                  _sp_active->numHeaders();

  if(_over_under == True)//need to modify this to handle PlotArray types
    {
    ntrc_div = _sp_over->plottedNplt();
    if(ntrc_div < 1) ntrc_div = 1;
    trace_width = (_over_x2 - _over_x1) / ntrc_div;
    }
  else
    {
    ntrc_div = _sp_active->plottedNplt();
    if(ntrc_div < 1) ntrc_div = 1;
    trace_width = _image_width / ntrc_div;
    }

  if(!_sp_active->rToL())
    traceloc = _xoffset + (trace_width / 2.0);
  else
    traceloc = _image_width + _xoffset - (trace_width / 2.0); 

  _hc_active->setLabelPlacement(HardCopyPlot::DeadCenter);

  switch(_sp_active->plotType()) 
    {
    /*----- Trace type labels -----*/
    case PlotImage::PlotWONLY:
    case PlotImage::PlotWFILL:
    case PlotImage::PlotGS:
    case PlotImage::PlotHEADER:
      if(label_inc < 1.0) break;
      if(_sp_active->firstLbl() < 1) _sp_active->setFirstLbl(1);
      TraceLabelIncr = label_inc * trace_width;
      while(TraceLabelIncr < (MAX_LABEL_LENGTH * .5) * charWidth(True)) 
        {
        _sp_active->setLblInc( 2 * _sp_active->lblInc());
        label_inc = (float)_sp_active->lblInc();
        TraceLabelIncr = label_inc * trace_width;
        }
      TracesToLabel  = (long)((
              (float)_sp_active->plottedNplt() - (float)_sp_active->firstLbl())
               / label_inc + 1.0);
      if(!_sp_active->rToL())
        x1 = traceloc + ((_sp_active->firstLbl() - 1) * trace_width);
      else
        x1 = traceloc - ((_sp_active->firstLbl() - 1) * trace_width);
      x2 = x1;
      y1 = _hc_active->topBorderHeight() - 1.8 * charHeight(True);        
      y2 = _hc_active->topBorderHeight();


      for(i=0;i<TracesToLabel;i++)//draw trace label vertical lines 
        {
        if( (x1 >= _xoffset) &&
            (x1 <= _image_width  + _xoffset)     )
	  {
          _hc_active->setLineWidth(_normal_line_width);
          _hc_active->drawLine(x1,y1,x2,y2,_coordinate_system);
	  }
        if(!_sp_active->rToL())
          x1 = x2 = x1 + TraceLabelIncr;
        else
          x1 = x2 = x1 - TraceLabelIncr;
        if(x1 > _image_width + _xoffset )
          i = TracesToLabel + 1; /*stop loop*/
        }

  
      //Draw the trace labels
      if(_sp_active->header2())doTwo = True;
      HeaderOne = (_sp_active->firstTrace() - 1 
                + _sp_active->firstLbl() - 1) 
                * _sp_active->numHeaders() + _sp_active->header1() - 1;
      if(doTwo)HeaderTwo =(_sp_active->firstTrace() - 1
                         + _sp_active->firstLbl() -1)
                         * _sp_active->numHeaders() 
                         + _sp_active->header2() - 1;

      if(!_sp_active->rToL())
        firstlabel = traceloc + ((_sp_active->firstLbl() - 1) * trace_width);
      else
        firstlabel = traceloc - ((_sp_active->firstLbl() - 1) * trace_width);

      /*
      if(_sp_active->plotType() == PlotImage::PlotCOLOR)
        {
        if(!_sp_active->rToL())
          firstlabel += (trace_width / 2.0);
       else
          firstlabel -= (trace_width / 2.0);
        }
        */
      sprintf(label,"%8.3f",headary[HeaderOne + header_offset]);
      x1 = firstlabel; 

      if(doTwo)
        { 
        sprintf(label2,"%8.3f",headary[HeaderTwo + header_offset]);
        j = 0;      
        while(label2[j] != '.')j++;
        x2 = firstlabel;
        }


      if(_over_under == True && _type == OVERLAY)
        if(_sp_under->getSeisAnnotation())
          raise_xlabel = True;


      if( !raise_xlabel )
        {
        y1 = _hc_active->topBorderHeight() - 4.0 * charHeight(True);
        if(doTwo) y2 = _hc_active->topBorderHeight() - 2.5 * charHeight(True);
        }
      else/*put the overlay annotation above the underlay annotation*/
        {
        y1 = _hc_active->topBorderHeight() - 7.0 * charHeight(True);
        if(doTwo) y2 = _hc_active->topBorderHeight() - 5.5 * charHeight(True);
        }

      /*Now draw trace labels*/
      for(l=0;l<TracesToLabel;l++)
        {
        if( (x1 >= _xoffset) &&
            (x1 <= _image_width + _xoffset))
	  {
          _hc_active->setTextHeight(charHeight(True));
          _hc_active->writeConstrainedText(x1, y1, 1.0, .25, label,
                                              _coordinate_system);
          }
        if(!_sp_active->rToL())
          x1 += TraceLabelIncr;
        else
          x1 -= TraceLabelIncr;

        if(doTwo)
          {
          if( (x2 >= _xoffset) &&
           (x2 <= _image_width + _xoffset)); 
	    {
            _hc_active->setTextHeight(charHeight(True));
            _hc_active->writeConstrainedText(x2, y2, 1.0, .25, label2,
                                              _coordinate_system);
	    }

          if(!_sp_active->rToL())
            x2 += TraceLabelIncr;
          else
            x2 -= TraceLabelIncr;
          }

        HeaderOne += (_sp_active->lblInc() * _sp_active->numHeaders());
        //following logic does not work on header plots since frames is 1
        //if(HeaderOne + header_offset >= _sp_active->memoryTraces() * frames 
        //   * _sp_active->getnumHeaderWords()) break;
        if(doTwo)
          {
          HeaderTwo += (_sp_active->lblInc()*_sp_active->numHeaders());
          //following logic does not work on header plots since frames is 1
          //if(HeaderTwo + header_offset >= _sp_active->memoryTraces() * frames
          //   * _sp_active->getnumHeaderWords())break;
          }
        sprintf(label,"%8.3f",headary[HeaderOne + header_offset]);

        if(doTwo)
          sprintf(label2,"%8.3f",headary[HeaderTwo + header_offset]);

       }/*end draw trace labels for loop*/ 
    break;
    /*---- End trace type x labels ----*/



  /*---- Semblance grid or contour type x labels ------*/
  case PlotImage::PlotCONTOUR:
  case PlotImage::PlotSEMB:
  case PlotImage::PlotGRID:
      gridWithoutHeaders(frame_number);
      break;
 /*---- end semblance grid or contour type x labels ----*/


 
 /*---- draw iso velocity and header type x labels ----*/
  case PlotImage::PlotARRAY:
      if(!_sp_active->labelByHeaders())
	{
        gridWithoutHeaders(frame_number);
        break;
	}
      for(i=1;i<=_sp_active->currentFrame();i++) 
          skip_traces += _sp_active->getTracesPerPanel(i-1);
      hoffset = _sp_active->numHeaders() * skip_traces; 
      labellen = MAX_LABEL_LENGTH;
      half_len = (labellen / 2.0) * charWidth(True);
      y1 = _hc_active->topBorderHeight() - 4.0 * charHeight(True);
      y2 = _hc_active->topBorderHeight() + _image_height;
      x2 = 0; /*for first time init*/
      x_perpix = (_sp_active->plottedGridX2() - _sp_active->plottedGridX1()) /
                 _image_width;
      for(i =  _sp_active->firstLbl()-1; 
          i < _sp_active->getTracesPerPanel(_sp_active->currentFrame());
          i += _sp_active->lblInc())
         {
         x1 = (headary[i*_sp_active->numHeaders()
                       +_sp_active->header1()-1+hoffset]
               - _sp_active->plottedGridX1()) / x_perpix 
               + _xoffset;
         if(!_sp_active->rToL())
            diff = x1 - x2;
         else
            diff = x2 - x1;
         if(i == _sp_active->firstLbl()-1)/*make sure 1st time will work*/
            diff = charWidth(True) * labellen + charWidth(True) + 1.0;
         if(diff > (charWidth(True) * labellen + charWidth(True)) )
            {
            if(x1 >= _xoffset && 
               x1 <= _image_width + _xoffset  )
               {
               _hc_active->setLineWidth(_normal_line_width);
               _hc_active->drawLine(x1,y1 + charHeight(True),x1,y2,
                                    _coordinate_system);
               sprintf(label,"%8.3f", headary[i*_sp_active->numHeaders()
                       + _sp_active->header1()-1+hoffset]);
               _hc_active->setTextHeight(charHeight(True));
               _hc_active->setLabelPlacement(HardCopyPlot::DeadCenter);
               _hc_active->writeConstrainedText(x1,y1, 1.0, .25, label,
                                               _coordinate_system);
               x2 = x1;
               }
            }
         } 
    break;
    /*---- end iso velocity type x labels ----*/
      
    }/*end switch*/ 


}




//===========================================================================
//===================== Draw a bounding rectangle    ========================
//===================== method.                      ========================
//===========================================================================
void HardCopyAnnotation::drawBorder(long /*frame_number*/)
{
float x1, x2, y1, y2;

 if(_sp_active->plotType() == PlotImage::PlotGRID ||
    _sp_active->plotType() == PlotImage::PlotHEADER)
 {
  x1 = _xoffset;
  y1 = _hc_active->topBorderHeight();
  x2 = x1 + _image_width;
  y2 = y1 + _image_height;

  _hc_active->setLineWidth(_bold_line_width);
  _hc_active->drawLine(x1,y1,x2,y1,_coordinate_system);
  _hc_active->drawLine(x1,y2,x2,y2,_coordinate_system);
  _hc_active->drawLine(x1,y1,x1,y2,_coordinate_system);
  _hc_active->drawLine(x2,y1,x2,y2,_coordinate_system);
 }

}



//===========================================================================
//===================== Method to annotate grid type ========================
//===================== plots that corner headers set  ======================
//===========================================================================
void HardCopyAnnotation::gridWithHeaders(long /*frame_number*/)
{
float labellen;
char label[20];
float x1,x2,y1,y2;
int i;
long data_samples;

    if(_sp_active->canOverlay()) return;


//If no annotation return
    if(_sp_active->primTimingLine() == 0.0 && 
       _sp_active->secTimingLine() == 0.0) return;

    data_samples = (long)(( _sp_active->plottedTmax()
                          - _sp_active->plottedTmin())/_sp_active->srval());
    if(data_samples < 0) data_samples = (-data_samples);

//Implement this later
//draw white grid rectangle on black background
//XSetForeground(dpy, image->gc2, image->black_pixel);
//XFillRectangle(dpy, image->pixmary[_sp->currentFrame()], image->gc2, 0,0,
//                   (_hc->plotWidth()
//                   +_hc->leftBorderWidth()+_sp->rightBorder()),
//                   (_hc->drawingAreaHeight()
//                   +_hc->topBorderHeight()+_sp->bottomBorder()) );
//
//XSetForeground(dpy, image->gc2, image->white_pixel);
//XDrawRectangle(dpy, image->pixmary[_sp->currentFrame()],image->gc2,
//                   _hc->leftBorderWidth()-7,_hc->topBorderHeight()-7,
//                   _hc->plotWidth()+15,_hc->drawingAreaHeight()+14);

//************** x annotation *************
//upper left x tic mark
    x1 = x2 = _xoffset;
    y1 = _hc_active->topBorderHeight() - charHeight(True);
    y2 = _hc_active->topBorderHeight();
    _hc_active->setLineWidth(_bold_line_width);
    _hc_active->drawLine(x1,y1,x2,y2,_coordinate_system);

//upper right x tic mark
    x1 = x2 = x1 + _image_width;
    _hc_active->drawLine(x1,y1,x2,y2,_coordinate_system);

//upper left x label
    sprintf(label,"%8f",_sp_active->plottedGridX1());
    i = 0;
    while(label[i] != '.')i++;
    labellen = (float)(i + 3);
    x1 = _xoffset - ((labellen - 1.0) * charWidth(True) 
       / 2.0);
    y1 = _hc_active->topBorderHeight() -  (2.5*charHeight(True));
    _hc_active->setTextHeight(charHeight(True));
    _hc_active->writeConstrainedText(x1, y1, 1.0, .25, label,
                                              _coordinate_system);

//upper right x label
    sprintf(label,"%8f",_sp_active->plottedGridX2());
    i = 0;
    while(label[i] != '.')i++;
    labellen = (float)(i + 3);
    x1 = _xoffset + _image_width
       - ((labellen - 1.0) * charWidth(True) / 2.0);
    _hc_active->writeConstrainedText(x1,y1, 1.0, .25, label,
                                     _coordinate_system);

//************* y annotation **********
//upper left tic mark
    x1 = _xoffset - (2*charWidth(True));
    x2 = x1 + (2*charWidth(True));
    y1 = y2 = _hc_active->topBorderHeight();
    _hc_active->drawLine(x1,y1,x2,y2,_coordinate_system);

//lower left tic mark
    y1 = y2 = y1 + _image_height;
    if(data_samples > 1)
    _hc_active->drawLine(x1,y1,x2,y2,_coordinate_system);

//lower right tic mark
    x1 = _xoffset + _image_width;
    x2 = x1 + (2*charWidth(True));
    if(data_samples > 1)
       _hc_active->drawLine(x1,y1,x2,y2,_coordinate_system);

//upper right tic mark
    y1 = y2 = y1 - _image_height;
    _hc_active->drawLine(x1,y1,x2,y2,_coordinate_system);

//upper left label
    sprintf(label,"%8f",_sp_active->plottedGridY1());
    i = 0;
    while(label[i] != '.')i++;
    labellen = (float)(i + 3);
    x1 = _xoffset - ((labellen+2.5) * charWidth(True));
    y1 = _hc_active->topBorderHeight() + (0.25 * charHeight(True));
    _hc_active->writeConstrainedText(x1,y1, 1.0, .25, label,
                                     _coordinate_system);

//upper right label
    sprintf(label,"%8f",_sp_active->plottedGridY1());
    i = 0;
    while(label[i] != '.')i++;
    labellen = (float)(i + 3);
    x1 = _xoffset + _image_width 
                                       + (2.5*charWidth(True));
    _hc_active->writeConstrainedText(x1,y1, 1.0, .25, label,
                                     _coordinate_system);

//lower left label
    if(data_samples > 1)
      {
      sprintf(label,"%8f",_sp_active->plottedGridY2());
      i = 0;
      while(label[i] != '.')i++;
      labellen = (float)(i + 3);
      x1 = _xoffset - ((labellen+2.5) * charWidth(True));
      y1 = y1 + _image_height;
      _hc_active->writeConstrainedText(x1,y1, 1.0, .25, label,
                                       _coordinate_system);
      }

//lower right label
    if(data_samples > 1)
      {
      sprintf(label,"%8f",_sp_active->plottedGridY2());
      i = 0;
      while(label[i] != '.')i++;
      labellen = (float)(i + 3);
      x1= _xoffset + _image_width 
                                 + (2.5*charWidth(True));
      _hc_active->writeConstrainedText(x1,y1, 1.0, .25, label,
                                       _coordinate_system);
      }


}



//===========================================================================
//===================== Method to annotate grid type ========================
//===================== plots that have no headers   ========================
//===========================================================================
void HardCopyAnnotation::gridWithoutHeaders(long /*frame_number*/)
{
float labellen2 = 0.0;
float labellen;
char label[20];
float x1,x2,y1,y2,oldx;
long i;
float vel_sign;
long temp_log;
float vel_log;
float fraction;
float vel_interval;
float s1;
float an;
float vel[3];
int n[5], nt[5];
long max_log;
float VelLabelIncr;
float vmin, vmax;
float vel_range;
float tempd;
long NumToLabel;
float half_len;
float vel_loc;
char label2[20];
float ivel;
const float *headary = _sp_active->firstMemoryHeaderData();
float x_inc;
float tempx1, tempivel;
int max_char = 20;
int num_decimals = 3;

  if(_sp_active->minImageVel() == _sp_active->maxImageVel())  return;


  vel[1] = _sp_active->minImageVel();
  vel[2] = _sp_active->maxImageVel();
  vel_sign = (_sp_active->maxImageVel() - _sp_active->minImageVel() > 0.0) 
           ? 1.0 : -1.0;
  if(_sp_active->usingSymetricalAnnotation())
    vel_range = max(_sp_active->plottedTmin(),_sp_active->plottedTmax()) 
              - min(_sp_active->plottedTmin(),_sp_active->plottedTmax());
  else
    vel_range = max(_sp_active->minImageVel(),_sp_active->maxImageVel()) 
              - min(_sp_active->minImageVel(),_sp_active->maxImageVel());
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

  if(_sp_active->minImageVel() > _sp_active->maxImageVel()) 
     vel_interval = (-vel_interval);

  for(i=1;i<3;i++)
     {
     s1 = 2.0 * (1.5-i)*vel_sign;
     an = vel[i]  / vel_interval;
     n[i] = (int)an;
     if (s1*n[i] < s1*an) n[i] = (int)(n[i] + s1);       
     if(n[i] == 0) 
       {
       nt[i] = (int)(max_log + 2); 
       }
     else
       {
       tempd = (float) ( abs((int)(n[i]*vel_interval)) + 0.023 );
       vel_log = log10(tempd);
       temp_log = (long)vel_log;
       if ((float)temp_log > vel_log) temp_log = temp_log - 1;     
       nt[i] = (int)(max(temp_log+3,2) + max_log);
       }       
     }

  NumToLabel = (abs(n[2]-n[1])) + 1;
  vmin  = _sp_active->minImageVel() 
        + ( n[1]*vel_interval-_sp_active->minImageVel());
  vmax  = _sp_active->minImageVel() 
        + ( n[2]*vel_interval-_sp_active->minImageVel());
  x_inc = (_sp_active->maxImageVel() - _sp_active->minImageVel()) 
        / _image_width;

  VelLabelIncr = vel_interval / x_inc; 
  labellen = MAX_LABEL_LENGTH;
  half_len = (labellen / 2.0) * charWidth(True);

  if(!_sp_active->useLogarithmicX())
    {
    tempx1 = (vmin-_sp_active->minImageVel()) / x_inc + _xoffset;
    //x1 = (long) floor(tempx1 + 0.5);
    x1 = tempx1;
    }
  else
    {
    if(vmin > 0.0 && _sp_active->gridX1() > 0.0 && _sp_active->gridX2() > 0.0)
      {
            x1 = ( (float)_hc_active->leftBorderWidth() +
                   getLogarithmicPointFromValue(vmin, _sp_active->gridX1(),
                                                _sp_active->gridX2(),
                                                _image_width));
      }
    else
      {
            x1 = ( (float)_hc_active->leftBorderWidth() + 
                   ((vmin) - _sp_active->gridX1()) / x_inc );
      }
    }

  



  y1 = _hc_active->topBorderHeight();
  x2 = _xoffset;
  y2 = _hc_active->topBorderHeight() - 6 * charHeight(True);

    /*vel location annotation*/
  i = (_sp_active->numHeaders() * (_sp_active->firstTrace() - 1)) 
    + (_sp_active->header1() - 1);
  vel_loc = headary[i];
  if(_sp_active->plotType() != PlotImage::PlotGRID &&
     _sp_active->plotType() != PlotImage::PlotARRAY )
    {
    sprintf(label2,"HDR# %d ID = %8.3f",_sp_active->header1(),vel_loc);
    labellen2 = (float)strlen(label2);
    _hc_active->writeConstrainedText(x2, y2, 1.0, .25, label2,
                                     _coordinate_system);
    y2 += charHeight(True);
    i = (_sp_active->numHeaders() * (_sp_active->firstTrace() - 1))
           + (_sp_active->header2() - 1);
    vel_loc = headary[i];
    sprintf(label2,"HDR# %d ID = %8.3f",_sp_active->header2(),vel_loc);
    labellen2 = (float)strlen(label2);
    _hc_active->writeConstrainedText(x2, y2, 1.0, .25, label2,
                                     _coordinate_system);
    }

  oldx = 0;
  for(i=0;i<NumToLabel;i++) 
     {
     
     if( (x1 - oldx > MAX_LABEL_LENGTH * charWidth(True) *.75) && 
         (x1 <= _image_width + _xoffset)
          && (x1 >= _xoffset) )
        {
        if(!_sp_active->getDrawXlines())
          {
          y1 = _hc_active->topBorderHeight() - charHeight(True);
          y2 = _hc_active->topBorderHeight();  
          }
        else
          {
          y1 = _hc_active->topBorderHeight() - charHeight(True);
          y2 = y1 + _image_height + charHeight(True);
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

        _hc_active->setLineWidth(_normal_line_width);
        _hc_active->drawLine(x1,y1,x1,y2,_coordinate_system);
        ivel = vmin + i * vel_interval;
/*
        convert_ff2ss(&ivel, label, &max_char, &num_decimals);
*/
        str_ff2ss(ivel, label, max_char, num_decimals);
        _hc_active->setTextHeight(charHeight(True));
        _hc_active->setLabelPlacement(HardCopyPlot::DeadCenter);
        _hc_active->writeConstrainedText(x1, y1 - charHeight(True),
                                         1.0, .25, label,
                                         _coordinate_system);
        oldx = x1;
        } 

     if(!_sp_active->useLogarithmicX())
       {
       x1 += VelLabelIncr;
       }
     else
       {
       if(vmin + ((i+1) * vel_interval) > 0.0)
         {

         x1 =  _hc_active->leftBorderWidth() +
                 getLogarithmicPointFromValue(
                     ( (vmin )  +  (i+1) * vel_interval ),
                       _sp_active->gridX1(),
                       _sp_active->gridX2(),
                       _image_width);
         }
       }
     }

}


/**************************************************************************
 *********** A manual defined y axis annotation method       **************
 **************************************************************************/
void HardCopyAnnotation::annotateManualGridY(long /*frame_number*/)
{
 long labellen = 0;
 int max_char = 20;
 int num_decimals = 3;
 char label[20];
 float y1,y2,oldy;
 float line_x1, line_x2, label_x1;
 long i;
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
 float half_height;
 float y_inc;


  if(!_sp_active->manualTransformY())
    {
    tmin = _sp_active->plottedGridY1();;
    tmax = _sp_active->plottedGridY2();;
    }
  else
    {
    tmin = _sp_active->manualY1();
    tmax = _sp_active->manualY2();
    }
  if(tmin == tmax)  return;

  y_inc = (tmax - tmin)  / _image_height;

  vel[0] = tmin;
  vel[1] = tmax;

  tsign = (tmax - tmin > 0.0) ? 1.0 : -1.0;
  if(_sp_active->usingSymetricalAnnotation())
    range = max(_sp_active->minImageVel(),_sp_active->maxImageVel()) -
            min(_sp_active->minImageVel(),_sp_active->maxImageVel());
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
  label_incr = interval / y_inc;
  if(!_sp_active->useLogarithmicY())
    {
    tempy1 = (ymin-tmin) / y_inc + _hc_active->topBorderHeight();
    y1 = tempy1;
    }
  else
    {
    if(ymin > 0.0 && _sp_active->gridY1() > 0.0 && _sp_active->gridY2() > 0.0)
      {
      y1 = ( (float)_hc_active->topBorderHeight() +
             getLogarithmicPointFromValue(ymin, _sp_active->gridY1(),
                                                _sp_active->gridY2(),
                                                _image_height));
      }
    else
      {
      y1 = y1 = ( (float)_hc_active->topBorderHeight() + 
                   ((ymin) - _sp_active->gridY1()) / y_inc );
      }
    }

  y2 = y1;
  line_x1  = _hc_active->leftBorderWidth() - charWidth(True);
  ival = max(ymin,ymax); 
/*
  convert_ff2ss(&ival, label, &max_char, &num_decimals);
*/
  str_ff2ss(ival, label, max_char, num_decimals);
  labellen = strlen(label);
  line_x2  = _hc_active->leftBorderWidth();
  half_height = (int)((float)charWidth(True) / 2.0);
  
  oldy = 0;
  for(i=0;i<NumToLabel;i++) 
    {
    if(y1 >= _hc_active->topBorderHeight() && 
       y1 <= _hc_active->topBorderHeight() + _image_height)
      {

      _hc_active->drawLine(line_x1,y1,line_x2 + charWidth(True),
                           y2, _coordinate_system);

      _hc_active->drawLine(line_x2 + _image_width,y1,
                           line_x2 + _image_width + charWidth(True),
                           y2, _coordinate_system);

      if(_sp_active->getDrawYlines())
        _hc_active->drawLine(_hc_active->leftBorderWidth(), y1,
                             _hc_active->leftBorderWidth() + _image_width,
                             y2, _coordinate_system);
      }
    if((y1 - oldy >  charHeight(True) ) && 
       (y1 <= _image_height + _hc_active->topBorderHeight()) &&
       (y1 >= _hc_active->topBorderHeight()) )
      {
      ival = ymin + i * interval;
/*
      convert_ff2ss(&ival, label, &max_char, &num_decimals);
*/
      str_ff2ss(ival, label, max_char, num_decimals);
      labellen = strlen(label);
      label_x1 = _hc_active->leftBorderWidth() -
                ( (labellen + 1) * charWidth(True));
      _hc_active->writeConstrainedText(label_x1,
                                       y1 + .5 * charHeight(True), 
                                       1.0, .25, label,
                                       _coordinate_system);
      _hc_active->writeConstrainedText(_hc_active->leftBorderWidth() +
                                       _image_width + charWidth(True),
                                       y1 + .5 * charHeight(True), 
                                       1.0, .25, label,
                                       _coordinate_system);
      oldy = y1;
      }
    if(!_sp_active->useLogarithmicY())
      {
      tempy1 += label_incr;
      y1 = y2 = tempy1;
      }
    else
      {
      if(ymin + ((i+1) * interval) > 0.0)
        {
          y1 = y2 = ( (float)_hc_active->topBorderHeight() +
                       getLogarithmicPointFromValue(
                         ((ymin + _sp_active->gridY1()) + ((i+1) * interval)),
                         _sp_active->gridY1(),
                         _sp_active->gridY2(),
                         _image_height) );
        }
      } 
   }

}


//===========================================================================
//===========Get a position from a value in a logarithmic display ===========
//===========================================================================
float HardCopyAnnotation::getLogarithmicPointFromValue(
                                            float value, float start_coord,
                                            float end_coord, 
                                            float max_dimension)
{
double val_per_point;
double log_start;
double log_value;

  assert(value > 0.0 && start_coord > 0.0 && end_coord > 0.0);

  val_per_point = ( log10(end_coord) - log10(start_coord) ) / max_dimension;   
  log_start = log10(start_coord);
  log_value = log10(value);
  return  (float)( (log_value - log_start) / val_per_point );

}
