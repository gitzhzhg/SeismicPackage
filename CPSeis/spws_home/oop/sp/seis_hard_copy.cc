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
#include <math.h>
#include "cprim.h"
#include "sp/seis_plot.hh"
#include "sp/seis_color.hh"
#include "hardcopy/paper_plot.hh"
#include "hardcopy/hardcopy_plot.hh"
#include "hardcopy/hardcopy_trace_plot.hh"
#include "hardcopy/hardcopy_annotate.hh"
#include "hardcopy/hardcopy_raster.hh"
#include "hardcopy/hardcopy_contour.hh"
#include "sp/seis_hard_copy.hh"
#include "sp/inform_list.hh"



SeisHardCopy::SeisHardCopy(char *filename) :
       _filename(newstr(filename)),
       _width(0),
       _height(0),
       _override_marker_scale_factor(0),
       _do_annotation(True),
       _panel(False),
       _cbar_both_sides(False),
       _use_pip_extention(True),
       _frame(0),
       _cbar_deci_places(5)
{
}

SeisHardCopy::SeisHardCopy(SeisHardCopy *s)
{
  _filename                    = newstr(s->_filename);
  _width                       = s->_width;
  _height                      = s->_height;
  _override_marker_scale_factor= s->_override_marker_scale_factor;
  _do_annotation               = s->_do_annotation;
  _panel                       = s->_panel;
  _cbar_both_sides             = s->_cbar_both_sides;
  _use_pip_extention           = s->_use_pip_extention;
  _frame                       = s->_frame;
  _cbar_deci_places            = s->_cbar_deci_places;
}

void SeisHardCopy::setFilename(char *filename)
{
 if (_filename) free (_filename);
 _filename= newstr(filename);
}

void SeisHardCopy::setWidthHeight(float width, float height)
{
  _width= width;
  _height= height;
}

void SeisHardCopy::setMarkerScaleFactor(float factor)
{
 _override_marker_scale_factor= factor;
}

void SeisHardCopy::setDoAnnotation(Boolean doit)
{
 _do_annotation= doit;
}

void SeisHardCopy::setColorBarBothSides(Boolean both)
{
 _cbar_both_sides= both;
}

void SeisHardCopy::setPanelMovie(Boolean panel)
{
  _panel= panel;
}

void SeisHardCopy::setUsePipExtension(Boolean use_pip)
{
 _use_pip_extention= use_pip;
}

void  SeisHardCopy::setFrame(int f) 
{
  _frame= f;
}

/*
 * returns is the plot is a wiggle type 
 */
static Boolean isWiggle(long ptype) 
{
  Boolean retval= False;
  if (ptype == PlotImage::PlotWONLY || ptype == PlotImage::PlotWFILL)
         retval= True;
  return retval;
}

Boolean SeisHardCopy::useRasterPlotter(long ptype) 
{
  Boolean retval= False;
  if ( ptype == PlotImage::PlotARRAY   ||
       ptype == PlotImage::PlotCOLOR   ||
       ptype == PlotImage::PlotSEMB    ||
       ptype == PlotImage::PlotCONTOUR)
              retval= True;

  if ( (ptype == PlotImage::PlotWONLY || ptype == PlotImage::PlotWFILL) && 
       !_use_pip_extention)
              retval= True;
  return retval;
}

Boolean SeisHardCopy::doColorBarForFrame(SeisPlot *sp,
                                         Boolean   cbar_both_sides,
                                         Boolean   panel,
                                         int       frame,
                                         Side      side   )
                                
                                         
{
   Boolean   cbar_left= False;
   Boolean   cbar_right= False;
   Boolean   retval= False;
   long      ptype = sp->plotType();
   SeisPlot *under_sp      = sp->_chained_sp.top();

   /*
    * is I am doing a color plot or have an underlay plot
    */
   if ( !(isWiggle(ptype) || ptype == PlotImage::PlotHEADER) ||
        (under_sp                      && 
         under_sp->isPlotDisplayed())  ) {
         if (sp->movie() && (frame == sp->plottedFrames()) && panel ) {
              cbar_right= True;
         }
         if (sp->movie() && frame == 1 && cbar_both_sides && panel ) {
              cbar_left= True; 
         }
         else if ( !sp->movie() || !panel ) {
              cbar_right= True;
              if (cbar_both_sides) cbar_left= True; 
         }
   }
   if (side == OnLeft)  retval = cbar_left;
   if (side == OnRight) retval = cbar_right;
   return retval;
}


float SeisHardCopy::widthofPaneledFrame(SeisPlot *sp,
                                        float     total_width)
{
  float work_width= total_width;
  Boolean cbar_left=  doColorBarForFrame(sp, _cbar_both_sides, _panel, 1,
                                        OnLeft);
  Boolean cbar_right= doColorBarForFrame(sp, _cbar_both_sides, _panel,
                                         (int)sp->plottedFrames(), OnRight);
  if (cbar_left)  work_width -= 2;
  if (cbar_right) work_width -= 2;

  work_width /= sp->plottedFrames();

  cbar_left= doColorBarForFrame(sp, _cbar_both_sides, _panel, _frame, OnLeft);
  cbar_right=doColorBarForFrame(sp, _cbar_both_sides, _panel, _frame, OnRight);
  
  if (cbar_left)  work_width += 2;
  if (cbar_right) work_width += 2;

  return work_width;
}
                                        

void SeisHardCopy::drawColorBar(SeisPlot       *sp,
                                HardCopyRaster *hr, 
                                Boolean         cbar_left,
                                Boolean         cbar_right)
{
 float ws=2.0;
 if (!_do_annotation) ws=3.3;
 if (sp->_cbar_on_hardcopy == SeisPlot::On) {
       if (cbar_right) hr->drawColorBar(_width-ws,1.0,1.75,5.0,
                                                    _cbar_deci_places);
       if (cbar_left)  hr->drawColorBar(0.3 ,1.0,1.75,5.0,
                                                    _cbar_deci_places);
 }
 else if (sp->_cbar_on_hardcopy == SeisPlot::Square9)  {
       if (cbar_right)hr->drawSquare9(_width- ws - 0.2, 2.0,2.0,2.0);
       if (cbar_left) hr->drawSquare9(0.2, 2.0,2.0,2.0);
 }
}

Boolean SeisHardCopy::overUnderDoesNotMatch(SeisPlot *over_sp)
{
   Boolean retval= False;
   if (over_sp) {
      float over_x1, under_x1, over_y1, under_y1;
      over_sp->getOverUnderXYs(&over_x1,  &over_y1,  NULL,  NULL,
                               &under_x1, &under_y1, NULL,  NULL);

      if ( (over_x1 != under_x1) || (over_y1 != under_y1) )
              retval= True;

   }
   return retval;

}




void SeisHardCopy::computeOverUnderLocs( SeisPlot     *sp,
                                         SeisPlot     *over_sp,
                                         HardCopyPlot *hc,
                                         float        *raster_x,
                                         float        *raster_y,
                                         float        *raster_width,
                                         float        *raster_height )
{

  float over_x1, under_x1, over_y1, under_y1;
  float over_x2, under_x2, over_y2, under_y2;
  float over_width= hc->drawingAreaWidth();
  float x_delta, y_delta;
  float screen_plot_width, screen_plot_height;
  Display *dpy= XtDisplay(sp->W());
  int screen_no= XScreenNumberOfScreen( XtScreen(sp->W()) );
  long pixel_width;
  long pixel_height;

  pixel_width=  over_sp->plottedWidth()  - over_sp->leftBorder() -
                                           over_sp->rightBorder();
  pixel_height= over_sp->plottedHeight() - over_sp->topBorder() -
                                           over_sp->bottomBorder();
 
  screen_plot_width= (1/over_sp->plottedTI())  * 
                              (fabs(hc->getX0() - hc->getX1()) +1) ;

  screen_plot_height= (float) pixel_height / 
                      over_sp->_image.verticalPixelsPerInch( dpy, screen_no); 


  x_delta= ( hc->drawingAreaWidth() * (1/over_sp->plottedTI()) ) /
                                                   screen_plot_width;
                

  sp->getOverUnderXYs(&over_x1,  &over_y1,  &over_x2,  &over_y2,
                      &under_x1, &under_y1, &under_x2, &under_y2);

  float x1, x2, x_offset;
  x1       = (sp == over_sp) ? over_x1 : under_x1;
  x2       = (sp == over_sp) ? over_x2 : under_x2;

  if (sp->rToL())
        x_offset = fabs(over_x2 - under_x2) * x_delta;
  else
        x_offset = fabs(over_x1 - under_x1) * x_delta;

  *raster_width = (fabs(x1 - x2) + 1) * x_delta;

  if (sp->imageXloc()) {
     *raster_x = hc->leftBorderWidth() + x_offset;
  }
  else 
     *raster_x = hc->leftBorderWidth();

  if (sp == over_sp) hc->setXMap( *raster_x );

  y_delta= ( hc->drawingAreaHeight() * 
              ( screen_plot_height/ 
                    (over_sp->plottedTmax()- over_sp->plottedTmin()) ) ) /
                                screen_plot_height; 

  float y1       = (sp == over_sp) ? over_y1 : under_y1;
  float y2       = (sp == over_sp) ? over_y2 : under_y2;
  float y_offset = fabs(over_y1 - under_y1) * y_delta;
  *raster_height = fabs(y1 - y2) * y_delta;

  if (sp->imageYloc()) {
     *raster_y = hc->topBorderHeight() + y_offset;
  }
  else {
     *raster_y= hc->topBorderHeight();
  }


}


void SeisHardCopy::wiggleWithPip(HardCopyPlot *hc,
                                 SeisPlot     *sp,
                                 SeisPlot     * /*under_sp*/,
                                 float         x_offset,
                                 float         y_offset,
                                 float         hard_is,
                                 float         plot_width)
{
   double lav;
   long ptype= sp->plotType();
   HardCopyTracePlot traces(hc);
   if (sp->_image._statusw) {
          wprocShowMsg(sp->_image._statusw, 
                              "Writing wiggle traces to CGM file...");
          XSync(XtDisplay(sp->_image._statusw),False);
   }

   traces.setYOffset(y_offset);
   traces.setRP( sp->rp() );
   traces.setRtoL( sp->rToL() );
   traces.setCT(sp->ct());
   traces.setIS( hard_is );
   traces.setInvert( sp->invert() );
   traces.setPlotWidth( plot_width );
   hc->setXMap(x_offset);

   if (ptype == PlotImage::PlotWONLY)
          traces.setPlotType(HardCopyTracePlot::Wiggle);
   else if (ptype == PlotImage::PlotWFILL)
          traces.setPlotType(HardCopyTracePlot::WiggleFill);
   traces.setSampleRate( sp->srval() );


   //CGM uses 128 as the maximum value
   lav = 128.0F;




   traces.plot(sp,  sp->plottedNplt(), sp->samplesPerTrace(),
              sp->displayedSamples(), sp->firstTraceIndex(),
              _frame, lav);

}


/*
 * ====================== Public Method ==============================
 *  make hardcopy plot
 */
int SeisHardCopy::writePlot(SeisPlot *sp)
{
  return writePlotWithHeader(sp);
}

/*
 * ====================== Public Method ==============================
 *  make hardcopy plot
 */
int SeisHardCopy::writePlotWithHeader(SeisPlot *sp, SeisPlot *header_graph)
{
   float width= _width;
   float height= _height;
   char *savestr= NULL;
   Display *dpy= XtDisplay(sp->W());
   int screen_num= XScreenNumberOfScreen( XtScreen(sp->W()) );
   float seis_width, seis_height;
   float head_width, head_height;
   float hc_seis_height;
   float hc_head_height;
   float total_width, total_height;  // total width & height of plot on screen
   long  original_frame= sp->currentFrame();


   assert(_filename);
   assert(strlen(_filename) > 0);

   /*
    * first get width of the seismic
    */
   seis_width=  sp->plottedWidth() / 
                   sp->_image.horizontalPixelsPerInch(dpy, screen_num);
   seis_height= sp->plottedHeight()/ 
                   sp->_image.verticalPixelsPerInch(dpy, screen_num);

   total_width= seis_width;
   total_height= seis_height;
   
   /*
    * then get width of the header plot if it exist
    */
   
   if (header_graph) {
     head_width=  header_graph->plottedWidth()  / 
                          sp->_image.horizontalPixelsPerInch(dpy, screen_num);
     head_height= header_graph->plottedHeight() /
                          sp->_image.verticalPixelsPerInch(dpy, screen_num);

     total_height+= head_height;
   } // end if

   if (width == 0)  width=  total_width;
   if (height == 0) height= total_height;
   if (sp->_image.getStatusWidget()) {
          savestr= wprocPushMsg(sp->_image.getStatusWidget(),
                                "Writing to CGM file...");
          XSync(XtDisplay(sp->_image.getStatusWidget()),False);
   }
   PaperPlot pp(_filename, width, height);


   if (header_graph) {
       hc_head_height= height * ( head_height / total_height);
       hc_seis_height= height * ( seis_height / total_height);
   } // header_graph
   else {
       hc_seis_height= height; 
   }


   SeisHardCopy head_shc(this);

   if (sp->plottedFrames() == 1 || !_panel ) {
        if (header_graph) {
             head_shc.setWidthHeight(width,hc_head_height);
             head_shc.writeHardCopy(header_graph, &pp, 0,hc_seis_height);
        }
        setWidthHeight(width, hc_seis_height);
        writeHardCopy(sp, &pp, 0,0 );
   } // end if
   else {
        float x=0;
        float pwidth;
        for(int i=0; (i<sp->plottedFrames()); i++) {
            sp->movieToFrame(i);
            setFrame(i+1);
            head_shc.setFrame(i+1);
            pwidth= widthofPaneledFrame(sp,width);
            if (header_graph) {
                 head_shc.setWidthHeight(pwidth,hc_head_height);
                 head_shc.writeHardCopy(header_graph, &pp, x,hc_seis_height);
            }
            setWidthHeight( pwidth, hc_seis_height);
            writeHardCopy(sp, &pp, x,0 );
            x += pwidth;
        }
        sp->movieToFrame((int)original_frame);

   } // end else


   if (sp->_image.getStatusWidget() && savestr)
          wprocPopMsg(sp->_image.getStatusWidget(), savestr);
   return 0;

}

Boolean SeisHardCopy::hasUnderlay(SeisPlot *sp, SeisPlot *under_sp) 
{
   Boolean retval= False;
   if (under_sp               && 
       sp->_image._chain_image &&
       under_sp->isPlotDisplayed() )
                      retval= True;
   return retval;
}


/*
 * ====================== Public Method ==============================
 *  make hardcopy plot
 */
int SeisHardCopy::writeHardCopy(SeisPlot     *sp,
                                PaperPlot    *pp,
                                float         x,
                                float         y,
                                HardCopyPlot *over_hc,
                                SeisPlot     *over_sp,
                                float         plotx0, 
                                float         plotx1,
                                float         ploty0,
                                float         ploty1)
{
  Display  *dpy           = XtDisplay(sp->W());
  SeisPlot *under_sp      = sp->_chained_sp.top();
  int       screen_num    = XScreenNumberOfScreen( XtScreen(sp->W()) );
  float     marker_scale  = 3.0;
  Boolean   compute_range = False;
  long      ptype         = sp->plotType();
  float     y_offset      = 0;
  Boolean   annotate      = True;
  float hard_is;
  float raster_height;
  float raster_width;
  float raster_x;
  float raster_y;
  float y_range;
  float *rgb;
  int i;
  Boolean doing_over_under= False;
  Boolean cbar_left= False;
  Boolean cbar_right= False;

  // ---------  BEGIN unresolved issues -----------------
   assert( sp->isPlotDisplayed() ); // need to show some message if no plot
                                    // i need a return code
                                    // metric is not completly delt with
  // ---------  END unresolved issues -----------------

   /*
    *  determine what the range of the data is
    */

   if ((plotx0 == 0) && (plotx1 == 0) && (ploty0 == 0) && (ploty1 == 0))
          compute_range= True;

   if (_width ==0)
          _width=  sp->plottedWidth()/
                        sp->_image.horizontalPixelsPerInch(dpy,screen_num);
   if (_height ==0)
          _height= sp->plottedHeight()/
                        sp->_image.verticalPixelsPerInch(dpy, screen_num);

   y_offset= sp->imageYloc() * fabs(sp->yvaluePerPixel());




   /*
    *  Now make object and start setting parameters 
    * 
    */
   HardCopyPlot hc(pp,sp);
   cbar_right= doColorBarForFrame(sp, _cbar_both_sides, _panel, _frame,
                                  SeisHardCopy::OnRight);
   cbar_left = doColorBarForFrame(sp, _cbar_both_sides, _panel, _frame,
                                  SeisHardCopy::OnLeft);
   if (cbar_right) hc.setRightBorderWidth( hc.rightBorderWidth() +2);
   if (cbar_left)  hc.setLeftBorderWidth( hc.leftBorderWidth() +2);
   if (ptype == PlotImage::PlotHEADER) hc.setBottomBorderHeight(0.0);

   hc.setWidthHeight(x,y, _width, _height);
   if (compute_range) getRangeForHC( sp, &plotx0, &plotx1, &ploty0, &ploty1,
                                     (hasUnderlay(sp,under_sp) || over_sp) );
   hc.setRange( plotx0, ploty0, plotx1, ploty1);


  /*
   * ============ Underlay Section- Determine if we have an underlay =====
   */

   if (hasUnderlay(sp, under_sp)) {
         writeHardCopy(under_sp, pp,0,0, &hc,sp,
                       plotx0, plotx1, ploty0, ploty1);
         annotate= False;
         doing_over_under= True;
         over_sp= sp;
   } // end if
   else if (over_hc && over_sp) {
         doing_over_under= True; 
         under_sp= sp;
   }

  /*
   * ================ Layout Section- Determine where plot(s) go ============
   */
   if (hc.goodFile()) {
        y_range = (float)fabs(ploty1-ploty0);
        if (y_range == 0.0) y_range= 1.0;
        hard_is= hc.drawingAreaHeight()/y_range;
        if (doing_over_under) {
            computeOverUnderLocs( sp, over_sp, &hc, &raster_x, &raster_y,
                                  &raster_width, &raster_height);
        }
        else {
            raster_x     =  hc.leftBorderWidth(); 
            raster_y     =  hc.topBorderHeight();
            raster_width =  hc.drawingAreaWidth();
            raster_height=  hc.drawingAreaHeight();
        }
         

  /*
   * ================ Data Section- Raster or Pip =================
   */
        if (useRasterPlotter(ptype)  ){
           rgb= sp->_sc->rgbOfPlot();
           for (i = 0; i<1024; i++) rgb[i]=  sp->_user->_cbar_rgb[i];

           if (sp->_image._statusw) {
                 wprocShowMsg(sp->_image._statusw, 
                              "Writing raster image to CGM file...");
                 XSync(XtDisplay(sp->_image._statusw),False);
           }
           if ((ptype == PlotImage::PlotSEMB &&
                sp->plottingContoursOnly() == False)  ||
               (ptype == PlotImage::PlotISO &&
                sp->plottingContoursOnly() == False) ||
               (ptype == PlotImage::PlotCOLOR &&
                sp->plottingContoursOnly() == False)){
              Boolean color_plot= True;
              HardCopyRaster hr(&hc, sp, sp->_user->_num_cbar_cols, 
                                rgb, &sp->_image._MapColors[0],
                                raster_x, raster_y, 
                                raster_x + raster_width,
                                raster_y + raster_height,
                                200.0, _frame, !isWiggle(ptype) );
              if (!isWiggle(ptype)) drawColorBar(sp, &hr, 
                                                 cbar_left, cbar_right);
              //draw contours on semblance
              if (ptype == PlotImage::PlotSEMB && sp->contours() ||
                  sp->plotWithContours()/*iso with contours*/) {
                  HardCopyContour hcc(&hc, sp, 200, _frame);
              }
           }
           else  {//semblance or iso type with contours only
              HardCopyContour hcc(&hc, sp, 200, _frame);
           }
        }  // end if useRasterPlotter

        else if ( isWiggle(ptype) ) {
             wiggleWithPip(&hc, sp, under_sp,
                           raster_x, y_offset, hard_is, raster_width);
        } // wiggle plots

  /*
   * ================ Annotation Section  =================
   */
        if (annotate && _do_annotation) {
          if (!doing_over_under) { //not a overlay/underlay combination
              HardCopyAnnotation ha(&hc, sp, NULL, NULL, 0.0, 0.0, 
                                    0.0, 0.0, 0.0, 0.0, 0.0, 0.0,_frame);
          }
          else {
              float o_raster_x, o_raster_y;
              float o_raster_width, o_raster_height;
              computeOverUnderLocs(over_sp, over_sp, &hc, 
                                   &o_raster_x, &o_raster_y,
                                   &o_raster_width, &o_raster_height);
              HardCopyAnnotation ha(over_hc,  over_sp, &hc,   sp, 
                                    o_raster_x, o_raster_x+o_raster_width,
                                    o_raster_y, o_raster_y+o_raster_height,
                                    raster_x, raster_x+raster_width,
                                    raster_y, raster_y+raster_height,
                                    _frame);
          } // end else
        } // end if annotate
  /*
   * ================ Vector Section  =================
   */
        if (sp->_image._statusw) {
             wprocShowMsg(sp->_image._statusw, 
                                     "Writing vector draws to CGM file...");
             XSync(XtDisplay(sp->_image._statusw),False);
        }
        if (_override_marker_scale_factor >  0.001)
                  marker_scale= _override_marker_scale_factor;
        sp->_inform_list->callWriteToHardCopy(sp, &hc, marker_scale);
   }
   return 0;
}





void SeisHardCopy::getRangeForHC(SeisPlot     *sp,
                                 float        *plotx0, 
                                 float        *plotx1,
                                 float        *ploty0,
                                 float        *ploty1,
                                 Boolean      range_includes_underlay)
{
  long ptype = sp->plotType();
  float over_x1, over_x2, under_x1, under_x2;  
  float over_y1, over_y2, under_y1, under_y2; 

   /*
    *   This next sections computes the range (x0,x1,y0,y1) for the 
    *   plot.  It takes into account the total range when you have an
    *   underlay plot.
    */
  if(range_includes_underlay)
    {
    sp->getOverUnderXYs(&over_x1,  &over_y1,  &over_x2,  &over_y2,
                        &under_x1, &under_y1, &under_x2, &under_y2);
    if(over_x1 < over_x2){       //increasing x
          *plotx0 = min(over_x1, under_x1);
          *plotx1 = max(over_x2, under_x2);
    }
    else { //decreasing x
          *plotx0 = max(over_x1, under_x1);
          *plotx1 = min(over_x2, under_x2);
    }
    if ((ptype == PlotImage::PlotWONLY  ||
         ptype == PlotImage::PlotWFILL  ||
         ptype == PlotImage::PlotCOLOR) && sp->rToL() ) {
          float tmp= *plotx0;
          *plotx0= *plotx1;
          *plotx1= tmp;
    }
    if(over_y1 < over_y2) { //increasing y
          *ploty0 = min(over_y1, under_y1);
          *ploty1 = max(over_y2, under_y2);
    }
    else { //decreasing y
          *ploty0 = max(over_y1, under_y1);
          *ploty1 = min(over_y2, under_y2);
    }
  }
  else { //No underlay
    if ((ptype ==PlotImage:: PlotWONLY)   ||
       (ptype == PlotImage::PlotWFILL)||(ptype == PlotImage::PlotCOLOR) ) {
      *plotx0= sp->firstTraceIndex() + 1;
      *ploty0= sp->plottedTmin();
      *ploty1= sp->plottedTmax();
      if (sp->rToL()) {
        *plotx1= sp->firstTraceIndex() - sp->plottedNplt() + 2;
      }  
      else {
        *plotx1= sp->firstTraceIndex() + sp->plottedNplt();
      } 
    }
    else  { // a grid plot or an array type plot
      *plotx0= sp->plottedGridX1();
      *ploty0= sp->plottedGridY1();
      *plotx1= sp->plottedGridX2();
      *ploty1= sp->plottedGridY2();
    }
  }
}
