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
#ifndef SEIS_HARD_COPY_HH
#define SEIS_HARD_COPY_HH

#include "sp/seis_plot.hh"
class SPList;
class PaperPlot;
class HardCopyPlot;
class HardCopyRaster;

class SeisHardCopy {

  friend class SeisPlot;
  friend class SeisPlotTie;
  private:

       char     *_filename;
       float     _width;
       float     _height;
       float     _override_marker_scale_factor;
       int       _frame;
       int       _cbar_deci_places;
       Boolean   _do_annotation;
       Boolean   _panel;
       Boolean   _cbar_both_sides;
       Boolean   _use_pip_extention;


     int writePlotWithHeader(SeisPlot *sp, SeisPlot *header_graph = NULL);
     int writePlot(SeisPlot *sp);

     int writeHardCopy( SeisPlot     *sp,
                        PaperPlot    *pp,
                        float         x,
                        float         y,
                        HardCopyPlot *over_hc=NULL,
                        SeisPlot     *over_sp=NULL,
                        float         plotx0 =0.0,
                        float         plotx1 =0.0,
                        float         ploty0 =0.0,
                        float         ploty1 =0.0);

     void getRangeForHC(SeisPlot     *sp,
                        float        *plotx0,
                        float        *plotx1,
                        float        *ploty0,
                        float        *ploty1,
                        Boolean      range_includes_underlay = False);

     void wiggleWithPip(HardCopyPlot *hc,
                        SeisPlot     *sp,
                        SeisPlot     *under_sp,
                        float         x_offset,
                        float         y_offset,
                        float         hard_is,
                        float         plot_width);

     Boolean useRasterPlotter(long ptype);
     void drawColorBar(SeisPlot       *sp, 
                       HardCopyRaster *hr,
                       Boolean         cbar_left,
                       Boolean         cbar_right);

     Boolean overUnderDoesNotMatch(SeisPlot *over_sp);

     void computeOverUnderLocs( SeisPlot     *sp,
                                SeisPlot     *over_sp,
                                HardCopyPlot *hc,
                                float        *raster_x,
                                float        *raster_y,
                                float        *raster_width,
                                float        *raster_height );
     Boolean hasUnderlay(SeisPlot *sp, SeisPlot *under_sp);


  public:
       enum Side {OnLeft, OnRight};

       SeisHardCopy(char *filename);
       SeisHardCopy(SeisHardCopy*);



       void setFilename(char *filename);
       void setWidthHeight(float width, float height);
       void setMarkerScaleFactor(float factor);
       void setDoAnnotation(Boolean doit);
       void setColorBarBothSides(Boolean both);
       void setPanelMovie(Boolean panel);
       void setUsePipExtension(Boolean use_pip);
       void setFrame(int frame);

       void writePaneledSetHardcopy(SPList *);
       void computePaneledSetWidthHeight(float  seismic_width,
                                         float  seismic_height,
                                         float *tot_width,
                                         float *tot_height);
       static Boolean doColorBarForFrame(SeisPlot *sp,
                                         Boolean   cbar_both_sides,
                                         Boolean   panel,
                                         int       frame,
                                         Side      side);
       float widthofPaneledFrame(SeisPlot *sp, float     total_width);
                                

};



#endif

