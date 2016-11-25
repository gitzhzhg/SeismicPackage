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
//========================= COPYRIGHT NOTICE ================================
//====  CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.      ========
//====   PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK       ========
//======================== COPYRIGHT NOTICE =================================


//===========================================================================
//========== Main class for user input for image generation       ===========
//========== Michael L. Sherrill 11/91 (C++ version 4/97)         ===========
//===========================================================================

#ifndef __IMAGEINPUT

#define __IMAGEINPUT


#include "tfio.h"

class ImageInput
{
  
  friend class PlotImage;

  private:

  protected:


  public:
    int          _hi_resolution;    /*Use float mapping of color bar*/
    long         _center_percent;   /*Place zero amps at center of color bar*/
    long         _do_color;         /*do cps type color processing*/
    long         _do_median;        /*rescale to median of data*/
    long         _do_percent;       /*calculate color percentages*/
    long         _do_amps;          /*scale color to trace amplitudes*/
    long         _pnc;              /*percent of negative to color*/
    long         _ppc;              /*percent of positive to color*/ 
    float        _color_ampmin;     /*min amplitude to color*/
    float        _color_ampmax;     /*max amplitude to color*/ 
    float        _cbar_rgb[1024];   /*cps color file rgb values*/
    float        _contour_rgb[1024];/*contour rgb values*/
    double       _external_amplitude;/*scale multiple data sets with*/
    long         _num_cbar_cols;    /*number of colors in cps color file*/
    long         _overlay;          /*overlay trace increment*/
    long         _mode;             /*wiggle trace or variable density*/
    long         _movie;            /*boolean for movie option*/
    long         _frames;           /*number of images to create*/
    long         _skip_frames;      /*number of frames between to skip*/
    char         _filename[200];    /*trace data file*/
    long         _iskp;             /*traces to initially skip*/
    long         _ndo;              /*traces to do between skip do*/
    long         _nplt;             /*traces to display*/
    long         _nskp;             /*traces to skip between skip do*/
    float        _tmin;             /*first time to read*/
    float        _tmax;             /*ending time to read*/
    long         _tdec;             /*sample decimation in the read*/
    long         _norm;             /*normalize the data when read*/
    int          _scale_to_file;    /*scale data to lav of entire file*/
    int          _scale_to_panel;   /*scale to lav of image data displayed*/
    long         _gradev;           /*vertical variable density grade*/
    long         _gradeh;           /*horizontal variable density grade*/
    long         _rp;               /*reverse data polarity*/
    long         _RtoL;             /*display data right to left*/
    long         _invert_yaxis;     /*invert the y of the image*/
    float        _ti;               /*traces per inch of display*/
    float        _is;               /*inches per second of display*/
    long         _metric;           /*plot in metric scale*/
    long         _depth;            /*plot in depth instead of time*/
    float        _ct;               /*channels per trace in display*/
    float        _clipping_factor;  /*to be implemented later*/
    long         _match_xheader;    /*x header to match different data sets*/
    long         _match_yheader;    /*y header to match different data sets*/
    long         _hdrOne;           /*primary header annotation*/
    long         _hdrTwo;           /*secondary header annotation*/
    long         _firstLbl;         /*trace to start annotation*/
    long         _xlabel_header;    /*header to use for annotating iso plots*/
    long         _active_header;    /*general purpose header reference*/
    long         _y_axis_header;    /*header to label y axis by*/
    long         _num_y_labels;     /*number of y axis labels to annotate*/
    int          _draw_x_lines;     /*draw lines thru image at x label locs*/
    int          _draw_y_lines;     /*draw lines thru image at y label locs*/
    int          _label_by_headers; /*use header values to annotate by*/
    int          _symetrical_anno;  /*make x and y annotation symetrical*/
    int          _annotate;         /*true or false to annotate the image*/
    long         _LblInc;           /*increment of annotation*/
    double       _ptl;              /*primary timing line to annotate*/
    double       _stl;              /*secondary timing line to annotate*/
    char         _plotLabel[200];   /*user plot label*/
    int          _plotlabel_x;      /*x location for plot label*/
    int          _plotlabel_y;      /*y location for plot label*/
    int          _plotlabel_only;   /*only annotate a label*/
    struct GLBL  _G;                /*data global information*/
    Grid3DDesc   _3dDesc;           /*holds 3d file coordinate info*/
    long         _color_data;       /*data is in color or gray*/
    int          _with_contours;    /*make plot with contours ( with iso type)*/
    int          _contours_only;    /*plot with contours only ( with iso type)*/
    float        _minp;             /*minimum percent (or value) to contour*/
    float        _maxp;             /*maximum percent (or value) to contour*/
    long         _contours;         /*number of contour levels to draw*/
    float        _contour_increment;/*contour level increment*/
    float        _grid_x1;          /*user starting x grid coordinate*/
    float        _grid_x2;          /*user ending   x grid coordinate*/
    float        _grid_y1;          /*user starting y grid coordinate*/
    float        _grid_y2;          /*user ending   y grid coordinate*/
    float        _grid_xinc;        /*user x grid increment*/
    float        _grid_yinc;        /*user y grid increment*/
    long         _grid_width;       /*pixel width  of grid*/
    long         _grid_height;      /*pixel height of grid*/
    float        _vel_min;          /*minimum image velocity*/
    float        _vel_max;          /*maximum image velocity*/
    long         _vary_size;        /*size of array to store velocities*/
    int          *_read_request_array;    /*controls random trace reading*/
    int          _number_of_read_requests;/*controls random trace reading*/

    ImageInput();
    ~ImageInput();
    long    getDoColor(){return _do_color;}
    void    setDoColor(long l){_do_color = l;}
    long    getDoMedian(){return _do_median;};
    void    setDoMedian(long l){_do_median = l;}
    long    getDoPercent(){return _do_percent;}
    void    setDoPercent(long l){_do_percent = l;}
    long    getDoAmps(){return _do_amps;};
    void    setDoAmps(long l){_do_amps = l;}
    long    getPnc(){return _pnc;}
    void    setPnc(long l){_pnc = l;}
    long    getPpc(){return _ppc;}
    void    setPpc(long l){_ppc = l;}
    float   getColorAmpmin(){return _color_ampmin;}
    void    setColorAmpmin(float f){_color_ampmin = f;}
    float   getColorAmpmax(){return _color_ampmax;}
    void    setColorAmpmax(float f){_color_ampmax = f;}
    float   getCbarRgb(long l){return _cbar_rgb[l];}
    void    setCbarRgb(long l, float f){_cbar_rgb[l] = f;}
    float   getContourCbarRgb(long l){return _contour_rgb[l];}
    void    setContourCbarRgb(long l, float f){_contour_rgb[l] = f;}
    double  getExternalAmplitude(){return _external_amplitude;}
    void    setExternalAmplitude(double d){_external_amplitude = d;}
    long    getNumberCbarColors(){return _num_cbar_cols;}
    void    setNumberCbarColors(long l){_num_cbar_cols = l;}
    long    getOverlayIncrement(){return _overlay;}
    void    setOverlayIncrement(long l){_overlay = l;}
    long    getMode(){return _mode;}
    void    setMode(long l){_mode = l;}
    long    getMovieOption(){return _movie;}
    void    setMovieOption(long l){_movie = l;}
    long    getFrames(){return _frames;}
    void    setFrames(long l){_frames = l;}
    long    getSkipFrames(){return _skip_frames;}
    void    setSkipFrames(long l){_skip_frames = l;}
    char    *getFilename(){return &_filename[0];}
    long    getInitialSkip(){return _iskp;}
    void    setInitialSkip(long l){_iskp = l;}
    long    getNumberToDo(){return _ndo;}
    void    setNumberToDo(long l){_ndo = l;}
    long    getNumberToPlot(){return _nplt;}
    void    setNumberToPlot(long l){_nplt = l;}
    long    getNumberToSkip(){return _nskp;}
    void    setNumberToSkip(long l){_nskp = l;}
    float   getTmin(){return _tmin;}
    void    setTmin(float f){_tmin = f;}
    float   getTmax(){return _tmax;}
    void    setTmax(float f){_tmax = f;}
    long    getTdec(){return _tdec;}
    void    setTdec(long l){_tdec = l;}
    long    getNorm(){return _norm;}
    void    setNorm(long l){_norm = l;}
    int     getScaleToFile(){return _scale_to_file;}
    void    setScaleToFile(int b){_scale_to_file = b;}
    int     getScaleToPanel(){return _scale_to_panel;}
    void    setScaleToPanel(int b){_scale_to_panel = b;}
    long    getGradeHorizontal(){return _gradeh;}
    void    setGradeHorizontal(long l){_gradeh = l;}
    long    getGradeVertical(){return _gradev;}
    void    setGradeVertical(long l){_gradev = l;}
    long    getReversePolarity(){return _rp;}
    void    setReversePolarity(long l){_rp = l;}
    long    getRightToLeft(){return _RtoL;}
    void    setRightToLeft(long l){_RtoL = l;}
    long    getInvertedYaxis(){return _invert_yaxis;}
    void    setInvertedYaxis(long l){_invert_yaxis = l;}
    float   getTracesPerInch(){return _ti;}
    void    setTracesPerInch(float f){_ti = f;}
    float   getInchesPerSecond(){return _is;}
    void    setInchesPerSecond(float f){_is = f;}
    long    getMetricMode(){return _metric;}
    void    setMetricMode(long l){_metric = l;}
    long    getDepthMode(){return _depth;}
    void    setDepthMode(long l){_depth = l;}
    float   getChannelsPerTrace(){return _ct;}
    void    setChannelsPerTrace(float f){_ct = f;}
    float   getClippingFactor(){return _clipping_factor;}
    void    setClippingFactor(float f){_clipping_factor = f;}
    long    getMatchXheader(){return _match_xheader;}
    void    setMatchXheader(long l){_match_xheader = l;}
    long    getMatchYheader(){return _match_yheader;}
    void    setMatchYheader(long l){_match_yheader = l;}
    long    getPrimaryAnnotationHeader(){return _hdrOne;}
    void    setPrimaryAnnotationHeader(long l){_hdrOne = l;}
    long    getSecondaryAnnotationHeader(){return _hdrTwo;}
    void    setSecondaryAnnotationHeader(long l){_hdrTwo = l;}
    void    setXLabelHeader(long l){_xlabel_header = l;}
    long    getXLabelHeader(){return _xlabel_header;}
    long    getFirstTraceToAnnotate(){return _firstLbl;}
    void    setFirstTraceToAnnotate(long l){_firstLbl = l;}
    long    getActiveHeader(){return _active_header;}
    void    setActiveHeader(long l){_active_header = l;}
    long    getYannotationHeader(){return _y_axis_header;}
    void    setYannotationHeader(long l){_y_axis_header = l;}
    long    getNumberYlabels(){return _num_y_labels;}
    void    setNumberYlabels(long l){_num_y_labels = l;}
    int     getDrawXlines(){return _draw_x_lines;}
    void    setDrawXlines(int b){_draw_x_lines = b;}
    int     getDrawYlines(){return _draw_y_lines;}
    void    setDrawYlines(int b){_draw_y_lines = b;}
    int     getUserHeaderAnnotation(){return _label_by_headers;}
    void    setUserHeaderAnnotation(int b){_label_by_headers = b;}
    int     getSymetricalAnnotation(){return _symetrical_anno;}
    void    setSymetricalAnnotation(int b){_symetrical_anno = b;}
    int     getAnnotateImage(){return _annotate;}
    void    setAnnotateImage(int b){_annotate = b;}
    long    getXlabelIncrement(){return _LblInc;}
    void    setXlabelIncrement(long l){_LblInc = l;}
    double  getPrimaryTimingLine(){return _ptl;}
    void    setPrimaryTimingLine(double d){_ptl = d;}
    double  getSecondaryTimingLine(){return _stl;}
    void    setSecondaryTimingLine(double d){_stl = d;}
    char    *getPlotLabel(){return &_plotLabel[0];}
    int     getPlotLabelX(){return _plotlabel_x;}
    void    setPlotLabelX(int i){_plotlabel_x = i;}  
    int     getPlotLabelY(){return _plotlabel_y;}
    void    setPlotLabelY(int i){_plotlabel_y = i;}
    int     getLabelPlotOnly(){return _plotlabel_only;}
    void    setLabelPlotOnly(int b){_plotlabel_only = b;}
    long    getHaveColorData(){return _color_data;}  
    void    setHaveColorData(long l){_color_data = l;}
    float   getContourMinimumVelocity(){return _minp;}
    void    setContourMinimumVelocity(float f){_minp = f;}
    float   getContourMaximumVelocity(){return _maxp;}
    void    setContourMaximumVelocity(float f){_maxp = f;}
    long    getNumberOfContours(){return _contours;}
    void    setNumberOfContours(long l){_contours = l;}
    void    setPlotWithContours(int i) {_with_contours = i;}
    int     plotWithContours(){return _with_contours;}
    void    setContoursOnly(int i) {_contours_only = i;}
    int     plottingContoursOnly(){return _contours_only;}
    void    setContourIncrement(float inc){_contour_increment = inc;}
    float   getContourIncrement(){return _contour_increment;}
    float   getX1(){return _grid_x1;}
    void    setX1(float f){_grid_x1 = f;}
    float   getX2(){return _grid_x2;}
    void    setX2(float f){_grid_x2 = f;}
    float   getY1(){return _grid_y1;}
    void    setY1(float f){_grid_y1 = f;}
    float   getY2(){return _grid_y2;}
    void    setY2(float f){_grid_y2 = f;}
    float   getXgridIncrement(){return _grid_xinc;}
    void    setXgridIncrement(float f){_grid_xinc = f;}
    float   getYgridIncrement(){return _grid_yinc;}
    void    setYgridIncrement(float f){_grid_yinc = f;}
    long    getGridHeight(){return _grid_height;}
    void    setGridHeight(long l){_grid_height = l;}
    long    getGridWidth(){return _grid_width;}
    void    setGridWidth(long l){_grid_width = l;}
    float   getVelocityMinimum(){return _vel_min;}
    void    setVelocityMinimum(float f){_vel_min = f;}
    float   getVelocityMaximum(){return _vel_max;}
    void    setVelocityMaximum(float f){_vel_max = f;}
    long    getVelocityArraySize(){return _vary_size;}
    void    setVelocityArraySize(long l){_vary_size = l;}
    struct  GLBL getGlobals(){return _G;}
    void    setReadRequestArray(int *array){_read_request_array = array;}
    int     *getReadRequestArray(){return _read_request_array;}
    void    setNumberOfReadRequests(int n){_number_of_read_requests = n;}
    int     getNumberOfReadRequests(){return _number_of_read_requests;} 
    int     useHiResolution(){return _hi_resolution;}
    void    setHiResolution(int set){_hi_resolution = set;}
    long    getCenterPercent(){return _center_percent;}
    void    setCenterPercent(long set){_center_percent = set;}
};


#endif
