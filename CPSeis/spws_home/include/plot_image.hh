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
//========== Main class for image generation                      ===========
//========== Michael L. Sherrill 11/91 (C++ version 4/97)         ===========
//===========================================================================

#ifndef __PLOTIMAGEDEF
#define __PLOTIMAGEDEF

#include "image_input.hh"
#include "wproc.h"
#include <X11/Intrinsic.h>
#include <assert.h>

/* macros for min and maximum value*/
#define max(a, b) (  (a) > (b) ? (a) : (b)  ) 
#define min(a, b) (  (a) < (b) ? (a) : (b)  )

typedef int (*ImageAbortFunction)(void*);
typedef void(*ImageXYOutputFunction)(void*, int x, int y);
typedef void(*ImageExternalFunction)(void*);

enum{ MAX_PIXMAP = 100000, MAX_ZOOM = 200,    NO_ZOOM = 100 };

class AmplitudeRecovery;
class NetEnv;
class Ximage;
class PixmapSet;
class ColorInfoSegment;



//===========================================================================
//========== Main class for image generation                      ===========
//===========================================================================
class PlotImage
{

  friend class SeisPlot;
  friend class SeisPlotZoom;
  friend class SeisPlotUnder;
  friend class SeisHardCopy;
  friend class AmplitudeRecovery;

  private:
    class TraceSelector *_trace_selector;
    class NDoTraceSelection *_ndo_select;
    class TraceOrderAnalyzer *_analyzer;

  protected:
    Boolean           _filedata;             /*using data from a file*/
    long              _zoomed;               /*the display is zoomed*/
    long              _zoomdown;             /*zoom down the display*/
    long              _zoomup;               /*zoom up the display*/
    long              _zoom_scan;            /*zoomed and scanned*/
    double            _zoom_factor;          /*zoom factor*/
    long              _scanright;            /*getting next traces*/
    long              _scanleft;             /*getting previous traces*/
    long              _displayed;            /*for front end*/
    long              _trace_delta;          /*pixel width of traces*/
    long              _first_trace_location; /*in the display*/
    long              _nhdrs;                /* # of headers in each trace*/
    long              _frames;               /* current # displayed frames*/
    long              _skip_frames;          /* skip between frames*/
    float             _ti;                   /*the image's traces per inch*/
    float             _is;                   /*the image's inches per sec*/
    long              _original_samples;     /*before zoom*/
    long              _original_traces;      /*before zoom*/
    double            _original_is;          /*inches per second before zoom*/
    double            _original_ti;          /*traces per inch before zoom*/
    float             _original_velfactor;   /*original pixel to vel ratio*/
    long              _use_old_arrays;       /*flag to reallocate arrays*/
    long              _zindex;               /*zoom index 0-199, 100=no zoom*/
    double            _zoomxary[MAX_ZOOM][2];/*x data in the zoom*/
    double            _zoomyary[MAX_ZOOM][2];/*y data in the zoom*/
    long              _neg_fill;             /*fill in troughs instead*/
    long              _first_trace_in_image; /*1st trace in display*/
    long              _displayed_traces;     /* # of traces in display*/
    long              _displayed_samples;    /* # of samples in display*/
    long              _ntot;                 /* # of traces to read */
    long              _nsamp;                /* # of samples to read */
    long              _tpnl[MAX_PIXMAP];     /* # traces per image */
    double            _tmin;                 /*first time in display*/
    double            _tmax;                 /*last time in display*/
    Boolean           _manual_annotate;      /*use manual annotation*/
    float             _manual_grid_y1;       /*value to over-ride data*/
    float             _manual_grid_y2;       /*value to over-ride data*/
    float             _manual_grid_x1;       /*value to over-ride data*/
    float             _manual_grid_x2;       /*value to over-ride data*/
    double            _manual_y_value_per_pixel;/*computed to over-ride data*/
    double            _manual_x_value_per_pixel;/*computed to over-ride data*/
    double            _manual_scaler;      /*ratio of over-ride to real data*/
    Boolean           _use_logarithmic_x;    /*plot x axis is logarithmic*/
    Boolean           _use_logarithmic_y;    /*plot y axis is logarithmic*/
    float             _grid_x1;              /*the image's uc x1*/
    float             _grid_x2;              /*the image's uc x2*/
    float             _grid_y1;              /*the image's uc y1*/
    float             _grid_y2;              /*the image's uc y2*/
    double            _y_value_per_pixel;    /*for mouse y readout*/
    double            _x_value_per_pixel;    /*for mouse x readout*/
    double            _max_amplitude;        /*max in the data read in*/
    double            _min_amplitude;        /*min in the data read in*/
    double            _color_ratio;          /*index to color map*/
    double            _display_scale;        /*to fit data into display*/
    double            _aux_value;            /*external value for readout*/
    char              *_aux_label;           /*external value for readout*/
    float             *_float_array;         /*to read in float data*/
    float             *_hd;                  /*to hold header data*/
    unsigned char     *_byte_array;          /*to hold byte file data*/
    XSegment          *_segments;            /*to hold xlib drawing of xys*/
    long              _num_segs;             /*number of segments in array*/
    Ximage            *_ximage_ptr;          /*pointer to Ximage object*/
    XImage            _ximage;               /*Xlib image struct*/
    PixmapSet         *_pixmap_set;          /*knows about the pixmaps*/
    ColorInfoSegment  *_col_segment;         /*identifies this program elmnt*/
    ImageInput        *_user;                /*user input class*/
    long              _graph_width;          /*width of pixmap */
    long              _graph_height;         /*height of pixmap */
    long              _left_border;          /*border pixels*/
    long              _right_border;         /*border pixels*/
    long              _top_border;           /*border pixels*/
    long              _bottom_border;        /*border pixels*/
    long              _dest_x;               /*drawable x destination*/
    long              _dest_y;               /*drawable y destination*/
    long              _orig_dest_x;          /*before any dragging*/
    long              _orig_dest_y;          /*before any dragging*/
    Pixmap            _pixmary[MAX_PIXMAP];  /*all pixmaps used*/
    Pixmap            _bitmap_pixmap;        /*for single plane*/
    int               _cpixm;                /*current pixmap displayed */
    GC                _gc1;                  /*normal context for graph*/
    GC                _gc2;                  /*bold context for annotation*/
    GC                _pick_gc;              /*pick marking*/
    GC                _bitmap_gc1;           /*for single plane*/
    GC                _bitmap_gc2;           /*for single plane*/
    XFontStruct       *_font_fixed;          /*fixed font structure*/
    XFontStruct       *_font_bold;           /*bold font structure*/
    long              _boldcharwidth;        /*pixel width of char in gc*/
    long              _boldcharheight;       /*pixel height of char in gc*/
    long              _fixedcharheight;      /*pixel height of char in gc*/
    long              _fixedcharwidth;       /*pixel height of char in gc*/
    ColorInfo         *_col;                 /*data color context*/
    ColorInfo         *_col_two;             /*secondary data color context*/
    struct Cntrl      _Cl;                   /*data read control*/
    unsigned char     _MapColors[256];       /*color map index for the image*/
    char              _graph_font[200];      /*graphic font to use*/
    Window            _graph_window;         /*window for graph*/
    long              _frame_buffer;         /*is device frame buffered*/
    Widget            _graphic;              /*widget id for graph*/
    long              _histogram[256];       /*amplitude history*/
    class XYdisp      *_xydisp;              /*automatic xy output */
    float             _median_scale;         /*scaler for median data*/
    Widget            _statusw;              /*show status - if defined */
    float             _vel_min;              /*minimum image velocity*/
    float             _vel_max;              /*maximum image velocity*/
    float             *_vary;                /*velocity array*/
    float             _velfactor;            /*pixel transform*/
    float             *_xloc;                /*array of x locations*/
    float             *_yloc;                /*array of y locations*/
    long              _traces_per_group;     /*original cvst traces in group*/
    long              _height_diff;          /*overlay and parent difference*/
    Boolean           _can_overlay;          /*enough colors to overlay*/
    Boolean           _underlay_only;        /*no overlay*/
    Boolean           _point_to_data;        /*using someones data dont free*/
    Boolean           _point_to_headers;     /*using someones headers*/
    Boolean           _sharing_resources;    /*using another Image's resources*/
    Pixel             _overlay_pixel;        /*overlay gc pixel*/
    Pixel             _white_pixel;          /*image background*/
    Pixel             _black_pixel;          /*image foreground*/
    Pixel             _grid_pixel;           /*grid image foreground*/
    Pixel             _background_pixel;     /*active background pixel*/
    Pixel             _foreground_pixel;     /*active foreground pixel*/
    char              _errstr[200];          /*contains error messages*/
    PlotImage         *_chain_image;         /*another image to plot*/
    PlotImage         *_over_image;          /*image that overlays this one*/
    ImageAbortFunction _abort_function;
    void              *_abort_data;
    ImageXYOutputFunction _xy_output_function;
    ImageExternalFunction _external_function;
    void              *_external_function_data;
    static int checkPixmap(Display *dpy, XErrorEvent *err );
    AmplitudeRecovery *_amp_recovery;
    NetEnv            *_netenv;
    float             _undefined_value;
    int               _use_selector;       /*Use traces by TraceSelector */
    Boolean           _use_ndo_selector;   /*Use traces by NDoTraceSelection */
    int               _current_selector_panel;
    void              useSelector (int n);
    void              useNDoSelector (Boolean n);
    Boolean           usingSelector ();
    int setTraceSelectorTrace (int pixmap, long trace);
    long getSelectorNumTraces (int pixmap);
    void resetSelectorNumTraces ();
    int               _coordinate_header;//Used to match up underlay images
    int               _apply_smoother;//Smooths the result of a PlotIso image
    int               _crossline_header;//Cube definition
    int               _inline_header;   //Cube definition


    // the following variables are used in converting floats to chars
    unsigned long     *_cfc_colors;
    unsigned char     *_cfc_mapcolors;
    unsigned char     _cfc_contiguous_colors;
    float             _cfc_amp_min;
    float             _cfc_amp_max;
    float             _cfc_scf;
    unsigned int      _cfc_default;
    int               _cfc_type;

    // the following variables are used in amplitude corrections
    long              _ac_ti;
    long              _ac_nti;
// the following variable is used in interpolation 2D
    int               _interp_lt_2D; // permits extrapolation from 1D to 2D

    //Color percent calculated amplitudes
    float             _percent_ampmin;
    float             _percent_ampmax;

  public: 
    PlotImage (Widget     Graphic,
               char       *graph_font,
               char       *small_graph_font,
               ColorInfo  *col,
               ColorInfo  *col_two,
               Boolean    frame_buff);
    PlotImage();
    ~PlotImage();
    enum{ MOUSE_AMP ,        MOUSE_VEL,         MOUSE_AUX};
    enum{ ReadFail = -1,     ResourceFail = -2, EofFail = -3,
          UserAbort = -4,    PlotSuccess  = 1,  PlotWarning = 2}; 
    enum{ DEFS_OK,           DEFS_FAIL};
    enum{ App_CBYT = 1};
    enum{ PlotWONLY = 1,     PlotWFILL = 2,     PlotGS = 3,
          PlotCOLOR = 3,     PlotCONTOUR = 4,   PlotSEMB = 5,
          PlotGRID = 6,      PlotISO = 7,       PlotARRAY = 7, 
          PlotHEADER = 8};
    //Warning color bars added after SAAT must be greater than 32 colors.
    //Assign bars with <= 32 colors an enum less than SAAT and of course 
    //reassign SAAT and above appropriately.
    enum{ STANDARD = 1,      STANDARD2 = 2,     WHITERED = 3,
          BLUEYELLOW1 = 4,   BLUEYELLOW2 = 5,   SEMBLANCE = 6,
          SEMBLANCE2 = 7,    CONTOUR = 8,       GRAY = 9,
          ALLISTAIR1 = 10,   ALLISTAIR2 = 11,   ALLISTAIR3 = 12,
          ALLISTAIR4 = 13,   ALLISTAIR5 = 14,   ALLISTAIR6 = 15,
          ALLISTAIR7 = 16,   ALLISTAIR8 = 17,   SECTOR = 18,
          MEDRAM = 19,       BLUEWHITERED = 20, 
          SAAT = 21,         MAXNAMEDCOLORS = 21};
    enum{ OURLAVHDR = 31,    CPSLAVHDR = 24,    BORDER = 50,
          MAXHDRS = 64};
    enum{ Z_ORIGINAL,        Z_UP,              Z_DOWN };
    enum{ ImageAll = -1,     ScanLeft = 1,      ScanRight = 2};
    enum{ XYOFF,             XYAUTO,            XYNOAUTO };
    enum{ PANELNORM,         NORM,              FILENORM,
          EXTERNALNORM};
    enum{ VELHDR = 5, GRPHDR = 2, NUMREAD = 10};

    AmplitudeRecovery *getAmplitudeRecovery(){return _amp_recovery;}

    class TraceSelector *getTraceSelector ();
    class NDoTraceSelection *getNDoTraceSelector ();
    int   getCurrentPanel ();
    void  setCurrentTraceSelectorPanel (int panel_index);

    int horizontalPixelsPerInch(Display *dsp, int screen);
    int verticalPixelsPerInch(Display *dsp, int screen);
    int horizontalPixelsFromMM(Display *dsp, int scrn, float mm);
    int verticalPixelsFromMM(Display *dsp, int scrn, float mm);

    void setStatusWidget(Widget w){_statusw = w;}
    Widget getStatusWidget(){return _statusw;}

    void setMouseXoutWidget(Widget w);
    void setMouseYoutWidget(Widget w);
    void setMouseZoutWidget(Widget w);
    void setMouseReadoutType(int i);

    Boolean isDisplayed(){return _displayed;}

    Widget graphicWidget(){return _graphic;}    
    long getGraphWidth(){return _graph_width;}
    long getGraphHeight(){return _graph_height;}    

    ColorInfo *getColorStructure(){return _col;}
    Colormap getColorMap(){return _col->cmap;}
    void setColorInfoStructure(ColorInfo *c);
    void setSecondaryColorInfoStructure(ColorInfo *c);

    long getLeftBorder(){return _left_border;}
    void setLeftBorder(long l){_left_border = l;}
    long getRightBorder(){return _right_border;}
    void setRightBorder(long l){_right_border = l;}
    long getTopBorder(){return _top_border;}
    void setTopBorder(long l){_top_border = l;}
    long getBottomBorder(){return _bottom_border;}    
    void setBottomBorder(long l){_bottom_border = l;}
    
    long getOverlayLeftBorder(){return _over_image->getLeftBorder();}
    void setOverlayLeftBorder(long l){_over_image->setLeftBorder(l);}
    long getOverlayRightBorder(){return _over_image->getRightBorder();}
    void setOverlayRightBorder(long l){_over_image->setRightBorder(l);}
    long getOverlayTopBorder(){return _over_image->getTopBorder();}
    void setOverlayTopBorder(long l){_over_image->setTopBorder(l);}
    long getOverlayBottomBorder(){return _over_image->getBottomBorder();}    
    void setOverlayBottomBorder(long l){_over_image->setBottomBorder(l);}

    GC getPickingGC(){return _pick_gc;}
    GC getPrimaryGC(){return _gc1;}
    GC getSecondaryGC(){return _gc2;}

    long getLeftImageX(){return getLeftBorder() + getXdestination();}
    long getTopImageY(){return getTopBorder() + getYdestination();}
    long getOverlayLeftImageX(){return getOverlayLeftBorder() +
                                getOverlayXdestination();}
    long getOverlayTopImageY(){return getOverlayTopBorder() +
                                getOverlayYdestination();}

    Pixmap getCurrentPixmap(){return _pixmary[_cpixm];}
    Pixmap getPixmapArray(long l){return _pixmary[l];}
    void setCurrentPixmap(int i){_cpixm = i;}
    int  getCurrentPixmapIndex(){return _cpixm;}
    void setPixmap(long l, int i){_pixmary[l] = i;}
    double getTmin(){return _tmin;}
    double getTmax(){return _tmax;}
    float getX1(){return _grid_x1;}    
    float getX2(){return _grid_x2;}
    float getY1(){return _grid_y1;}    
    float getY2(){return _grid_y2;}
    float manualY1(){ return _manual_grid_y1;}
    float manualY2(){ return _manual_grid_y2;}
    float manualX1(){ return _manual_grid_x1;}
    float manualX2(){ return _manual_grid_x2;}
    void setManualY1(float y){ _manual_grid_y1 = y;}
    void setManualY2(float y){ _manual_grid_y2 = y;}
    void setManualX1(float x){ _manual_grid_x1 = x;}
    void setManualX2(float x){ _manual_grid_x2 = x;}
    void setManualTransform(Boolean b) { _manual_annotate = b;}
    Boolean getManualTransformX();
    Boolean getManualTransformY();
    double manualXvaluePerPixel(){return _manual_x_value_per_pixel;}
    double manualYvaluePerPixel(){return _manual_y_value_per_pixel;}
    void setLogarithmicXY(Boolean xset = True, Boolean yset = True)
                        { _use_logarithmic_x = xset; _use_logarithmic_y = yset;}
    Boolean useLogarithmicX(){return _use_logarithmic_x;}
    Boolean useLogarithmicY(){return _use_logarithmic_y;}
    long getNumberOfHeaders(){return _nhdrs;}
    void setNumberOfHeaders(long l){_nhdrs = l;}
    float *getHeaderArrayForUpdate(){return &_hd[0];}
    void releaseHeaderArray(){_hd = NULL;}
    void releaseFloatArray(){_float_array = NULL;}
    float *getVelocityArrayForUpdate(){return &_vary[0];}
    long getXimageHeight(){return _ximage.height;}
    long getOverlayXimageHeight(){return _over_image->getXimageHeight();}
    long getXimageWidth(){return _ximage.width;}
    long getOverlayXimageWidth(){return _over_image->getXimageWidth();}
    long getXimageDepth(){return _ximage.depth;}
    long getTraceWidth(){return _trace_delta;}
    void setTraceWidth(long l){_trace_delta = l;}
    long getXdestination(){return _dest_x;}
    long getYdestination(){return _dest_y;}
    void setXdestination(long l){_dest_x = l;}
    void setYdestination(long l){_dest_y = l;}
    long getOrigXdestination(){return _orig_dest_x;}
    long getOrigYdestination(){return _orig_dest_y;}
    long getOverlayXdestination(){return _over_image->getXdestination();}
    long getOverlayYdestination(){return _over_image->getYdestination();}    
    double getXvaluePerPixel(){return _x_value_per_pixel;}
    double getYvaluePerPixel(){return _y_value_per_pixel;}
    long getFirstTraceInImage(){return _first_trace_in_image;}
    long getFirstTraceLocation(){return _first_trace_location;}
    long getNumberDisplayedTraces();
    long getNumberDisplayedSamples(){return _displayed_samples;}
    long getOriginalDisplayedTraces(){return _original_traces;}
    long getOriginalDisplayedSamples(){return _original_samples;}
    ImageInput *getImageInput(){return _user;}
    float *getFloatArrayForUpdate(){return &_float_array[0];}
    unsigned char *getByteArrayForUpdate(){return &_byte_array[0];}     
    float getMemoryTmin();    
    float getMemoryTmax();
    int   getSamplesInMemory(){return _Cl.nsamp;}
    void  setSamplesInMemory(int n){ _Cl.nsamp = n;}
    long  getTracesInMemory(){return _Cl.ntot;}
    void  setTracesInMemory(long n){_Cl.ntot = n;}
    long  getPlottedInitialSkip(){return _Cl.iskp;}
    long  getPlottedNumberToDo(){return _Cl.ndo;}
    long  getPlottedNumberToSkip(){return _Cl.nskp;}
    long  getPlottedSdec(){return _Cl.sdec;}
    float getPlottedInchesPerSecond(){return _is;}
    float getPlottedTracesPerInch(){return _ti;}
    float getOriginalInchesPerSecond(){return _original_is;}
    float getOriginalTracesPerInch(){return _original_ti;}
    long  getOriginalSamples(){return _original_samples;}

    Boolean isFloatData();
    float getImageSampleRate();
    long  getFirstTraceInImageIndex();
    float *getDisplayedHeaderData();
    unsigned char *getDisplayedByteData();
    float *getDisplayedFloatData();
    unsigned char *getMemoryByteData();
    float *getMemoryFloatData();
    float *getMemoryHeaderData();
    int getXpixelFromX(float x);
    int getManualXpixelFromX(float x);
    float getXfromXpixel(int x);
    float getManualXfromXpixel(int x);
    int getYpixelFromY(float y);
    int getManualYpixelFromY(float y);
    float getYfromYpixel(int y);
    float getManualYfromYpixel(int y);
    int getYpixelFromTime(float t);
    float getTimeFromYpixel(int y);
    int getXpixelFromTraceNumber(int t);
    int getLogarithmicPixelFromValue(float value, float start_coord,
                                     float end_coord, long num_pixels);
    float getLogarithmicValueFromPixel(long pixel, float start_coord,
                                     float end_coord, long num_pixels);
    long getTraceNumberFromXpixel(int x);
    long getSampleIndexFromTime(float t);
    float getTimeFromSampleIndex(long i);
    float getHeaderFromTrace(long t, long h);
    long getPixelFromHeader(float header_val, int header_number);
    void setX1(float f){_grid_x1 = f;}
    void setX2(float f){_grid_x2 = f;}
    void setY1(float f){_grid_y1 = f;}
    void setY2(float f){_grid_y2 = f;}
    char *getErrorString(){return &_errstr[0];}
    long getPlottedTraces(){return _ntot;}
    long getPlottedSamples(){return _nsamp;}
    void setSamples(long n){_nsamp = n;}
    void setTracesPerPanel(long i, long traces){_user->_nplt = _ntot =
                                                _tpnl[i] = traces;}
    long getTracesPerPanel(long l){return _tpnl[l];}
    float getSampleRate(){return _user->_G.srval * _Cl.sdec;} 
    void setPointToData(Boolean b){_point_to_data = b;}
    void setPointToHeaders(Boolean b){_point_to_headers = b;}
    void setSharingResources(Boolean b){_sharing_resources = b;}

    long getZoomUp(){return _zoomup;}
    long getIsZoomed(){return _zoomed;}
    long getZoomIndex(){return _zindex;}
    float getZoomXstart(){return _zoomxary[_zindex][0];} 
    float getZoomXend(){ return _zoomxary[_zindex][1];}
    float getZoomYstart(){return _zoomyary[_zindex][0];}
    float getZoomYend(){return _zoomyary[_zindex][1];}
    double getZoomFactor(){return _zoom_factor;}
    void setZoomFactor(double d){_zoom_factor = d;}    
    long getTracesPerGroup(){return _traces_per_group;}
    void setTracesPerGroup(long l){_traces_per_group = l;}
    long getPlottedFrames(){return _frames;}
    long getPlottedSkipFrames(){return _skip_frames;}

    void setGridColor(Pixel c){_grid_pixel = c;}
    Pixel getGridColor(){return _grid_pixel;}
    Pixel getImageBackgroundPixel(){return _background_pixel;}
    Pixel getImageForegroundPixel(){return _foreground_pixel;}
    Pixel getImageWhitePixel(){return _white_pixel;}
    Pixel getImageBlackPixel(){return _black_pixel;}
    Pixel getOverlayPixel(){return _overlay_pixel;}

    float getVelocityMinimum(){return _vel_min;}
    float getVelocityMaximum(){return _vel_max;}
    float getVelocityFactor(){return _velfactor;}
    Boolean getIsFileData(){return _filedata;}
    void setIsFileData(Boolean b){_filedata = b;}
    XYdisp *getMouseReadout(){return _xydisp;}
    void    setMouseReadout(XYdisp *xydisp){_xydisp= xydisp;}
    Boolean getUnderlayOnly(){return _underlay_only;}
    PlotImage *getOverlayImage(){return _over_image;}
    Boolean getCanOverlay(){return _can_overlay;}    
    PlotImage *getChainImage(){return _chain_image;}

    void setImageAbortFunction(ImageAbortFunction func, void *data)
                               {_abort_function = func; _abort_data = data;}

    void setImageXYOutputFunction (ImageXYOutputFunction func, void *data);

    void setImageExternalFunction(ImageExternalFunction func, void *data)
                               {_external_function = func; 
                                _external_function_data = data;}
    void setUndefinedValue(float f){_undefined_value = f;}

    void imageFree();

    void imageFreeXresources( );

    long plot();

    long modifyPlot();

    long checkSize();

    void maximize();

    long getXys();

    long imageGrid();

    void setGridSize();

    long colorReplot();

    int allocateVelocityArray(int size);

    void annotatePlot(long HeaderOffset);

    void drawPlotLabel (Pixmap *pixmap);

    void reAnnotate();

    void annotateGridX(long HeaderOffset);

    void annotateGridY(long HeaderOffset);

    void drawString(     Display *dpy, Pixmap pixmap, GC gc, int x1, int y1,
                         char *string, int length, long trace_index = -1);

    void drawLine(       Display *dpy, Pixmap pixmap, GC gc, int x1, int y1,
                         int x2, int y2, long trace_index = -1);

    void drawLabel(      char *label, long frame, int x = 0, int y = 0,
                         Boolean is_horizontal = True, 
                         Boolean auto_position = True);

    void drawHorizontalLabel(      
                         char *label, long frame, int x, int y,
                         Boolean auto_position);

    void drawVerticalLabel(      
                         char *label, long frame, int x, int y,
                         Boolean auto_position);

    void initImage(      Widget  Graphic,    char *graph_font,
                         char                *small_graph_font,
                         ColorInfo           *col,
                         ColorInfo           *col_two,
                         Boolean             frame_buff);

    void blankImage(     float               xloc,
                         float               yloc);

    void definedColor(   float               *cbar,
                         long                barnumber,
                         long                *numcolors,
                         long                *tracevalues,
                         long                maxcolors);

    long storeColors(    float               *rgb, 
                         long                numcolors,
                         long                *compression, 
                         long                *intensity,
                         ColorInfo           *col,
                         float               *rgb_out);

    

    void imageColor(     long BytOffset,     long num_images);


    void variableArea(   unsigned char       trace[], 
                         long                index,
                         long                trace_num_index);

    int rasterizeSemblance(
                         unsigned char      trace_in[], 
                         long               index,
                         long               trace_num_index, 
                         long               HdrOffset,
                         unsigned char      *raster_array = NULL,
                         long               rwidth = 0,
                         long               rheight = 0, 
                         unsigned char      *color_array = NULL,
                         unsigned char      *color_map_array = NULL,
                         Boolean            hardcopy = False);

    int rasterizeByBytes(
                         float              *trace_in, 
                         long               index,
                         long               trace_num_index, 
                         unsigned char      *raster_array = NULL,
                         long               rwidth = 0,
                         long               rheight = 0, 
                         unsigned char      *color_array = NULL,
                         unsigned char      *color_map_array = NULL,
                         Boolean            hardcopy = False);

    int rasterizeByFloats(
                         float              *trace_in, 
                         long               index,
                         long               trace_num_index, 
                         unsigned char      *raster_array = NULL,
                         long               rwidth = 0,
                         long               rheight = 0, 
                         unsigned char      *color_array = NULL,
                         unsigned char      *color_map_array = NULL,
                         Boolean            hardcopy = False);

    enum {BYTE, PIXEL, UINT, P2UI};

    void movePixel (     int type,
			 void *to,
			 int to_index,
			 void *from,
			 int from_index = 0);

    void *allocatePixels (
			 int type,
			 int num_pixels);

    unsigned long getPixel (
			 int type,
			 void *array,
			 int array_index);

    int getColorIndex (  unsigned long color,
			 int num_colors,
			 unsigned long *colors);

    int variableArray (  long frame, 
                         unsigned char *raster_array = NULL,
                         long rwidth = 0,
                         long rheight = 0, 
                         unsigned char *color_array = NULL,
                         unsigned char *color_map_array = NULL,
                         Boolean hardcopy = False);

    int contour(         long                AryOffset,
                         long                HdrOffset);
  
    int contourGridder(  long                AryOffset,
                         long                HdrOffset);


    void contourLevel(   float *di, float *dj, float *conval,
                         float *z1, float *z2, float *z3, float *z4,
                         float *dx, float *dy,
                         float *xs, float *ys,
                         float *velocity, float *conint, 
                         float *zminm, float *zmaxm, 
                         float *gpiy, long *is, long *numvels);

    void loadColors(     Widget               w,
                         long                 predef,
                         ColorInfo            *col_cust,
                         ColorInfo            *col_gs,
                         long                 *numcolors,
                         float                *rgb,
                         long                 *change_colors,
			 float                cell_width_factor=1.0);

    void partLoadColors( Widget               w,
                         long                 predef,
                         ColorInfo            *col_cust,
                         long                 *numcolors,
                         float                *rgb,
                         long                 *change_colors,
			 float                cell_width_factor);

    void labelColorArray(float                *rgb,
                         float                vmin,
                         float                vmax,
                         long                 numcolors);

    void drawCbar(       Widget               w); 

    long getCpsCbar(     char                 *filename,
                         float                *rgb, 
                         long                 *numcolors,
                         long                 *tracevalues);

    void refresh(        long                 x,
                         long                 y,
                         long                 width,
                         long                 height);

    void refreshMain(    long                 x,
                         long                 y,
                         long                 width,
                         long                 height,
                         long                 dest_x,
                         long                 dest_y,
                         Boolean              border_only,
                         Widget               w);

    void refreshTraces(  long                 first_trace, 
                         long                 last_trace, 
                         float                first_time, 
                         float                last_time);

    void refreshPicks(   GC                   gc,
                         float                picks[],
                         float                missing_pick, 
                         float                zero_pick,
                         long                 first_trace, 
                         long                 last_trace,
                         long                 x,
                         long                 y, 
                         long                 width, 
                         long                 height);

    void refreshPoints(  GC                   gc,
                         float                xpoints[], 
                         float                ypoints[], 
                         long                 npoints,
                         long                 x, 
                         long                 y, 
                         long                 width, 
                         long                 height);

    void imageMovie(     long                 frame,
                         long                 x,
                         long                 y,
                         long                 width,
                         long                 height);

    void  imageMedian(   float                scale_amp, 
                         int                  numvals, 
                         long                 trace_index,
                         float                *median_scale,
                         float                *ct_factor);

    void imagePercent(   float                scale_amp, 
                         long                 LowPercent,
                         long                 HiPercent,
                         int                  numvals,
                         float                *LowAmp,
                         float                *HiAmp, 
                         long                 trace_index,
                         long                 histogram[]);

    
    void mapColors(      float                *LowAmp,
                         float                *HiAmp, 
                         long                 *calculate_colors);

    int processZoom(     long                 zoom_type,
                         long                 x1,
                         long                 y1,
                         long                 x2,
                         long                 y2,
                         long                 pan); 

    long zoom(           double               *zoom_factor_ptr);

    int zoomOriginal();

    int zoomUp(          long                 x1,
                         long                 y1,
                         long                 x2,
                         long                 y2,
                         long                 pan);

    int zoomDown();

    void setXYout(       Widget               xloc,
                         Widget               yloc,
                         Widget               aout,
                         int                  newstat );

    PlotImage *spawnImage(
                         PlotImage            *sourceimage,
                         PlotImage            *parentimage,
                         long                 width,
                         long                 height,
                         float                tmin,
                         float                tmax,
                         float                ti,
                         float                is,
                         float                grid_x1,
                         float                grid_x2);

    long updateImage(    long                 start_func,
                         long                 end_func,
                         long                 starting_sample,
                         long                 ending_sample,
                         long                 pixmap_index,
                         long                 *expose_x,
                         long                 *expose_y,
                         long                 *expose_width,
                         long                 *expose_height);

    int contourGridderUpdate(
                         long              start_column,
                         long              end_column,
                         long              starting_sample,
                         long              ending_sample,
                         long              pixmap_index,
                         long              *expose_x,
                         long              *expose_y,
                         long              *expose_width,
                         long              *expose_height);

    void parseArrayValue(int                  yin,
                         long                 top_border,
                         double               xval,   
                         double               *aval,
                         long                 nhdrs,
                         long                 samples,
                         float                *hd,
                         float                *float_array);

    long imageScan(      int                  direction,
                         int                  scan_original,
                         int                  scan_screen);

    void setDestinations(PlotImage            *over,
                         PlotImage            *under);

    long bytedefs (      float                *xloc,
                         float                *yloc,
                         long                 xloc_hdr,
                         long                 yloc_hdr,
                         long                 *num_gathers);

    void annotateGrid(   float                user_x1,
                         float                user_x2,
                         float                user_y1,
                         float                user_y2);

    long imageIO(        long                 AryOffset,
                         long                 HdrOffset,
                         long                 image_number);

    long readoutXY(      long                 xin, 
                         long                 yin, 
                         long                 *xindex, 
                         double               *xval,
                         double               *yval,
                         double               *aval);

    void imageMarks(     long                 active_func,
                         float                *xloc,
                         long                 num_marks,
                         float                *times);

    long createOverlay(  PlotImage            *over,
                         PlotImage            *parent);

    long createUnderlay( PlotImage            *under,
                         PlotImage            *over);
    
    static void showxy(  Widget                w,
                         PlotImage             *image,
                         XEvent                *event);

    long redrawImage(    long                  frame_index,
			 int                   do_refresh = TRUE);


    long recolorImage(   long                  frame_index,
			 int                   do_refresh = TRUE);


    long findTraces(     char *filename,              int primary_header = 0,
                         float primary_min = 0.0,     float primary_max = 0.0,
                         float primary_inc = 0.0,     int secondary_header = 0, 
                         float secondary_min = 0.0,   float secondary_max = 0.0,
                         float secondary_inc = 0.0,   int tertiary_header = 0,
                         float tertiary_min  = 0.0,   float tertiary_max = 0.0,
                         float tertiary_inc  = 0.0,   long nplt = 1,
                         long iskp = 0,               long ndo = 1, 
                         long nskp = 0,               long num_frames = 1, 
                         int pixmap = 0);

    long findTraces
      (char *filename,
       class NDoTraceSelection *select,
       long ntraces,
       long nplt,
       long iskp,
       long ndo,
       long nskp,
       long num_frames,
       int pixmap);

    Boolean isRegularized
      (char *filename,
       class NDoTraceSelection *select,
       long ntraces);

    Boolean matches
      (char *filename,
       class NDoTraceSelection *select,
       long ntraces);

    long findTracesByPattern(
                         char *filename,     int xheader, 
                         int yheader,        long num_traces,
                         long *num_gathers,  int *user_aborted, 
                         float *xloc =NULL,  float *yloc = NULL);

    Boolean contiguousColors (
			 int type,
			 int num_colors,
			 Pixel *colors);

    static void convertFloatToPix (
                         void *pi_obj,
		         void *value_out,
			 float value_in);

    static void convertFloatToChar (
                         void *pi_obj,
		         void *value_out,
			 float value_in);

    static void convertFloatToInt (
                         void *pi_obj,
		         void *value_out,
			 float value_in);

    static void convertFloatToPixel (
                         void *pi_obj,
		         void *value_out,
			 float value_in);

    static void amplitudeCorrection (
                         void *pi_obj,
			 float *value_out,
			 float value_in,
			 int which_index);

    int colorIndexOfFloat(
                         float value, 
                         Boolean initialize);

    class PixmapSet *pixmapSet () { return _pixmap_set; }

  /* long colorReplotFrame (long frame); */

    void colorInfoChanged (ColorInfo *col);

    void colorInfoInit (ColorInfo *col, ColorInfo *col_two);
    void memcpyThis (PlotImage *to);

    void testPlot ();  

};


#endif
