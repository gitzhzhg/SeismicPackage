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
#ifndef SEISPLOT_HH
#define SEISPLOT_HH

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <Xm/DrawingA.h>

#include "plot_image.hh"
#include "image_input.hh"
#include "xy_display.hh"
#include "sp/sp_list.hh"
#include "plot/plot_base.hh"
#include "read_data.h"
#include "wproc.h"

class SeisInform;
class SeisZoomer;
class SeisPanner;
class SeisPlotZoom;
class SeisPlotTie;
class SeisPlotUnder;
class SeisOvjd;
class SeisDrag;
class SeisDragInform;
class InformSeisZoomer;
class ZoomInformSeisZoomer;
class TieInformSeisZoomer;
class ColorBarFile;
class SeisColor;
class SlaveDisplayLinkedList;
class DoAbort;
class SPTransform;
class SeisScrWin;
class InformList;
class PaperPlot;
class HardCopyPlot;
class SeisHardCopy;
class ShareDataInform;
class SeisWinMan;
class SeisCubeSlice;
class AmplitudeRecovery;
class NetEnv;
class HorizonsPicks;
class HorizonsManager;
class HorizonsVectStyle;
class TraceSelector;
class NDoTraceSelection;
class NDoTraceFind;

static const int SPAutoColor= -1;
static const int PlotMetric= True;
static const int PlotEnglish= False;


// For color type
static const int SPColorPercent= 1;
static const int SPColorAmp=     2;
static const int SPColorMedram=  3;
static const int SPGS_Shared=     True;
static const int SPGS_NotShared=  False;
static const int PlotColorFail= -100;



typedef struct HELPCTX *HelpCtx;


class SeisPlot : public PlotBase {


  public:
      SeisPlot( const Widget p, 
                const char   *name          ="plot",
                const int ptype             =PlotImage::PlotWFILL,
                const Boolean do_scroll     =True);
      SeisPlot( SeisPlot  *other_sp, const int ptype =PlotImage::PlotWFILL);
      virtual ~SeisPlot();

      enum _col_types {COLOR, GS, CONT};
      enum _ZoomTypes {Box, Pointer};
      enum _FindLavTypes {NOLAV, LAVBYFILE, LAVBYPANEL};
      enum HardCbarType {On, Off, Square9};
      enum RedrawType   {Redraw, Clear};
      typedef enum _ZoomDir {Up, Down, Orginal, UpSeparateWin, Abort}  ZoomDir;
      typedef enum _ScanDir {Left, Right, NotChanged}                  ScanDir;
      typedef enum _MovieDir{StepForward, StepBackward, AnyChange}     MovieDir;

      virtual SeisPlot& operator=(SeisPlot& sp);

         friend class SeisZoomer;
         friend class SeisPanner;
         friend class SeisPlotZoom;
         friend class SeisPlotTie;
         friend class SeisPlotUnder;
         friend class SeisDrag;
         friend class SeisDragInform;
         friend class InformSeisZoomer;
         friend class ZoomInformSeisZoomer;
         friend class TieInformSeisZoomer;
         friend class SeisColor;
         friend class ShareDataInform;
         friend class SeisWinMan;
         friend class SeisCubeSlice;
         friend class SeisHardCopy;
         friend class SeisScrWin;
         friend class CbarInform;
         friend class SeisCbarPop;


         void cleanup();

         void forceRereadOfData(){_goto_disk = True;}

         void addInformer(SeisInform *p); // add new pick class to do picking
         void delInformer(SeisInform *p); // remove a pick class
/*todo*/ void setFrameBuff(Boolean b);    // turn frame buffer on or off
/*todo*/ void setGraphFont(char *font);   // set new font for annotation
         XFontStruct *annoFont();
         XFontStruct *annoBoldFont();
         void callStartingDragScroll();
         void callEndingDragScroll();
         void callVisibleAreaChange(int          x,
                                    int          y,
                                    unsigned int width,
                                    unsigned int height);
         SeisPlot *getChainedSP();
         void removeChainedSP (SeisPlot *sp);

         virtual void addHorizons (HorizonsManager *manager,
	   HorizonsVectStyle *style);


     // -----------  PlotBase virtual functions -----------------
         virtual Widget getWidget();
         virtual void repair(int x, int y, int width, int height) 
                           { redraw(x,y,width,height); }
         virtual void getClipArea(int *x, int *y, int *width, int *height);
         virtual void getVisibleArea(int *x, int *y, int *width, int *height);
         virtual short xPixel(float x);
         virtual short yPixel(float y);
         virtual float xWC(int x);
         virtual float yWC(int y);
         virtual float xHeader(int x, int header);
         virtual short headerPixel(int header_number, float header_value);

    // --------- Register an external function that the image library will --
    // --------- call after reading data and before making image           --
    // !!!! may need to change this to be an ordered list of functions to
    // !!!! call so that multiple processes and add and delete can be 
    // !!!! maintained. 
         void setExternalFunction(ImageExternalFunction func, void *data)
                         {_image.setImageExternalFunction(func,data);}

    // -----------  Input Params -----------
         Boolean setFilename(char *filename);
         void resetPreviousFilename();
         Boolean fileHasNeverBeenPlotted();
         char *filename()     { return _user->_filename; }

         void setOverlayTraceCnt(long n) { _user->_overlay= n;}
         long overlayTraceCnt()          { return _user->_overlay; }
         void setReadRequestArray(int *array)
                             {_user->setReadRequestArray(array);}
         int  *getReadRequestArray()
                             {return _user->getReadRequestArray();}
         void setNumberOfReadRequests(int n)
                             {_user->setNumberOfReadRequests(n);}
         int  getNumberOfReadRequests()
                             {return _user->getNumberOfReadRequests();}
         void setNPlt(long n) { isDiff(_user->_nplt,n); _user->_nplt= n;}
         void setHeader(long header_index, float v);
         long nplt()         { return _user->_nplt; }
         long totalTraces()  { return _temp_G.ntrfil; }
         long plottedNplt()  { return _image.getPlottedTraces(); }
         void setTracesPerPanel(long i, long traces){
                                     _image.setTracesPerPanel(i,traces);}
         long getTracesPerPanel(long l) {return _image.getTracesPerPanel(l);}

         void   setTmin(float t) { isDiff(_tmin,t); _tmin= t;}
         float  tmin()           { return _tmin; }
         float  minTmin()        { return _mintime; }
         double plottedTmin()    { return _image.getTmin(); }
         float  memTmin()        { return _image.getMemoryTmin();}
    
         void setTmax(float t)   { isDiff(_tmax,t); _tmax= t;}
         float  tmax()           { return _tmax; }
         float  maxTmax()        { return _maxtime; }
         double plottedTmax()    { return _image.getTmax(); }
         float  memTmax()        { return _image.getMemoryTmax();}

         void setTminTmax(float tmin, float tmax) 
             { isDiff(_tmax,tmax); _tmax= tmax;
               isDiff(_tmin,tmin); _tmin= tmin; }

         void setISkp(long n)    { isDiff(_user->_iskp,n); _user->_iskp= n;}
         long iskp()             { return _user->_iskp; }
         long plottedISkp()      { return _image.getPlottedInitialSkip();}

         void setNdo(long n)     { isDiff(_user->_ndo,n);  _user->_ndo= n;}
         long ndo()              { return _user->_ndo; }
         long plottedNdo()       { return _image.getPlottedNumberToDo();}

         void setNSkp(long n)    { isDiff(_user->_nskp,n); _user->_nskp= n;}
         long nskp()             { return _user->_nskp; }
         long plottedNskp()      { return _image.getPlottedNumberToSkip();}

         void setPlotLabel(char *label, Boolean label_only = False, 
                           Boolean draw_now = False, int x = 0, int y = 0); 
         char *plotLabel();
         int plotLabelX()               { return _user->_plotlabel_x;}
         int plotLabelY()               { return _user->_plotlabel_y;} 
         void setExtraXHardcopyAnnotation(Boolean draw_it, char *label);
         void setExtraYHardcopyAnnotation(Boolean draw_it, char *label);
         Boolean getExtraXHardcopyAnnotation(char *label);
         Boolean getExtraYHardcopyAnnotation(char *label);
         float tstrt()                  { return _temp_G.tstrt; }
         float srval()                  { return _temp_G.srval; }
         void setSrval(float n)         { _user->_G.srval = _temp_G.srval = n;}
         void setNumDataPoints(long n)  { _user->_G.nbydp = (int)n;}
         void setTdec(long n)           {isDiff(_user->_tdec,n);_user->_tdec=n;}
         long tdec()                    { return _user->_tdec; }
         long plottedTdec()             { return _image.getPlottedSdec();}

         void setNorm(long n)         { isDiff(_user->_norm,n);_user->_norm= n;}
         long  norm()                 { return _user->_norm; }
         Boolean canScaleToFile();  // true if I can scale to file

         double externalAmp();
         void setExternalAmp(double amp);

         void setGradeVert(Boolean g) {_user->_gradev= g;}
         long  gradeVert()            { return _user->_gradev; }

         void setGradeHorz(Boolean g) {_user->_gradeh= g;}
         long  gradeHorz()            { return _user->_gradeh; }

         void applySmoother(int s)    {_image._apply_smoother = s;}

         void setRP(long g)           { _user->_rp= g;}
         long  rp()                   { return _user->_rp; }

         void setRtoL(long g)         { _user->_RtoL= g;}
         long rToL()                  { return _user->_RtoL; }

         void setInvert(long g)       { _user->_invert_yaxis = g;}
         long invert()                { return _user->_invert_yaxis; }
         long yAxisHeader()           { return _user->_y_axis_header;}
         Boolean labelByHeaders()     { return _user->_label_by_headers;}

         void setDepth(long d)        { _user->_depth= d;}
         long depth()                 { return _user->_depth; }

         void setCT(float v)          { _user->_ct= v;}
         float ct()                   { return _user->_ct; }

         long negativeFill(){return _image._neg_fill;}
         void setNegativeFill(long fill){_image._neg_fill = fill;}
         double displayScale(){return _image._display_scale;}
         double minDisplayedAmplitude(){return _image._min_amplitude;}
         double maxDisplayedAmplitude(){return _image._max_amplitude;}

    //--------- Annotation Convenience Methods -------------
         Boolean setSymetricalAnnotation(float x1,float x2,float y1,float y2);
         float getSymetricalSize(Boolean get_xsize,  float x1, float x2,
                                 float y1, float y2, float *other_side_size,
                                 Boolean limit_screen_size = False);
         Boolean usingSymetricalAnnotation() {return _user->_symetrical_anno;} 
                   
    // --------- Annotation Headers -------------
         void setDrawXlines(Boolean l) {_user->_draw_x_lines = l;}
         void setDrawYlines(Boolean l) {_user->_draw_y_lines = l;}
         Boolean getDrawXlines(){ return _user->_draw_x_lines;}
         Boolean getDrawYlines(){ return _user->_draw_y_lines;}
         void setYannotationHeader(long h){_user->setYannotationHeader(h);}
         void setNumberYlabels(long n){_user->setNumberYlabels(n);}
         void setHeader1(int h) { _user->_hdrOne= h;}
         long header1()         { return _user->_hdrOne; }
         void setHeader2(int h) { _user->_hdrTwo= h;}
         long header2()         { return _user->_hdrTwo; }
         void setHeaders(int h1, int h2){_user->_hdrOne= h1;_user->_hdrTwo= h2;}
         void setSeisAnnotation(int a)  {_user->_annotate= a;}
         Boolean getSeisAnnotation() {return _user->_annotate;}
         
    // --------- Annotation of Traces -------------
         void setXLabelHeader(long l){_user->_xlabel_header = l;}
         void setFirstLbl(long t)    {_user->_firstLbl= t;}
         long firstLbl()             { return _user->_firstLbl;}
         void setLblInc(long t)      {_user->_LblInc= t;}
         long lblInc()               { return _user->_LblInc;}
         void setLabeling(long f, long inc) 
                                     {_user->_firstLbl= f; _user->_LblInc=inc;}
         long numYlabels() { return _user->_num_y_labels;}
         void annotatePlot(long header_offset)
                                     { _image.annotatePlot(header_offset);}

    // --------- Annotation of Timing Lines -------------
         void setPrimTimingLine(double l) {_user->_ptl= l;}
         double primTimingLine()          { return _user->_ptl;}
         void setSecTimingLine(double l)  {_user->_stl= l;}
         double secTimingLine()           { return _user->_stl;}
         void setTimingLines(double p, double s)
                                          {_user->_ptl= p; _user->_stl= s;}

    // -------- Pass thru iso image rasterization -----------------
        
         
         int rasterizeSemblance(
                           unsigned char      trace_in[], 
                           long               index,
                           long               trace_num_index, 
                           long               HdrOffset,
                           unsigned char      *raster_array,
                           long               rwidth,
                           long               rheight, 
                           unsigned char      *color_array,
                           unsigned char      *color_map_array,
                           Boolean            hardcopy)
              { return _image.rasterizeSemblance(
                                              trace_in, 
                                              index,
                                              trace_num_index, 
                                              HdrOffset,
                                              raster_array,
                                              rwidth,
                                              rheight, 
                                              color_array,
                                              color_map_array,
                                              hardcopy);}
         
         int rasterizeByBytes(
                           float              *trace_in, 
                           long               index,
                           long               trace_num_index, 
                           unsigned char      *raster_array,
                           long               rwidth,
                           long               rheight, 
                           unsigned char      *color_array,
                           unsigned char      *color_map_array,
                          Boolean            hardcopy)
              { return _image.rasterizeByBytes(
                                              trace_in, 
                                              index,
                                              trace_num_index, 
                                              raster_array,
                                              rwidth,
                                              rheight, 
                                              color_array,
                                              color_map_array,
                                              hardcopy);}

         int rasterizeByFloats(
                           float              *trace_in, 
                           long               index,
                           long               trace_num_index, 
                           unsigned char      *raster_array,
                           long               rwidth,
                           long               rheight, 
                           unsigned char      *color_array,
                           unsigned char      *color_map_array,
                          Boolean            hardcopy)
              { return _image.rasterizeByFloats(
                                              trace_in, 
                                              index,
                                              trace_num_index, 
                                              raster_array,
                                              rwidth,
                                              rheight, 
                                              color_array,
                                              color_map_array,
                                              hardcopy);}

         int variableArray(long frame, 
                           unsigned char *raster_array,
                           long rwidth,
                           long rheight, 
                           unsigned char *color_array,
                           unsigned char *color_map_array,
                           Boolean hardcopy) 
              { return _image.variableArray(frame, 
                           raster_array,
                           rwidth,
                           rheight, 
                           color_array,
                           color_map_array,
                           hardcopy);}
    // --------- Semblence and contour plotting -------------
         void  setMinP(float l)     {_user->_minp= l;}
         float minP()               { return _user->_minp;}
         void  setMaxP(float l)     {_user->_maxp= l;}
         float maxP()               { return _user->_maxp;}
         void  setMinMaxP(float minp, float maxp) 
                                    {_user->_minp= minp; _user->_maxp= maxp;}
         void  setContours(long c)  {_user->_contours= c;}
         long  contours()           { return _user->_contours;}
         void  setPlotWithContours(int i)    //iso array type plotting param
                                    {_user->setPlotWithContours(i);}
         void  setContoursOnly(int i)        //iso array type plotting param
                                    {_user->setContoursOnly(i);}
         void  setContourIncrement(float inc)//iso array type plotting param
                                    {_user->setContourIncrement(inc);}
         float getContourIncrement() {return _user->getContourIncrement();}
         int   plotWithContours()   { return _user->plotWithContours();}/*iso*/
         //Next is a iso array type but plotting it as contours only
         int   plottingContoursOnly(){return _user->plottingContoursOnly();}
    // ---------- Grid Coordinates --------------
         void  setGridX1(float x1);
         void  setGridX2(float x2);
         void  setGridY1(float y1);
         void  setGridY2(float y2);
         void  setGridXYS(float x1, float x2, float y1, float y2, 
                          Boolean do_manual_annotation = False);
         float gridX1()             {return _grid_x1;}
         float gridX2()             {return _grid_x2;}
         float gridY1()             {return _grid_y1;}
         float gridY2()             {return _grid_y2;}
         float plottedGridX1()      {return _image.getX1();}
         float plottedGridX2()      {return _image.getX2();}
         float plottedGridY1()      {return _image.getY1();}
         float plottedGridY2()      {return _image.getY2();}


    // ------------ Velocity info ------------------------
         void  setMinVel(float l) {_user->_vel_min= l;}
         float minVel()           { return _user->_vel_min;}
         void  setMaxVel(float l) {_user->_vel_max= l;}
         float maxVel()           { return _user->_vel_max;}
         void  setMinMaxVel(float minv, float maxv) 
                                 {_user->_vel_min= minv; _user->_vel_max= maxv;}
         float minImageVel()      { return _image.getVelocityMinimum();}
         float maxImageVel()      { return _image.getVelocityMaximum();}
         float velFactor()        { return _image.getVelocityFactor();}
         int allocateVelocityArray(int size);
         long getByteDefaults(float            *xloc,
                              float            *yloc,
                              long             xloc_hdr,
                              long             yloc_hdr,
                              long             *num_gathers);

    // ------------ Array type plotting ------------------
         void    initArrayTypePlot();
         void    cancelArrayTypeData();
         Boolean initArrayTypeData(int  frame, long num_frames,
                                   long numx,  long  numy, float *array_in,
				   int  interp_lt_2D = 1);
         void    setArrayTypeData(int  frame, long column, long num_yvals, 
                                  float xlocations, float *values, float *ys);
         long    modArrayTypeImage( long first_column, long last_column, 
                                    long first_sample, long last_sample,
                                    long pixmap_index, 
                                    SeisPlot *overlay_sp = NULL);
         int     contourGridderUpdate(long              first_column,
                                    long              last_column,
                                    long              starting_sample,
                                    long              ending_sample,
                                    long              pixmap_index,
                                    SeisPlot *overlay_sp = NULL);
         void    setUndefinedValue(float f) {_image.setUndefinedValue(f);}
         void    imageMarks(long                 active_func,
                            float                *xloc,
                            long                 num_marks,
                            float                *times,
                            SeisPlot             *overlay_sp = NULL);


    // ------------- Plot Type ------------------
         void    setPlotType(int type =PlotImage::PlotWFILL);
         void    setFileData(Boolean set = True){_image.setIsFileData(set);}
         long    getFileData()      {return _image.getIsFileData();} 
         long    plotType()         { return _user->_mode; }
         long    plottedPlotType()  { return _user->_mode; }

         void    setPlotDescription(char *s);
         char    *plotDescription(){ return (_description); }



         void setLocationOutput(Widget xloc, Widget yloc, Widget aout);
         void removeLocationOutput();
         void setLocationOutputType(int t, char *label, double value);
         int  getLocationOutputType()
              {return _image.getMouseReadout()->getMouseReadoutType();}
         void setLocationOutputXLabel(char *);
         void setLocationOutputYLabel(char *);
         void setLocationInterpolation(Boolean set, float v)
              {_image.getMouseReadout()->setMouseReadoutInterpolation(set,v);}
         void setXReadoutHeader(int h);
         int  getXReadoutHeader();
         void setAltYReadoutHeader(int h);
         int  getAltYReadoutHeader();

    // ----------- Border widths --------------
         void setLeftBorder(long b) {_image.setLeftBorder(b);_new_border= True;}
         void setTopBorder(long b)  {_image.setTopBorder(b); _new_border= True;}
         void setRightBorder(long b){_image.setRightBorder(b);_new_border=True;}
         void setBottomBorder(long b){_image.setBottomBorder(b);
                                            _new_border=True;}
         long leftBorder()           { return _image.getLeftBorder(); }
         long topBorder()            { return _image.getTopBorder(); }
         long rightBorder()          { return _image.getRightBorder(); }
         long bottomBorder()         { return _image.getBottomBorder(); }
         long overlayLeftBorder()    { return _image.getOverlayLeftBorder();}
         long overlayRightBorder()   { return _image.getOverlayRightBorder(); }
         long overlayTopBorder()     { return _image.getOverlayTopBorder(); }
         long overlayBottomBorder()  { return _image.getOverlayBottomBorder();}
         void showAnnotation(Boolean show);
         void showBorders(Boolean l, Boolean r, Boolean t, Boolean b);

    // ----------- Plot Information --------------
         long plottedWidth()    {return _image.getGraphWidth();}
         long plottedHeight()   {return _image.getGraphHeight();}
         long imageWidth()      {return _image.getXimageWidth();}
         long imageHeight()     {return _image.getXimageHeight();}
         long overImageWidth()  {return _image.getOverlayXimageWidth();}
         long overImageHeight() {return _image.getOverlayXimageHeight();}
         long overlayOnTop()    {return !_image.getUnderlayOnly();}
         void underlayOnly(Boolean u) {_image._underlay_only = u;}
         Boolean haveOverlay()  {return (_image.getOverlayImage() != NULL) 
                                 ? True : False;}
         Boolean isAnUnderlay()  {return (_type == UNDERLAY) 
                                 ? True : False;}
         Boolean canOverlay()   {return _image.getCanOverlay();}
         long firstTraceLocation() {return _image.getFirstTraceLocation();}
         virtual long isPlotDisplayed();



    // ----------- Image references --------------
         Widget imageGraphic()          { return _image.graphicWidget(); }
         long displayedTraces(int pixmap = 0);   
         long traceWidth()              { return _image.getTraceWidth();}
         void setDelta(int n)           { _image.setTraceWidth(n);}
         long imageXloc()               { return _image.getXdestination(); }
         long imageYloc()               { return _image.getYdestination();}
         long overImageXloc()        { return _image.getOverlayXdestination();}
         long overImageYloc()        { return _image.getOverlayYdestination();}
         long imageXorigin()            { return _image.getLeftImageX(); }
         long imageYorigin()            { return _image.getTopImageY(); }
         long overImageXorigin()        { return _image.getOverlayLeftImageX();}
         long overImageYorigin()        { return _image.getOverlayTopImageY();}
         long firstTrace()              { return _image.getFirstTraceInImage();}
         long originalTraces()   { return _image.getOriginalDisplayedTraces(); }
         long imageIsDisplayed()        { return _image.isDisplayed();}
         double xperPix()               { return _image.getXvaluePerPixel();}
         double yperPix()               { return _image.getYvaluePerPixel();}
         long numBytesInHeader()        { return _temp_G.nbyhd;}
         long numTempHeaderWords()      { return _temp_G.nhdwd;}
         char *fileType()               { return _user->_G.ftyp; }
         Boolean isByteData();
         Boolean isZoomed()             { return _image.getIsZoomed();}
         long zoomIndex()               { return _image.getZoomIndex();}
         float zoomXstart()             { return _image.getZoomXstart();}
         float zoomXend()               { return _image.getZoomXend();} 
         float zoomYstart()             { return _image.getZoomYstart();}
         float zoomYend()               { return _image.getZoomYend();}
         Pixmap imagePixmap(int i)      { return _image.getPixmapArray(i);} 
         long getTraceNumberFromXpixel(int x) 
                                { return _image.getTraceNumberFromXpixel(x);}
         float getZvalueFromPixelXY(int x, int y);
         const float *floatTraceData();
         const unsigned char *byteTraceData();
         const unsigned char *firstDisplayedByteTraceData();
         const float *firstDisplayedFloatTraceData();
         const float *firstDisplayedHeaderData();
         const unsigned char *firstMemoryByteTraceData();
         const float *firstMemoryFloatTraceData();
         const float *firstMemoryHeaderData();
         AmplitudeRecovery *getAmplitudeRecovery()
                        {return _image.getAmplitudeRecovery();}

         float         *floatTraceDataForUpdate();
         unsigned char *byteTraceDataForUpdate();
         unsigned char *firstDisplayedByteTraceDataForUpdate();
         float         *firstDisplayedFloatTraceDataForUpdate();
         float         *firstDisplayedHeaderDataForUpdate();
         unsigned char *firstMemoryByteTraceDataForUpdate();
         float         *firstMemoryFloatTraceDataForUpdate();
         float         *firstMemoryHeaderDataForUpdate();
         void          pointToArrayTypeData(SeisPlot *sp);

         long memoryTraces()         { return _image.getTracesInMemory();}
         void setMemoryTraces(long n){ _image.setTracesInMemory(n);}
         long samplesPerTrace()      { return _image.getSamplesInMemory();}
         void setSamplesPerTrace(int n)
                                     { _image.setSamplesInMemory(n);}
         long displayedSamples()     { return _image.getPlottedSamples();}
         float sampleRate()          { return _image.getSampleRate();}
         long getSampleIndexFromTime(float t)
                                     { return _image.getSampleIndexFromTime(t);}
         long firstSampleOfTrace();
         long firstTraceIndex();
         long ySampleNumFromPixel(int ypixel);
         void xyFromTraceAndSample(float trace, long sample, int *x, int *y);
         float*getFloatArrayForUpdate()
                                  {return _image.getFloatArrayForUpdate();}
         float *getVelocityArrayForUpdate()
                                  {return _image.getVelocityArrayForUpdate();}

// ----------- Header References --------------
         float *getHeaderArrayForUpdate()
                                  {return _image.getHeaderArrayForUpdate();}
         const float *headers();
         float *headersForUpdate();
         long numHeaders()             { return _image.getNumberOfHeaders(); }
         long activeHdr()              { return _user->_active_header; }
         void setnumHeaderWords(int n) { _image.setNumberOfHeaders(n);}
         //The following by default sets the coordinate header that is used
         //by underlay images to define the x screen axis but it can be
         //set explicitly by setCoordinateHeader. This was done to provide
         //backward compatibility for older code. A separate coordinate
         //header is now needed because Geopress uses the match header for
         //other purposes (not as a display screen coordinate)
         void setMatchHeader(int n)    { _user->_match_xheader = n;
                                         _image._coordinate_header = n;}
         int matchHeader()             { return _user->_match_xheader;}
  void setCoordinateHeader(int n);
         int  getCoordinateHeader(){ return _image._coordinate_header ;}
         void getOverUnderXYs(float *over_x1,
                              float *over_y1,
                              float *over_x2,
                              float *over_y2,
                              float *under_x1,
                              float *under_y1,
                              float *under_x2,
                              float *under_y2);

    // ----------- Movie Information --------------
         void setMovie(Boolean m=True){isDiff(_user->_movie,m);_user->_movie=m;}
         void setFrames(long f) {isDiff(_user->_frames,f); _user->_frames=f;}
         void setSkipFrames(long f) 
                       {isDiff(_user->_skip_frames,f); _user->_skip_frames=f;}
         void setTracesPerGroup(long t){_image.setTracesPerGroup(t);}
         Boolean movie()             { return ((Boolean)_user->_movie);}
         long    frames()            { return _user->_frames;}
         long    skipFrames()        { return _user->_skip_frames;}
         long    plottedFrames()     { return _image.getPlottedFrames();}
         long    plottedskipFrames() { return _image.getPlottedSkipFrames();}
         long    currentFrame()      { return _image.getCurrentPixmapIndex();}
     


    // -----------  plot sizes & units -----------
         void setIS(float s);              // for seismic data modes
         void setTI(float s);              // for seismic data modes
         void setPlotSize(float w, float h);//call setPlotWidth, setPlotHeight
         void setGridWidth(float width);   // only for grid & iso modes
         void setGridHeight(float height); // only for grid & iso modes
         float gridWidth()                  { return _plot_width;}
         float gridHeight()                 { return _plot_height;}
         float is()                         { return _y_unit_per_inch;}
         float ti()                         { return _x_unit_per_inch;}
         float plottedIS()        { return _image.getPlottedInchesPerSecond();}
         float plottedTI()        { return _image.getPlottedTracesPerInch();}
         double xvaluePerPixel()  { return _image.getXvaluePerPixel();}
         double yvaluePerPixel()  { return _image.getYvaluePerPixel();}
//         void setNY_per_inch_or_cm(float s) { setIS(s);}
//         void setNX_per_inch_or_cm(float s) { setTI(s);}
//         float cms()                        { return _user->_is;}
//         float plottedTCM()                 { return _image.is;}
//         float tcp()                        { return _user->_ti;}
//         float plottedCMS()                 { return _image.ti;}
         float userTI()                     { return _user->_ti;}
         float userIS()                     { return _user->_is;}
         float originalIS()  { return _image.getOriginalInchesPerSecond();}
         float originalTI()  { return _image.getOriginalTracesPerInch();}
         long  originalSamples(){ return _image.getOriginalSamples();}
         void  setSamples(long n){_image.setSamples(n);}
         void setUnits(const int u);
         int units()                        { return _user->_metric;}
         int maxWigglesPerInch();

    // -----------  3D file plot sizes & units -----------
         void set3dAxis(int axis);
         void set3dSlice(int index);
         CubeTrcio *getCubeTrcio ();
         //Set headers used to create the CubeTrcio C structure
         void setCubeTrcioHeaders(int crossline_header, int inline_header)
               { _user->_G.crossline_header = crossline_header;
                 _user->_G.inline_header    = inline_header;
                 _temp_G.crossline_header   = crossline_header;
                 _temp_G.inline_header      = inline_header;
                 _image._crossline_header   = crossline_header;
                 _image._inline_header      = inline_header;}

    // -----------   other -----------
         void backingStore(Boolean doit =True);
         long findTraces(char *filename,         int primary_header = 0,
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
	    NDoTraceSelection *select,
            long nplt = 1, 
            long iskp = 0,
	    long ndo = 1,  
	    long nskp = 0,
	    long num_frames = 1, 
	    int pixmap = 0);

         long reestablishTraces ();

         Boolean isRegularized
	   (NDoTraceSelection *select);

         Boolean matches
	   (NDoTraceSelection *select);

         long findTracesByPattern(char *filename,     int xheader, 
                             int yheader,        long num_traces,
                             long *num_gathers,  int *user_aborted, 
                             float *xloc =NULL,  float *yloc = NULL); 

         void setSelectorParameters(int   use              = 0,
                                    int   primary_header   = 1,
                                    float primary_min      = 0.0F,  
                                    float primary_max      = 1.0F,
                                    float primary_inc      = 0.0F,
                                    int   secondary_header = 2, 
                                    float secondary_min    = 0.0F, 
                                    float secondary_max    = 1.0F,
                                    float secondary_inc    = 0.0F,
                                    int   tertiary_header  = 2, 
                                    float tertiary_min     = 0.0F, 
                                    float tertiary_max     = 1.0F,
                                    float tertiary_inc     = 0.0F,
                                    long  traces_found     = 0,
                                    long  frames_found     = 0,
                                    long  traces_in_last_frame = 0,
                                    long  secondary_search = 0,
                                    long  tertiary_search  = 0);

         void getSelectorParameters(int   *primary_header,
                                    float *primary_min,  
                                    float *primary_max,
                                    float *primary_inc,
                                    int   *secondary_header, 
                                    float *secondary_min, 
                                    float *secondary_max,
                                    float *secondary_inc,
                                    int   *tertiary_header, 
                                    float *tertiary_min, 
                                    float *tertiary_max,
                                    float *tertiary_inc,
                                    long  *traces_found,
                                    long  *frames_found,
                                    long  *traces_in_last_frame,
                                    long  *secondary_search,
                                    long  *tertiary_search);

         void setNDoSelectorParameters
	   (int use,
	    NDoTraceSelection *select = NULL,
	    NDoTraceFind *results = NULL);

         void getSelectorParameters
	   (NDoTraceSelection **select,
	    NDoTraceFind **results);

         void resetSelectorNumTraces ();
         void updateSelector (NDoTraceSelection *select);

         int  usingSelector(){ return _image.usingSelector();}
         int  setTraceSelectorTrace(int pixmap, long trace);
         void setCurrentTraceSelectorPanel(int panel_index)
                           {_image.setCurrentTraceSelectorPanel(panel_index);}
         long getCurrentPanel(){ return _image.getCurrentPanel();}
         long  getSelectorNumTraces(int pixmap);
         TraceSelector *getTraceSelector() {return _image.getTraceSelector();}
         NDoTraceSelection *getNDoTraceSelector()
                                        {return _image.getNDoTraceSelector();}
 


// ----------- manual annotation and transform over-ride methods ------------
         void reAnnotate();                 //re-annotates the image
         float manualY1()                   { return _image.manualY1();}
         float manualY2()                   { return _image.manualY2();}
         float manualX1()                   { return _image.manualX1();}
         float manualX2()                   { return _image.manualX2();}
         void setManualY1(float y)          { _image.setManualY1(y);}
         void setManualY2(float y)          { _image.setManualY2(y);}
         void setManualX1(float x)          { _image.setManualX1(x);}
         void setManualX2(float x)          { _image.setManualX2(x);}
         void setManualTransform(Boolean b) { _image.setManualTransform(b);}
         Boolean manualTransformX()    { return _image.getManualTransformX();}
         Boolean manualTransformY()    { return _image.getManualTransformY();}
         double manualXvaluePerPixel(){return _image.manualXvaluePerPixel();}
         double manualYvaluePerPixel(){return _image.manualYvaluePerPixel();}
         void setLogarithmicXY(Boolean xset = True, Boolean yset = True)
                                      {_image.setLogarithmicXY(xset, yset);}
         Boolean useLogarithmicX(){return _image.useLogarithmicX();}
         Boolean useLogarithmicY(){return _image.useLogarithmicY();}
         int getHorizontalPixelsPerInch(Display *dsp, int screen)
            {return _image.horizontalPixelsPerInch(dsp,screen);}
         int getVerticalPixelsPerInch(Display *dsp, int screen)
            {return _image.verticalPixelsPerInch(dsp,screen);}
       
    // ------------ Image transformations
         long  getTraceFromPixel( long pixel);
         float   getTraceFromHeader(int header_number, float header_value);
         float getHeaderFromTrace(int trace_number, int header_number);

    // -----------  zoom / scan Options -----------
         void setZoomFactor(float zf)           { _zoom_factor=zf;
                                                  _image.setZoomFactor(zf);}
         void setZoomUpType(int zt)             { _zoom_type=  zt;}
         void setZoomBoxSize(int box)           { _box_size=   box;}
         long getZoomUp()                       { return _image.getZoomUp();}
         void setScanOriginalScale(Boolean b)   { _origin_scale= b;}
         void setScanAScreen(Boolean b)         { _scan_a_screen= b;}

         float zoomFactor()          { return _zoom_factor;}
         int   zoomUpType()          { return _zoom_type;}
         int   zoomBoxSize()         { return _box_size;}
         Boolean scanOriginalScale() { return _origin_scale; };
         Boolean scanAScreen()       { return _scan_a_screen; };

    // ----------- color setup -----------
         void setHiResolution(int set)    {_user->setHiResolution(set);}
         int  useHiResolution()           {return _user->useHiResolution();}
         void setCenterPercent(long set)  {_user->setCenterPercent(set);}
         long getCenterPercent()          {return _user->getCenterPercent();}
         void setPNC(int pnc)             {_user->_pnc= pnc;};
         long pnc()                       { return _user->_pnc;}
         void setPPC(int ppc)             { _user->_ppc= ppc;}
         long ppc()                       { return _user->_ppc;}
         void setMinColorAmp(float ma)    {_user->_color_ampmin= ma;}
         float minColorAmp()              { return _user->_color_ampmin;}
         void setMaxColorAmp(float ma)    {_user->_color_ampmax= ma;}
         float maxColorAmp()              { return _user->_color_ampmax;}
         void setMaxDataAmp(float ma)     {_user->_G.trmaxg = ma;}
         float maxDataAmp()               { return _user->_G.trmaxg;}
         float maxTempDataAmp()           { return _temp_G.trmaxg;}
         void setDoMedian(int med)        {_user->_do_median = med;}
         long domedian()                  {return _user->_do_median;}
         void setDoColor(int dc)          {_user->_do_color = dc;}
         long docolor()                   {return _user->_do_color;}
         void setDoPercent(int pc)        {_user->_do_percent = pc;}
         long dopercent()                 {return _user->_do_percent;}
         void setDoAmplitude(int da)      {_user->_do_amps = da;}
         long doamplitude()               {return _user->_do_amps;}         
         void setGridColor(Pixel c)       {_image.setGridColor(c);}
         Pixel getGridColor()             {return _image.getGridColor();}
         virtual Pixel getImageBackgroundPixel(){
             if(_user->_mode == PlotImage::PlotGRID ||
                _user->_mode == PlotImage::PlotHEADER ||
                _user->_mode == PlotImage::PlotISO)
                return getGridColor();
             else
                return _image.getImageBackgroundPixel();}
         virtual Pixel getImageForegroundPixel()  
                {return _image.getImageForegroundPixel();}
/*todo*/ void setColorType(int t);

         void setPreDefColor(int c);      // set one of the predefined colors
         int  getPreDefColor();
/*todo*/ void setInputColor(ColorBarFile *cf);  // user inputed colors

    // ----------- Color Allocation -----------
         void setColInfo (ColorInfoPtr col);
         ColorInfoPtr getColInfo () {return _col;}
         void setContColInfo (ColorInfoPtr col);
         void shareContourColors(ColorInfoPtr col);
         ColorInfoPtr getContourColorPtr(){return &_col_contour;}
         Boolean setLoadableColors(int n);     // set how many loadable colors
         void freeLoadableColors();            // free main colors
         Boolean setContourColors(int n);      // contour colors
         void freeContourColors();             // free contour colors
         int getLoadableColors() {return _col->cnum;}
         int getContourColors()  {return _col_contour.cnum;}
         void freeUneededColors(Boolean f) {_free_unneeded= f;};
         void setGStype(int t) {_use_shared_gs= t;};
/*todo*/ Boolean setGrayColors(int n);         // set how many gray colors
         Boolean colorCapable();           //probably posible to do color
         Boolean colorsAllocated() { return _col->colorsafe;} 
         void shareColorsWith(SeisPlot *sp);     // get colors from another 
         void shareReadOnlyColorsWith(SeisPlot *sp);// get colors from another 
         void shareDynamicColorsWith(SeisPlot *sp); // get colors from another 
         void takeOverSharingFrom(SeisPlot *sp);
         Boolean isMultiPlane() { return _multi_plane; }

    // --------- Slave Routines ----------
         void setSlaveList( SlaveDisplayLinkedList *slaves) {_slaves= slaves;}


    // --------- Help ----------
         void    setHelpCtx(HelpCtx hctx) { _hctx= hctx;}
         HelpCtx getHelpCtx()             { return _hctx;}

    // scroll bar routine
         void setVerticalScrollBarOnRight(Boolean doit = True);

    // ----------- Action Routines -----------------------
         virtual Widget W();              // the topmost widget created

         virtual int plot();              // plot the file & check setup
         int plotFromFile(){_goto_disk= True;return plot(); }
         void plotFrom(SeisPlot *sp) {*this= *sp; plot();}
         virtual void blankImage(float xloc, float yloc);
         virtual long redrawImage( long frame_index );
         void contourSemblanceData(float *di, float *dj, float *conval,
                                   float *z1, float *z2, float *z3, float *z4,
                                   float *dx, float *dy,
                                   float *xs, float *ys,
                                   float *velocity, float *conint, 
                                   float *zminm, float *zmaxm, 
                                   float *gpiy, long *is, long *numvels);
         virtual void drawLabel(char *label, long frame, int x = 0, int y = 0,
                                Boolean is_horizontal = True, 
                                Boolean auto_position = True);
         void redraw(  long  x =0,
                       long  y =0,
                       long  width =PlotImage::ImageAll,
                       long  height=PlotImage::ImageAll);
         void clear();
         void setRedrawAction(RedrawType redraw);
         void redrawToWidget(  Widget w, 
                               long  x =0,
                               long  y =0,
                               long  width =PlotImage::ImageAll,
                               long  height=PlotImage::ImageAll,
                               long  destx =0,
                               long  desty =0);
         void replotZoom();
         virtual void zoomUp();                   // zoom in place
         virtual void zoomDown();                 // zoom in place
         virtual void originalSize();             // goto orginal size
         virtual SeisPlotZoom *zoomUpSeparateWin();
         virtual int scan(ScanDir dir);           // scan in more data
         void maximizeScale(int plot_type);
/*todo*/ void movie(int dir);
         void movieToFrame(int frame, MovieDir change_type =AnyChange);
         int pstat() { return _last_pstat;} // return last plot status
/*todo*/ char *plotErrStr();              // return last plot error string

         void showWarnings(Boolean w)   {_show_warnings= w;};
         void showErrors(Boolean e)     {_show_errors= e;};
         void showProgErrors(Boolean p) {_show_prog_err= p;};
         void disableErrors()           {_normal_error_processing= False;}
         void enableErrors()            {_normal_error_processing= True;}
         char *lastError();
        
         void setMessageWidget(Widget msg);
         void setModeWidget(Widget mode, char *mode_str);


    // ----------- Transform Routines -----------------------
         void setTransform(SPTransform *transform =NULL){_transform= transform;}
         SPTransform *transform(){return _transform;}



    // ----------- Hardcopy Routines -----------------------
         virtual int writeHardCopy(SeisHardCopy *shc);
         virtual void computeHardWidthHeight(float   seismic_width,
                                             float   seismic_height,
                                             float  *tot_width,
                                             float  *tot_height,
                                             Boolean left_cbar,
                                             int     frame      = 1,
                                             Boolean panel      = False);

         virtual void computePaneledHardWidthHeight(float   seismic_width,
                                                    float   seismic_height,
                                                    float  *tot_width,
                                                    float  *tot_height,
                                                    Boolean left_cbar);


         void drawColorBarOnHardCopy(HardCbarType which_type);

         void setNetEnv(NetEnv *netenv);
         NetEnv *netEnv();

    // ----------- Window Routines -----------------------
         SeisWinMan  *getSeisWinMan();
         virtual Boolean isCurrentInWindow();
         SeisPlot    *currentSPInWindow();

    // ----------- Public method to to set the DoAbort actions -------
    void setDoAbortNewAction();
    void setDoAbortActionComplete();

    // ----------- Public method to unmanage the zoom abort button ---
    void unmanageZoomAbortButton();

    SeisColor *getSeisColor () {return _sc;}

    void colorInfoChangedImmediately (ColorInfo *col);
    void colorInfoChangedFromApply (ColorInfo *col);
    void colorInfoChangedFromLoop (ColorInfo *col);
    void setColorRequiredPlotFlag (int flag);
    void drawNewPixmap ();
    double findLav (int type);


  //=========================================================================
  //======================= PROTECTED AREA ==================================
  //=========================================================================
  protected:     

      // --------- General  -----------
      ImageInput        *_user;
      PlotImage          _image;
      struct GLBL        _temp_G; 
      InformList        *_inform_list;
      Boolean            _goto_disk;
      Boolean            _new_file;
      Cursor             _watch_cur;
      char              *_description;
      char              _previous_filename[256];
      DoAbort           *_do_abort;
      HelpCtx            _hctx;
      time_t             _file_mod_time; 
      Boolean            _sharing_x_resources;
      SeisWinMan        *_widget_manager;   // creates & manages all widgets 
      HardCbarType       _cbar_on_hardcopy;
      RedrawType         _redraw_action;
      NetEnv            *_netenv;
      Boolean           _draw_extra_x_label;
      Boolean           _draw_extra_y_label;
      HorizonsPicks     *_hpicks;

      int                     _shared_contour_colors;
      int                     _color_info_changed;
      int                     _color_required_plot;
      class ColorInfoSegment *_col_segment;

      // --------  Trace Selector -----------
      int   _primary_header;
      float _primary_min;
      float _primary_max;
      float _primary_inc;
      int   _secondary_header;
      float _secondary_min;
      float _secondary_max;
      float _secondary_inc;
      int   _tertiary_header;
      float _tertiary_min;
      float _tertiary_max;
      float _tertiary_inc;
      long  _traces_found;
      long  _frames_found;
      long  _traces_in_last_frame;
      int   _secondary_search;
      int   _tertiary_search;

      NDoTraceSelection *_select;
      NDoTraceFind *_results;

      // --------  Parameters ----------
      float             _maxtime;
      float             _mintime;
      float             _plot_width;           // only for grid & iso
      float             _plot_height;          // only for grid & iso
      float             _tmin;                 // only for seismic
      float             _tmax;                 // only for seismic
      float             _x_unit_per_inch;      // can be inch or cm
      float             _y_unit_per_inch;      // can be inch or cm
      float             _grid_x1;              // only for grid & iso
      float             _grid_x2;              // only for grid & iso
      float             _grid_y1;              // only for grid & iso
      float             _grid_y2;              // only for grid & iso
   
      // --------  3d Parameters ----------
      int               _3d_axis;
      int               _3d_slice;

      // --------- borders  && Persistent annotation -----------
      Boolean           _new_border;
      Boolean           _show_left;
      Boolean           _show_right;
      Boolean           _show_top;
      Boolean           _show_bottom;
      Boolean           _app_wants_persistent_anno;
      Boolean           _anno_override_app_request;

      // --------- Zoom & Scan -----------
      float             _zoom_factor;
      int               _zoom_type;
      int               _box_size;
      Boolean           _origin_scale;
      Boolean           _scan_a_screen;

      // --------- Errors & Status  ----------
      int        _last_pstat;
      Boolean    _may_plot;
      char       _last_errstr[400];
      char       _extra_x_label[256];
      char       _extra_y_label[256];
      char       *_mode_str;
      Widget     _mode_widget;
      Boolean    _show_warnings;
      Boolean    _show_errors;
      Boolean    _show_prog_err;
      Boolean    _normal_error_processing;
      Boolean    _updating_data;

      // --------- Colors ----------
      ColorInfoPtr     _col;
      ColorInfo        _col_gs;
      ColorInfo        _col_contour;
      Boolean          _multi_plane;
      Boolean          _auto_load_colors;
      Boolean          _alloc_shared;
      Boolean          _use_shared_gs;
      Boolean          _free_unneeded;
      long             _bar_number;
      long             _gs_colors;
      long             _color_flags;
      SeisColor       *_sc;
      SeisPlot        *_color_share_sp;
      SeisPlot        *_data_pointed_to_sp;
      ShareDataInform *_share_data_inform;
      SPList           _cshares;
      SPList           _chained_sp;

      // --------- Slave Stuff ----------
      SlaveDisplayLinkedList *_slaves;

      // --------- Transform ----------
      SPTransform *_transform;

      // --------- functions -----------


      void   init(Widget da, int ptype);
      Widget initWidgets( const   Widget  p, 
                          const   char    *name, 
                          Boolean         do_scroll);
      Boolean autoAllocColors();       // alloc colors based on plot type
      /*todo*/ void allocNumber(long numcol);       
      void initUser();              // place all initials value in user


      void isDiff(long a, long b)      {if(a!=b) _goto_disk= True;}
      void isDiff(float a, float b);
      void isDiff(double a, double b);

      static void xyUpdate(void *obj, int x, int y);

      static void exposeCallback( Widget, XtPointer,
                                  XmDrawingAreaCallbackStruct* );
      virtual void expose( Widget, XtPointer, XmDrawingAreaCallbackStruct*);


      // a lot of function pointers here
      void    preZoomFunc(void*);
      void    postZoomFunc(void*);
      Boolean readyToPlot();
      void    deliverError(char *errstr, int etype);
      char    haveColors();
      char    haveContourColors();
      void    freeColors();
      void    callAllColorChange(int cnum);
      Boolean checkPlotStatus(int pstat);   // check status after plot
      void    callNewPlot ();
      void    plottingMessages(char *message); //show a message durring plot
      void    updateEnable();
      void    showAnnotationOverride(Boolean show);
      void    showAnnotationRequested();
      void    updateUserForPlot();
      void    setSharingXResources();
      Boolean inbounds(XExposeEvent *ev, PlotImage *image);


      // -----------------------------------------------------------------
      // -----------------------------------------------------------------
      // -----------------------------------------------------------------
      // these constructors only used for derived classes
      SeisPlot();
      SeisPlot( SeisWinMan   *widget_manager,
                const int     ptype          =PlotImage::PlotWFILL,
                const int     do_multi_plane =False);
      SeisPlot( const Widget  p,
                const char    *name,
                const Boolean do_scroll,
                const Boolean do_initImage = False);
      void constructor( const Widget    p, 
                        const char     *name     ="plot",
                        const int       ptype     =PlotImage::PlotWFILL,
                        const Boolean   do_scroll =True,
                              SeisPlot *other_sp  =NULL);

      virtual void addPixmaps();
      virtual void delPixmaps();
      virtual void displayPixmap();

      /* int recolorFrame (long frame); */

    private:
      static SPList _sp_list;  // for sharing resources (like color)
      int _type;
      enum {OVERLAY, UNDERLAY};
};
#endif
