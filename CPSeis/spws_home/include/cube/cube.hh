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
#ifndef CUBE_HH
#define CUBE_HH

#include <Xm/Xm.h>
#include "sl/sl_scroll_win.hh"
#include "cube.h"

class SeisPlot;
class SeisCubeSlice;
class CubeInform;
class CubeInformList;
class CubeVisitor;
class CubeSliceTransform;
class CubeRandomLine;
class SLScrollWin;
class NetEnv;


#define CubePlotSuccess   (1<<0)
#define CubeInlineFail    (1<<1)
#define CubeCrosslineFail (1<<2)
#define CubeTimeSliceFail (1<<3)
#define CubeNoPlot        (1<<4)

class Cube {

  public:
      enum { ScalePanel, Norm, ScaleCube, ScaleAllCubes, ExternalAmp};
      enum WhichPlot {InLine, CrossLine, TimeSlice, RandomLine, AllPlots};
      enum           {NoLinePlotted = -10};
      enum MovieDir  {StepForward, StepBackward, AnyChange};


  private:
      double _external_amp;      


  protected:     
      CubeInformList    *_inform_list;
      SeisPlot          *_inline_sp;
      SeisPlot          *_xline_sp;
      SeisPlot          *_ts_sp;
      SeisCubeSlice     *_inline_cs;
      SeisCubeSlice     *_xline_cs;
      SeisCubeSlice     *_ts_cs;
      SeisCubeSlice     *_rl_cs;
      CubeRandomLine    *_cube_random_line;
      char              *_primary_file;
      char              *_secondary_file;
      char              *_cube_desc;
      CubeSliceTransform *_inline_trans;
      CubeSliceTransform *_xline_trans;
      CubeSliceTransform *_ts_trans;
      unsigned long      _last_plot_status;
      int                _time_slice;
      int                _inline_slice;
      int                _xline_slice;
      int                _disp_time_slice;
      int                _disp_inline_slice;
      int                _disp_xline_slice;
      int                _tot_time_slices;
      int                _tot_inline_slices;
      int                _tot_xline_slices;
      float              _max_lav;
      Boolean            _good_file;
      Boolean            _replot_inline;
      Boolean            _replot_xline;
      Boolean            _replot_ts;
      Boolean            _do_ts_movie;
      Boolean            _do_inline_movie;
      Boolean            _do_xline_movie;
      Boolean            _negative_fill;
      int                _amptype;
      double             _amp;
      Boolean            _headers_set;
 
      int getSliceIndex(SeisPlot *sp, int slice, int movie_start_slice);
      void doMovie(SeisPlot                *sp,
                   char                    *label_proto, 
                   int                      frame,
                   int                      index,
                   MovieDir                 change_type,
                   WhichPlot                which_plot,
                   SLScrollWin::WhichCorner corner);

  public:
      Cube(SeisPlot  *inline_sp,
           SeisPlot  *xline_sp,
           SeisPlot  *ts_sp);
      Cube ();
      ~Cube();
      Boolean setPrimaryFilename(char *fname, Boolean use_secondary= True);
      char *primaryFilename();
      CubeTrcio *getCubeTrcio ();
      Boolean setSecondaryFilename(char *fname);
      char *cubeDescription();
      void setCubeDescription(char *str);

      SeisPlot      *inlineSP();
      SeisPlot      *crosslineSP();
      SeisPlot      *timesliceSP();
      SeisCubeSlice *inlineSCS();
      SeisCubeSlice *crosslineSCS();
      SeisCubeSlice *timesliceSCS();

      void resetCubeSlice ();

      // set Movie Parameters
      void setMovie(WhichPlot which_plot, Boolean m);
      void setFrames(WhichPlot which_plot, long f);
      void setSkipFrames(WhichPlot which_plot, long f);
      void movieToFrame(WhichPlot which_plot,
                        int       frame, 
                        MovieDir  change_type =AnyChange);


      // return Cube stats
      int  totalLines(); 
      int  totalCrossLines(); 
      int  totalTimeSlices(); 

      int  currentLine(); 
      int  currentCrossLine(); 
      int  currentTimeSlice(); 
      Boolean validCubeFile();

      float convertIndexToWC(WhichPlot axis, int index);
      int   convertWCToIndex(WhichPlot axis, float coord);
      
      int totalInLineFrames();
      int totalCrossLineFrames();
      int totalTimeSliceFrames();

      int currentInLineFrame();
      int currentCrossLineFrame();
      int currentTimeSliceFrame();

      int inLineFrameSkip();
      int crossLineFrameSkip();
      int timeSliceFrameSkip();
  
      void setTransformHeaders(int h1, int h2);
      void transformHeaders(int *h1, int *h2);

      void setTmin(float t);
      void setTmax(float t);
      void setIS(float s);
      void setLinesPerInch(float s);
      void setXlinesPerInch(float s);
      void setPlotType(int ptype);
      void setNegativeFill(Boolean fill){_negative_fill = fill;}
      void setRightToLeft(WhichPlot which_plot, long rl);
      void setInvertVerticalAxis(WhichPlot which_plot, long iv);
      //void setTI(float s);
      void setCT(float v);
      float minTmin();
      float maxTmax();

      void setMinColorAmp(float min);
      void setMaxColorAmp(float min);


      void setTimeSlice(int slice);
      void setInlineSlice(int slice);
      void setCrosslineSlice(int slice);

      void setTimeSliceToReplot();
      void setInlineSliceToReplot();
      void setCrosslineSliceToReplot();

      int firstMemoryTimeSlice();
      int lastMemoryTimeSlice();
      int timeSlice();
      int firstMemoryInlineSlice();
      int lastMemoryInlineSlice();
      int inlineSlice();
      int firstMemoryCrosslineSlice();
      int lastMemoryCrosslineSlice();
      int crosslineSlice();

      Boolean plot(WhichPlot which_plot =AllPlots); 
      Boolean plotIfNecessary();
      unsigned long lastPlotStat();
      void makeCurrentInWindow();
      void callCubeIsNolongerCurrent(Cube *newcube);
      Boolean isCurrentInWindow();
      void disableErrors();
      void addInformer(CubeInform *inform);
      void delInformer(CubeInform *inform);
      void setNormType(int amptype, double amp);
      int getNormForSP (int amptype);
      double getMaxAmp();
      double getAmplitude(){return _amp;}     //user amplitude to scale by
      int getAmplitudeType(){return _amptype;}//user specified scaling method
      CubeRandomLine *randomLine(){return _cube_random_line;}
      void setExternalAmp(double a){_external_amp = a;}
      double externalAmp(){return _external_amp;}
      
  
      void setNetEnv(NetEnv *netenv);
      NetEnv *netEnv();

     //Set headers used to create the CubeTrcio C structure
     void setCubeTrcioHeaders(int crossline_header, int inline_header);

     void ignoreAnnotationRequest (Boolean ignore);
};
#endif
