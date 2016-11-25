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
#ifndef POPCTL_H
#define POPCTL_H

#include "va_const.h"
#include "va.h"

class PlottingPop;
class ZoomDisp;
class ZoomOp;
class CntlPop;
class SLDefPop;
class HeaderShiftPop;
class OutputNmcPop;

#define PopCtlAryLen 15

class PopCtl {

  protected:
      PlottingPop *_sempop;
      PlottingPop *_isopop;
      PlottingPop *_moviepop;
      PlottingPop *_annopop;
      PlottingPop *_gvscmp_pop;
      PlottingPop *_gridpop;
      CntlPop     *_cntlpop;
      ZoomOp      *_zoomOp;
      HeaderShiftPop *_header_shift_pop;
      OutputNmcPop *_output_nmc_pop;
      ZoomDisp    *_zoompop[MAX_DRAW];
      SLDefPop    *_savedef;
      SLDefPop    *_getdef;
      SLFormPop   *_popary[PopCtlAryLen];
      Widget     _p;
      VelStruct *_vel;
      float _tmin, _tmax, _is;
      void first(char *, char**);
      void last(char *);
      void manageIt( PlottingPop *, Boolean);
      char *_work_tempstr;
      void do_watch(SLFormPop *pop);
      void make_a_pop( SLFormPop *pop, char *mess_str );
  public:
      PopCtl( VelStruct *va, Widget p);
      
      // Routines to create and/or manage the popups
      void makeSem(   Boolean plot);
      void makeIso(   Boolean plot);
      void makeMovie( Boolean plot);
      void makeAnno(  Boolean /*plot*/) {};
      void makeGvsCmp(Boolean plot);
      void makeGrid(  Boolean plot);
      void makeCntl();
      void makeSaveDefs();
      void makeGetDefs();
      void makeZoomop();  
      ZoomDisp *makeZoom(long);
      void makeHeaderShiftPop();
      void makeOutputNmcPop();
      
      // Routines to return the class pointers
      PlottingPop *getSemPtr()    { return _sempop; };
      PlottingPop *getIsoPtr()    { return _isopop; };
      PlottingPop *getMoviePtr()  { return _moviepop; };
      PlottingPop *getAnnoPtr()   { return _annopop; };
      PlottingPop *getGvsCmpPtr() { return _gvscmp_pop; };
      PlottingPop *getGridPtr()   { return _gridpop; };
      CntlPop     *getCntlPtr()   { return _cntlpop; };
      ZoomOp      *getZoomOpPtr() { return _zoomOp; };
      HeaderShiftPop *getHeaderShiftPop() { return _header_shift_pop;};
      OutputNmcPop *getOutputNmcPop() { return _output_nmc_pop; };
      ZoomDisp    *getZoom(long i){ return _zoompop[i]; }; 

      // Other routines
      void callPlotPrep();
      void callNewFiles(Boolean file_defs);
      void preNewFile();
      void setNormalCursor(Widget w =NULL);
      void setWatches(Widget w =NULL);
      void workMsg(char *s) { _work_tempstr= wprocPushMsg(  _vel->mhelp, s); }
      void workMsgRelease(){ wprocPopMsg( _vel->mhelp, _work_tempstr); }
      void savePopDefaults( char *filename =NULL);
      void reloadPopDefaults(char *filename =NULL);
      void reloadSystemDefaults();
      void useFileDefs(Boolean file_defs);
      void loadValues();
};

#endif
