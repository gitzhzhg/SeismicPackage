#include <assert.h>
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
#include "sp/seis_plot.hh"
#include "sp/seis_winman.hh"
#include "sp/seis_cube_slice.hh"
#include "sl/sl_scroll_win.hh"
#include "cube/cube.hh"
#include "cube/cube_inform_list.hh"
#include "cube/cube_slice_transform.hh"
#include "cube/cube_random_line.hh"
#include "cube/cube_random_line_plot.hh"
//#include "cube/test_sinform.hh"
//#include "cube/cube_world_coords.hh"
#include "cprim.h"


#define TS_LABEL "TS - %4.3f"
#define IL_LABEL "IL - %4.1f"
#define XL_LABEL "XL - %4.1f"




Cube::Cube(SeisPlot  *inline_sp,
           SeisPlot  *xline_sp,
           SeisPlot  *ts_sp) :
              _inline_sp(inline_sp), _xline_sp(xline_sp),
              _ts_sp(ts_sp), _primary_file(NULL), _secondary_file(NULL),
              _inline_cs(NULL), _xline_cs(NULL), _ts_cs(NULL),
              _last_plot_status(CubeNoPlot), _cube_desc(NULL),
              _inline_slice(0), _xline_slice(0), 
              _time_slice(0),
              _disp_time_slice(NoLinePlotted), 
              _disp_inline_slice(NoLinePlotted), 
              _disp_xline_slice(NoLinePlotted),
              _tot_time_slices(0), _tot_inline_slices(0),
              _tot_xline_slices(0), _good_file(False),
              _replot_inline(True), _replot_xline(True),
              _replot_ts(True),
              _do_ts_movie(False), _do_inline_movie(False),
              _do_xline_movie(False), _headers_set(False),
              _max_lav(0.0F)
{
  long inline_inc= 20;
  _inline_trans = new CubeSliceTransform(CubeSliceTransform::InLine);
  _xline_trans  = new CubeSliceTransform(CubeSliceTransform::CrossLine);
  _ts_trans     = new CubeSliceTransform(CubeSliceTransform::TimeSlice);
  if (_inline_sp) {
      _inline_cs= new SeisCubeSlice(_inline_sp, SeisCubeSlice::InLine);
      _inline_sp->setLocationOutputXLabel("X:");
      _inline_sp->setLocationOutputYLabel("Z:");
      _inline_sp->setTransform(_inline_trans);
      _inline_sp->showBorders(True, False, True, False);
      inline_inc= _inline_sp->lblInc();
  }
  if (_xline_sp) {
      _xline_cs=  new SeisCubeSlice(_xline_sp, SeisCubeSlice::CrossLine);
      _xline_sp->setTracesPerGroup(1);
      _xline_sp->setLocationOutputXLabel("Y:");
      _xline_sp->setLocationOutputYLabel("Z:");
      _xline_sp->setTransform(_xline_trans);
      _xline_sp->showBorders(False, True, True, False);
  }
  if (_ts_sp) {
      _ts_cs=     new SeisCubeSlice(_ts_sp, SeisCubeSlice::TimeSlice);
      _ts_sp->setPlotType(PlotImage::PlotCOLOR);
      _ts_sp->setLocationOutputXLabel("X:");
      _ts_sp->setLocationOutputYLabel("Y:");
      _ts_sp->setTransform(_ts_trans);
      _ts_sp->setTimingLines( inline_inc, inline_inc);


      _cube_random_line = new CubeRandomLine(this); 
  }
  _inform_list= new CubeInformList();
  _negative_fill = False;
  //new TestSInform(this);
}

Cube::Cube ()
{
  int i = 0;
}

Cube::~Cube()
{
  _inform_list->callDestroyed(this);
  if (_inline_sp)        delete _inline_sp;
  if (_inline_cs)        delete _inline_cs;
  if (_xline_sp)         delete _xline_sp;
  if (_xline_cs)         delete _xline_cs;
  if (_ts_sp)            delete _ts_sp;
  if (_ts_cs)            delete _ts_cs;
  if (_cube_random_line) delete _cube_random_line;
  if (_primary_file)     free(_primary_file);
  if (_secondary_file)   free(_secondary_file);
  if (_cube_desc)        free(_cube_desc);
  delete _inline_trans;
  delete _xline_trans;
  delete _ts_trans;
}

void Cube::transformHeaders(int *h1, int *h2)
{
  if (_headers_set) {
      _inline_trans->getHeaders(h1,h2);
  }
  else {
      _inline_cs->getAxisHeaders(h1, h2); 
  }
}

void Cube::setTransformHeaders(int h1, int h2)
{
  _inline_trans->setHeaders(h1,h2);
  _xline_trans->setHeaders(h1,h2);
  _ts_trans->setHeaders(h1,h2);
  _headers_set= True;
}

void Cube::setCubeDescription(char *str )
{
  if (_cube_desc) free(_cube_desc);
  _cube_desc= newstr(str);
}

char *Cube::cubeDescription()
{
  return _cube_desc;
}

void Cube::resetCubeSlice ()
{
  setTimeSlice      (timeSlice());
  setInlineSlice    (inlineSlice());
  setCrosslineSlice (crosslineSlice());
  _inform_list->callNewFilename (this);
  _replot_inline= _replot_xline= _replot_ts= True;
}

Boolean Cube::setPrimaryFilename(char *fname, Boolean use_secondary)
{
  Boolean succ;

  _good_file= succ= _inline_sp->setFilename(fname);
  if (succ) succ= _xline_sp->setFilename(fname);
  if (succ && !use_secondary) succ= _ts_sp->setFilename(fname);
  if (succ) { 
      int h1, h2;
      int ts, ils, xls;
      _tot_time_slices=   _ts_cs->totalSlices();
      _tot_inline_slices= _inline_cs->totalSlices();
      _tot_xline_slices=  _xline_cs->totalSlices();
      _max_lav= _inline_sp->maxTempDataAmp();
      ts=  _tot_time_slices / 2;
      ils= _tot_inline_slices / 2;
      xls= _tot_xline_slices / 2;
      _time_slice= -1;
      _inline_slice= -1;
      _xline_slice= -1;
      setTimeSlice(ts);

// Addeded to fix bug when a cube slice is initially set
      _inline_cs->useTempGlobal ();
      setInlineSlice(ils);
      _inline_cs->useTempGlobal (False);

      _xline_cs->useTempGlobal ();
      setCrosslineSlice(xls);
      _xline_cs->useTempGlobal (False);
// Addeded to fix bug when a cube slice is initially set

      _inform_list->callNewFilename(this);
      _inline_cs->getAxisHeaders(&h1, &h2); 
      _inline_trans->setHeaders(h1, h2);
      _xline_trans->setHeaders(h1, h2);
      _ts_trans->setHeaders(h1, h2);
  }
  _replot_inline= _replot_xline= _replot_ts= True;

  return succ;
}

char *Cube::primaryFilename()
{
  return _inline_sp->filename();
}

CubeTrcio *Cube::getCubeTrcio ()
{
  return _inline_sp->getCubeTrcio ();
}

Boolean Cube::setSecondaryFilename(char *)
{
  return True;
}

void Cube::setPlotType(int ptype)
{
  if (ptype != _inline_sp->plotType()) _replot_inline= _replot_xline= True;
  if (_inline_sp)
    {
    _inline_sp->setNegativeFill(_negative_fill);
    _inline_sp->setPlotType(ptype);
    }
  if (_xline_sp)
    {
    _inline_sp->setNegativeFill(_negative_fill);
    _xline_sp->setPlotType(ptype);
    }
}

void Cube::setRightToLeft(WhichPlot which_plot, long rtol)
{
  switch(which_plot)
    {
    case InLine:
      if (rtol != _inline_sp->rToL()) _replot_inline = True;
      if (_inline_sp) _inline_sp->setRtoL(rtol);
    break;
      
    case CrossLine:
      if (rtol != _xline_sp->rToL()) _replot_xline = True;
      if (_xline_sp) _xline_sp->setRtoL(rtol);
    break;

    case TimeSlice:
      if (rtol != _ts_sp->rToL()) _replot_ts = True;
      if (_ts_sp) _ts_sp->setRtoL(rtol);
    break;

    case RandomLine:
    break;

    case AllPlots:
      if (rtol != _inline_sp->rToL()) _replot_inline = True; 
      if (rtol != _xline_sp->rToL())  _replot_xline  = True;
      if (rtol != _ts_sp->rToL())     _replot_ts     = True;
      if (_inline_sp) _inline_sp->setRtoL(rtol);
      if (_xline_sp)  _xline_sp->setRtoL(rtol);
      if (_ts_sp)     _ts_sp->setRtoL(rtol);
    break;
    }

}

void Cube::setInvertVerticalAxis(WhichPlot which_plot, long iv)
{

  switch(which_plot)
    {
    case InLine:
      if (iv != _inline_sp->invert()) _replot_inline = True;
      if (_inline_sp) _inline_sp->setInvert(iv);
    break;
      
    case CrossLine:
      if (iv != _xline_sp->invert()) _replot_xline = True;
      if (_xline_sp) _xline_sp->setInvert(iv);
    break;

    case TimeSlice:
      if (iv != _ts_sp->invert()) _replot_ts = True;
      if (_ts_sp) _ts_sp->setInvert(iv);
    break;

    case RandomLine:
    break;

    case AllPlots:
      if (iv != _inline_sp->invert()) _replot_inline = True; 
      if (iv != _xline_sp->invert())  _replot_xline  = True;
      if (iv != _ts_sp->invert())     _replot_ts     = True;
      if (_inline_sp) _inline_sp->setInvert(iv);
      if (_xline_sp)  _xline_sp->setInvert(iv);
      if (_ts_sp)     _ts_sp->setInvert(iv);
    break;
    }

}




void Cube::setTmin(float t)
{
  int slice;
  if (t != _inline_sp->tmin()) {
    _replot_inline = _replot_xline = _replot_ts= True;
  }
  if (_inline_sp) _inline_sp->setTmin(t);
  if (_xline_sp)  _xline_sp->setTmin(t);
  slice = (int)((t - _inline_sp->tmin()) / _inline_sp->srval()); 
  if (slice > _time_slice) setTimeSlice( slice );
}

void Cube::setTmax(float t)
{
  if (t != _inline_sp->tmax()) _replot_inline= _replot_xline= _replot_ts= True;
  if (_inline_sp) _inline_sp->setTmax(t);
  if (_xline_sp)  _xline_sp->setTmax(t);
}

void Cube::setMinColorAmp(float min)
{
  if (min != _inline_sp->minColorAmp()) 
                 _replot_inline= _replot_xline= _replot_ts= True;
  if (_inline_sp) _inline_sp->setMinColorAmp(min);
  if (_xline_sp)  _xline_sp->setMinColorAmp(min);
  if (_ts_sp)     _ts_sp->setMinColorAmp(min);

}

void Cube::setMaxColorAmp(float max)
{
  if (max != _inline_sp->maxColorAmp()) 
                 _replot_inline= _replot_xline= _replot_ts= True;
  if (_inline_sp) _inline_sp->setMaxColorAmp(max);
  if (_xline_sp)  _xline_sp->setMaxColorAmp(max);
  if (_ts_sp)     _ts_sp->setMaxColorAmp(max);
}

void Cube::setIS(float s)
{
  if (s != _inline_sp->is()) _replot_inline= _replot_xline= True;
  if (_inline_sp) _inline_sp->setIS(s);
  if (_xline_sp)  _xline_sp->setIS(s);
}

void Cube::setLinesPerInch(float s)
{
 if (s != _xline_sp->ti()) _replot_xline= _replot_ts= True;
 if (_xline_sp)  _xline_sp->setTI(s);
 if (_ts_sp)  {
       _ts_sp->setIS(1.0 / s);
 }
 
}

void Cube::setXlinesPerInch(float s)
{
 if (s != _inline_sp->ti()) _replot_inline= _replot_ts= True;
 if (_inline_sp) _inline_sp->setTI(s);
 if (_ts_sp)     _ts_sp->setTI(s);
}

void Cube::setCT(float v)
{
  if (v != _inline_sp->ct()) _replot_inline= _replot_xline= True;
  if (_inline_sp) _inline_sp->setCT(v);
  if (_xline_sp)  _xline_sp->setCT(v);
}


int Cube::getSliceIndex(SeisPlot *sp, int slice, int movie_start_slice) 
{
   Boolean found= False;
   int retval= -1;
   int i, j;
   int step= (int)sp->plottedskipFrames()+1;
   int last_frame= _time_slice + step * (int)sp->plottedFrames();
   for (i= movie_start_slice,j=0; ( (i<last_frame) && !found ); i+= step,j++){
                if (i == slice) found= True;
   }
   if (found) retval= j-1;
   return retval;
}


void Cube::setTimeSlice(int slice )
{
   if ( (slice == _time_slice) && !_ts_sp->movie() ) {
       //_replot_ts= False;
   } // end if
   else if ( _ts_sp->movie() ) {
       int frame= getSliceIndex(_ts_sp, slice, _time_slice);
       if (frame > -1) {
            movieToFrame( TimeSlice, frame);
            //_replot_ts= False;
       } // end if
       else {
            _replot_ts= True;
       }  // end if
   } // end if
   else {
       _replot_ts= True;
   } // end if

   if (_replot_ts) {
       char label[50];
       if (_ts_cs) _ts_cs->setSlice(slice);
       _time_slice= slice;
       sprintf(label, "Time Slice: %2d", slice);
       _ts_sp->setPlotLabel(label);
       sprintf(label, TS_LABEL, convertIndexToWC(TimeSlice, slice) );
       _ts_sp->getSeisWinMan()->setCornerAnnotation(label);
   } // end if
}

void Cube::setInlineSlice(int slice)
{
  if ( (slice == _inline_slice) && !_inline_sp->movie() ){
       //_replot_inline= False;
  } // end if
  else if ( _inline_sp->movie() ) {
       int target_slice= getSliceIndex(_inline_sp, slice, _inline_slice);
       if (target_slice > -1) {
            movieToFrame( InLine, target_slice);
            //_replot_inline= False;
       } // end if
       else {
            _replot_inline= True;
       }  // end if
  } // end if
  else {
       _replot_inline= True;
  } // end if

  if (_replot_inline) {
      char label[50];
      if (_inline_cs) _inline_cs->setSlice(slice);
      _inline_slice= slice;
      sprintf(label, "Line: %2d", slice);
      _inline_sp->setPlotLabel(label);
      sprintf(label, IL_LABEL, convertIndexToWC(InLine, slice) );
      _inline_sp->getSeisWinMan()->setCornerAnnotation(label);
  }
}
void Cube::setCrosslineSlice(int slice)
{
  if ( (slice == _xline_slice) && !_xline_sp->movie() ){
       //_replot_xline= False;
  } // end if
  else if ( _xline_sp->movie() ) {
       int target_slice= getSliceIndex(_xline_sp, slice, _xline_slice);
       if (target_slice > -1) {
            movieToFrame( CrossLine, target_slice);
            //_replot_xline= False;
       } // end if
       else {
            _replot_xline= True;
       }  // end if
  } // end if
  else {
       _replot_xline= True;
  } // end if

  if (_replot_xline) {
      char label[50];
      if (_xline_cs) _xline_cs->setSlice(slice);
      _xline_slice= slice;
      sprintf(label, "Cross Line: %2d", slice);
      _xline_sp->setPlotLabel(label);
      sprintf(label, XL_LABEL, convertIndexToWC(CrossLine, slice) );
      _xline_sp->getSeisWinMan()->setCornerAnnotation(label, SLScrollWin::NE);
  } // end if
}



int Cube::firstMemoryTimeSlice() 
{ 
  return _time_slice;
}
int Cube::lastMemoryTimeSlice()
{
  int retval= _time_slice;
  if (_ts_sp->plottedFrames() > 1) {
     retval+= (int)_ts_sp->plottedFrames() * 
                           (int)(_ts_sp->plottedskipFrames()+1) - 1;
  }
  return retval;
}
int Cube::timeSlice()      
{ 
  int retval= _time_slice;
  if (_ts_sp->plottedFrames() > 1) {
     retval+= (int)_ts_sp->currentFrame() * 
                           (int)(_ts_sp->plottedskipFrames()+1);
  }
  return retval;
}


int Cube::firstMemoryInlineSlice() 
{ 
  return _inline_slice;
}
int Cube::lastMemoryInlineSlice()
{
  int retval= _inline_slice;
  if (_inline_sp->plottedFrames() > 1) {
     retval+= (int)_inline_sp->plottedFrames() * 
                             (int)(_inline_sp->plottedskipFrames()+1) - 1;
  }
  return retval;
}
int Cube::inlineSlice()
{
  int retval= _inline_slice;
  if (_inline_sp->plottedFrames() > 1) {
     retval+= (int)_inline_sp->currentFrame() * 
                             (int)(_inline_sp->plottedskipFrames()+1);
  }
  return retval;
}


int Cube::firstMemoryCrosslineSlice() 
{ 
  return _xline_slice;
}
int Cube::lastMemoryCrosslineSlice()
{
  int retval= _xline_slice;
  if (_inline_sp->plottedFrames() > 1) {
     retval+= (int)_xline_sp->plottedFrames() * 
                             (int)(_xline_sp->plottedskipFrames()+1) - 1;
  }
  return retval;
}
int Cube::crosslineSlice() 
{ 
  int retval= _xline_slice;
  if (_xline_sp->plottedFrames() > 1) {
     retval+= (int)_xline_sp->currentFrame() * 
                             (int)(_xline_sp->plottedskipFrames()+1);
  }
  return retval;
}




Boolean Cube::plot(WhichPlot which_plot)
{
  Boolean retval= True;
  int     last_sp_stat= PlotImage::PlotSuccess;
  int disp_random_line = RandomLine;
  CubeRandomLinePlot *crlp = _cube_random_line->cubeRandomLinePlot();

  _last_plot_status= CubePlotSuccess; 

  switch (which_plot) {
     case InLine:     
                      _inline_sp->setMovie(_do_inline_movie);
                      //_disp_inline_slice= NoLinePlotted;
                      break;
     case CrossLine:
                      _xline_sp->setMovie(_do_xline_movie);
                      //_disp_xline_slice=  NoLinePlotted;
                      break;
     case TimeSlice: 
                      _ts_sp->setMovie(_do_ts_movie);
                      //_disp_time_slice=   NoLinePlotted; 
                      break;
     case RandomLine: 
                      assert(0);
                      break;
     case AllPlots:
       //_disp_inline_slice= _disp_xline_slice= _disp_time_slice= NoLinePlotted;
        _inline_sp->setMovie(_do_inline_movie);
        _xline_sp->setMovie(_do_xline_movie);
        _ts_sp->setMovie(_do_ts_movie);
        break;
  }





  if (_inline_sp && (which_plot==InLine || which_plot==AllPlots)) {
        if (_inline_sp->plot()) {
             _disp_inline_slice= _inline_slice;
             _inform_list->callNewInLinePlot(this, _disp_inline_slice);
             _replot_inline= False;
        }
        else {
             if (!_inline_sp->isPlotDisplayed()) 
                         _disp_inline_slice= NoLinePlotted;
             _last_plot_status &= ~CubePlotSuccess; 
             _last_plot_status |=  CubeInlineFail; 
             last_sp_stat= _inline_sp->pstat();
             retval= False;
        }
  }
  if (_xline_sp && (which_plot==CrossLine || which_plot==AllPlots) &&
      last_sp_stat != PlotImage::UserAbort ) {
        if (_xline_sp->plot()) {
             _disp_xline_slice= _xline_slice;
             _inform_list->callNewCrossLinePlot(this, _disp_xline_slice);
             _replot_xline= False;
        }
        else {
             if (!_xline_sp->isPlotDisplayed()) 
                         _disp_xline_slice= NoLinePlotted;
             _last_plot_status &= ~CubePlotSuccess; 
             _last_plot_status |=  CubeCrosslineFail; 
             last_sp_stat= _xline_sp->pstat();
             retval= False;
        }
  }

  if (_ts_sp && (which_plot==TimeSlice || which_plot==AllPlots) &&
      last_sp_stat != PlotImage::UserAbort ) {
        if (_ts_sp->plot()) {
             _disp_time_slice= _time_slice;
             _inform_list->callNewTimeSlicePlot(this, _disp_time_slice);
             _replot_ts= False;
        }
        else {
             if (!_ts_sp->isPlotDisplayed()) 
                         _disp_time_slice= NoLinePlotted;
             _last_plot_status &= ~CubePlotSuccess; 
             _last_plot_status |=  CubeTimeSliceFail; 
             last_sp_stat= _ts_sp->pstat();
             retval= False;
        }
  }

  //We only do the random line so it gets replotted if already plotted
  if(crlp != NULL) {
    if (crlp->SP() && crlp->SP()->isPlotDisplayed() && which_plot==AllPlots &&
      last_sp_stat != PlotImage::UserAbort ) {
        crlp->SP()->plot();
        //     _disp_time_slice= _time_slice;
        //     _inform_list->callNewTimeSlicePlot(this, _disp_time_slice);
        //     _replot_ts= False;
        //}
        //else {
        if (!crlp->SP()->isPlotDisplayed()) {
            disp_random_line = NoLinePlotted;
             //_last_plot_status &= ~CubePlotSuccess; 
             //_last_plot_status |=  CubeTimeSliceFail; 
             //last_sp_stat= _ts_sp->pstat();
        retval= False;
        }
    }
  }


  _inform_list->callPostPlot(this, _disp_inline_slice, _disp_xline_slice,
                                   _disp_time_slice);


#if 0
   CubeWorldCoords cwc(this);

   printf("line min=  %f\n", cwc.lineMin() );
   printf("line max=  %f\n", cwc.lineMax() );
   printf("xline min= %f\n", cwc.crossLineMin() );
   printf("xline max= %f\n", cwc.crossLineMax() );
   printf("slice min= %f\n", cwc.sliceMin() );
   printf("slice max= %f\n\n", cwc.sliceMax() );

   printf("current line  = %f\n", cwc.lineCurrent() );
   printf("current xline = %f\n", cwc.crossLineCurrent() );
   printf("current slice = %f\n", cwc.sliceCurrent() );

#endif

  _do_inline_movie= False;
  _do_xline_movie= False;
  _do_ts_movie= False;

  return retval;
}

Boolean Cube::plotIfNecessary()
{
  Boolean retval= True;

  if (_replot_inline && _replot_xline && _replot_ts)
         retval= plot();
  else {
     if (_replot_inline)
              if (!plot(InLine))    retval= False;
     if (_replot_xline)
              if (!plot(CrossLine)) retval= False;
     if (_replot_ts)
              if (!plot(TimeSlice)) retval= False;
  } // end else

  return retval;
}



unsigned long Cube::lastPlotStat()
{
  return _last_plot_status;
}

void Cube::makeCurrentInWindow()
{
  if (_inline_sp) {
      _inline_sp->getSeisWinMan()->setCurrentSP(_inline_sp);
  }
  if (_xline_sp) {
      _xline_sp->getSeisWinMan()->setCurrentSP(_xline_sp);
  }
  if (_ts_sp) {
      _ts_sp->getSeisWinMan()->setCurrentSP(_ts_sp);
  }
  _inform_list->callCubeIsCurrent(this);
}

void Cube::disableErrors()
{
  if (_inline_sp) _inline_sp->disableErrors();
  if (_xline_sp)  _xline_sp->disableErrors();
  if (_ts_sp)     _ts_sp->disableErrors();
}

void Cube::callCubeIsNolongerCurrent(Cube *newcube)
{
  _inform_list->callCubeIsNolongerCurrent(this,newcube);
}

Boolean Cube::isCurrentInWindow()
{
  Boolean retval= False;
  if (_inline_sp)     retval= _inline_sp->isCurrentInWindow();
  else if (_xline_sp) retval= _xline_sp->isCurrentInWindow();
  else if (_ts_sp)    retval= _ts_sp->isCurrentInWindow();
  return retval;
}

/*
 *
 ********************************************************************
 *              Adding and removing Inform classes
 ********************************************************************
 */
void Cube::addInformer(CubeInform *p)
{
  _inform_list->add(p);
}
void Cube::delInformer(CubeInform *p)
{
  _inform_list->remove(p);
}

void Cube::setMovie(WhichPlot which_plot, Boolean m)
{
  switch (which_plot) {
     case InLine:     
                      if (m != _inline_sp->movie()) _replot_inline= True;
                      _replot_inline= True;
                      _do_inline_movie= True;
                      break;

     case CrossLine:  
                      if (m != _xline_sp->movie()) _replot_xline= True;
                      _replot_xline= True;
                      _do_xline_movie= True;
                      break;

     case TimeSlice:  
                      if (m != _ts_sp->movie()) _replot_ts= True;
                      _replot_ts= True;
                      _do_ts_movie= True;
                      break;

     case AllPlots:
                      setMovie(InLine,m);
                      setMovie(CrossLine,m);
                      setMovie(TimeSlice,m);
                      break;
     default:
                      assert(0);
                      break;
  } // end switch
}


void Cube::setFrames(WhichPlot which_plot, long f)
{
  switch (which_plot) {
     case InLine:     
                      if (f != _inline_sp->frames()) _replot_inline= True;
                      _inline_sp->setFrames(f);
                      break;

     case CrossLine: 
                      if (f != _xline_sp->frames()) _replot_xline= True;
                      _xline_sp->setFrames(f);
                      break;

     case TimeSlice:  
                      if (f != _ts_sp->frames()) _replot_ts= True;
                      _ts_sp->setFrames(f);
                      break;

     case AllPlots:
                      setFrames(InLine,f);
                      setFrames(CrossLine,f);
                      setFrames(TimeSlice,f);
                      break;
     default:
                      assert(0);
                      break;
  } // end switch
}


void Cube::setSkipFrames(WhichPlot which_plot, long f)
{
  switch (which_plot) {
     case InLine:     
                      if (f != _inline_sp->skipFrames()) _replot_inline= True;
                      _inline_sp->setSkipFrames(f);
                      break;

     case CrossLine:  
                      if (f != _xline_sp->skipFrames()) _replot_xline= True;
                      _xline_sp->setSkipFrames(f);
                      break;

     case TimeSlice:  
                      if (f != _ts_sp->skipFrames()) _replot_ts= True;
                      _ts_sp->setSkipFrames(f);
                      break;

     case AllPlots:
                      setSkipFrames(InLine,f);
                      setSkipFrames(CrossLine,f);
                      setSkipFrames(TimeSlice,f);
                      break;
     default:
                      assert(0);
                      break;
  } // end switch
}

void Cube::doMovie(SeisPlot                *sp,
                   char                    *label_proto, 
                   int                      frame,
                   int                      index,
                   MovieDir                 change_type,
                   WhichPlot                which_plot,
                   SLScrollWin::WhichCorner corner)
{
  char label[30];
  sp->movieToFrame(frame, (SeisPlot::MovieDir)change_type);
  int slice= index+ frame * ((int)sp->plottedskipFrames()+1);
  float sidx= convertIndexToWC(which_plot, slice );
  sprintf(label, label_proto, sidx);
  sp->getSeisWinMan()->setCornerAnnotation(label,corner);
  _inform_list->callCubeMovie(this, which_plot, change_type, slice);;
}


void Cube::movieToFrame(WhichPlot which_plot,
                        int       frame, 
                        MovieDir  change_type)
{
  switch (which_plot) {
    case InLine:     
                     doMovie(_inline_sp, IL_LABEL, frame, _inline_slice,
                             change_type, InLine, SLScrollWin::NW); 
                     break;

    case CrossLine:  
                     doMovie(_xline_sp, XL_LABEL, frame, _xline_slice,
                             change_type, CrossLine, SLScrollWin::NE); 
                     break;

    case TimeSlice:  
                     doMovie(_ts_sp, TS_LABEL, frame, _time_slice,
                             change_type, TimeSlice, SLScrollWin::NW); 
                     break;

    default:
                     assert(0);
                     break;
  } // end switch
}


double Cube::getMaxAmp()
{
  return _max_lav;
}

void Cube::setNormType(int amptype, double amp)
{
  SeisPlot *sp;
  _amptype = amptype;
  _amp = amp;

    switch (amptype) {
        case ScalePanel : 
                if (_inline_sp) _inline_sp->setNorm(PlotImage::PANELNORM);
                if (_xline_sp)  _xline_sp->setNorm(PlotImage::PANELNORM);
                if (_ts_sp)     _ts_sp->setNorm(PlotImage::PANELNORM);
                if (_cube_random_line) {
                  if (_cube_random_line->cubeRandomLinePlot()) {
                    sp = _cube_random_line->cubeRandomLinePlot()->SP ();
                    if (sp) sp->setNorm (PlotImage::PANELNORM);
                  }
                }
                break; 
        case Norm : 
                if (_inline_sp) _inline_sp->setNorm(PlotImage::NORM);
                if (_xline_sp)  _xline_sp->setNorm(PlotImage::NORM);
                if (_ts_sp)     _ts_sp->setNorm(PlotImage::NORM);
                if (_cube_random_line) {
                  if (_cube_random_line->cubeRandomLinePlot()) {
                    sp = _cube_random_line->cubeRandomLinePlot()->SP ();
                    if (sp) sp->setNorm (PlotImage::NORM);
                  }
                }
                break;
        case ScaleCube :  
                if (_inline_sp) _inline_sp->setNorm(PlotImage::FILENORM);
                if (_xline_sp)  _xline_sp->setNorm(PlotImage::FILENORM);
                if (_ts_sp)     _ts_sp->setNorm(PlotImage::FILENORM);
                if (_cube_random_line) {
                  if (_cube_random_line->cubeRandomLinePlot()) {
                    sp = _cube_random_line->cubeRandomLinePlot()->SP ();
                    if (sp) sp->setNorm (PlotImage::FILENORM);
                  }
                }
                break; 
        case ScaleAllCubes :
        case ExternalAmp :
                if (_inline_sp) {
                      _inline_sp->setExternalAmp(amp);
                      _inline_sp->setNorm(PlotImage::EXTERNALNORM);
                }
                if (_xline_sp)  {
                      _xline_sp->setExternalAmp(amp);
                      _xline_sp->setNorm(PlotImage::EXTERNALNORM);
                }
                if (_ts_sp)     {
                      _ts_sp->setExternalAmp(amp);
                      _ts_sp->setNorm(PlotImage::EXTERNALNORM);
                }
                if (_cube_random_line) {
                  if (_cube_random_line->cubeRandomLinePlot()) {
                    sp = _cube_random_line->cubeRandomLinePlot()->SP ();
                    if (sp) {
                      sp->setExternalAmp (amp);
                      sp->setNorm (PlotImage::EXTERNALNORM);
                    }
                  }
                }
                break;
        default :
                            assert(0);
                            break;
    } // end switch
}

int Cube::getNormForSP (int amptype)
{
    int retval;
    switch (amptype) {
        case ScalePanel : 
          retval = PlotImage::PANELNORM;
          break;
        case Norm : 
          retval = PlotImage::NORM;
          break;
        case ScaleCube :  
          retval = PlotImage::FILENORM;
          break;
        case ScaleAllCubes :
        case ExternalAmp :
          retval = PlotImage::EXTERNALNORM;
          break;
        default :
          assert (0);
          break;
    }
    return retval;
}


float Cube::convertIndexToWC(WhichPlot axis, int index)
{
   float wc;
   assert(axis != AllPlots);
   switch (axis) {
       case InLine:
                   wc= _inline_cs->convertIndexToWC(index);
                   break;
       case CrossLine:
                   wc= _xline_cs->convertIndexToWC(index);
                   break;
       case TimeSlice:
         wc= index * _inline_sp->srval() + _inline_sp->tstrt();
                   break;

       case RandomLine:
       break;

       case AllPlots:
                   assert(0);
                   break;

   };
   return wc;
}

int Cube::convertWCToIndex(WhichPlot axis, float coord)
{
   int slice;
   assert(axis != AllPlots);
   switch (axis) {
       case InLine:
                   slice= _inline_cs->convertWCToIndex(coord);
                   break;
       case CrossLine:
                   slice= _xline_cs->convertWCToIndex(coord);
                   break;
       case TimeSlice:
                   slice= _ts_cs->convertWCToIndex(coord);
                   break;

       case RandomLine:
       break;

       case AllPlots:
                   assert(0);
                   break;

   };
   return slice;
}


Boolean Cube::validCubeFile()
{
  return _good_file;
}

void Cube::setNetEnv(NetEnv *netenv)
{
  if (_ts_sp)     _ts_sp->setNetEnv(netenv);
  if (_inline_sp) _inline_sp->setNetEnv(netenv);
  if (_xline_sp)  _xline_sp->setNetEnv(netenv);
}

NetEnv *Cube::netEnv()
{
  return _inline_sp->netEnv();
}

float Cube::minTmin() { return _inline_sp->minTmin(); }
float Cube::maxTmax() { return _inline_sp->maxTmax(); }


void Cube::setTimeSliceToReplot()      {_replot_ts=     True;}
void Cube::setInlineSliceToReplot()    {_replot_inline= True;}
void Cube::setCrosslineSliceToReplot() {_replot_xline=  True;}

SeisPlot *Cube::inlineSP()    {return _inline_sp;}
SeisPlot *Cube::crosslineSP() {return _xline_sp;}
SeisPlot *Cube::timesliceSP() {return _ts_sp;}
SeisCubeSlice *Cube::inlineSCS()    { return _inline_cs;}
SeisCubeSlice *Cube::crosslineSCS() { return _xline_cs;}
SeisCubeSlice *Cube::timesliceSCS() { return _ts_cs;}

int Cube::totalLines()      { return _tot_inline_slices; }
int Cube::totalCrossLines() { return _tot_xline_slices;  }
int Cube::totalTimeSlices() { return _tot_time_slices;   }

int  Cube::currentLine()      {return _disp_inline_slice; }
int  Cube::currentCrossLine() {return _disp_xline_slice; }
int  Cube::currentTimeSlice() {return _disp_time_slice; }

int Cube::totalInLineFrames()     { return _inline_sp->plottedFrames();}
int Cube::totalCrossLineFrames()  { return _xline_sp->plottedFrames(); }
int Cube::totalTimeSliceFrames()  { return _ts_sp->plottedFrames();    }

int Cube::currentInLineFrame()    { return _inline_sp->currentFrame();}
int Cube::currentCrossLineFrame() { return _xline_sp->currentFrame(); }
int Cube::currentTimeSliceFrame() { return _ts_sp->currentFrame();    }

int Cube::inLineFrameSkip()       { return _inline_sp->plottedskipFrames(); }
int Cube::crossLineFrameSkip()    { return _xline_sp->plottedskipFrames();  }
int Cube::timeSliceFrameSkip()    { return _ts_sp->plottedskipFrames();     }


void Cube::setCubeTrcioHeaders(int crossline_header, int inline_header)
{
  assert(_ts_sp && _inline_sp && _xline_sp);
  _ts_sp->setCubeTrcioHeaders(crossline_header, inline_header);
  _inline_sp->setCubeTrcioHeaders(crossline_header, inline_header);
  _xline_sp->setCubeTrcioHeaders(crossline_header, inline_header);
}

void Cube::ignoreAnnotationRequest (Boolean ignore)
{
  SeisWinMan *widget_manager;
  if (_ts_sp) {
    widget_manager = _ts_sp->getSeisWinMan ();
    if (widget_manager) {
      widget_manager->ignoreAnnotationRequest (True);
    }
  }
  if (_inline_sp) {
    widget_manager = _inline_sp->getSeisWinMan ();
    if (widget_manager) {
      widget_manager->ignoreAnnotationRequest (True);
    }
  }
  if (_xline_sp) {
    widget_manager = _xline_sp->getSeisWinMan ();
    if (widget_manager) {
      widget_manager->ignoreAnnotationRequest (True);
    }
  }
}
