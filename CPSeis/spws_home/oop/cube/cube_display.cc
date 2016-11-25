
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
#include "sp/seis_cbar_pop.hh"
#include "cube/cube.hh"
#include "cube/cube_list.hh"
#include "cube/cube_display.hh"
#include "cube/cube_master.hh"
#include "cube/cube_master_iterator.hh"
#include "cube/cube_movie_control.hh"
#include "cube/cube_visitor.hh"
#include "cube/cube_wire_frame.hh"
#include "cube/cube_movie_pop.hh"
#include "cube/cube_random_line_pop.hh"
#include <Xm/Xm.h>
#include "sl/view_win.hh"
#include "sl/sl_push_box.hh"
#include "sp/seis_color_pop.hh"
#include "sp/seis_loc_out.hh"
#include "sp/seis_scrwin.hh"
#include "sp/seis_winman.hh"
#include "sp/seis_zoomop_pop.hh"
#include "cube/cube_overlays.hh"
#include "hardcopy/hardcopy_seis_pop.hh"
#include "sl/slp_file_data.hh"
#include "sl/sl_error_pop.hh"
#include <string.h>
#include <assert.h>



CubeDisplay::CubeDisplay(Widget p, char *name, int colors) : 
        SLForm(p,"form"), CubeInform(),
        _lock_scrolling(False), _restore_locking(False),
        _norm_type(Cube::ScalePanel), _validation_cube(NULL),
        _bottom_push(NULL), _sync_slices(True), _colors(colors)
{
   HelpCtx hctx= NULL;
   SeisPlot *insp, *xsp, *tssp;
   _winctl= new WinCtl(W(), name);

   insp= new SeisPlot(_winctl->W(), "inline");
   xsp=  new SeisPlot(_winctl->W(), "crossline");
   tssp= new SeisPlot(_winctl->W(), "timeslice");

   insp->getCubeTrcio ();   
    xsp->getCubeTrcio ();   
   tssp->getCubeTrcio ();   

   tssp->setLoadableColors (_colors);
   _wire_sp= new SeisPlot(_winctl->W(), "wireframe");

   _cbar= new SeisCbarPop(p, "_cbar", tssp, hctx);
   SeisPlot::setWatchOK(False);

   _wire_sp->setPlotSize(5.0F,5.0F);
   insp->setVerticalScrollBarOnRight(False);
   insp->showBorders(True, False, True, False);
   xsp->showBorders(False, True, True, False);

   _cube_movie_control= new CubeMovieControl(_winctl->W(),hctx,this);

   _curr_displayed_cube= new Cube(insp, xsp, tssp);
   _first_cube = _curr_displayed_cube;
   addCube(_curr_displayed_cube);
   _cube_wf= new CubeWireFrame(this, _wire_sp);
   _cube_overlay= new CubeOverlays(this, _cube_wf);
   _cube_random_line_pop = new CubeRandomLinePop(p, "Random Line",
                                       hctx, _curr_displayed_cube->randomLine(),
                                       this);
   CubeMaster::instance()->notifyOfNewCubeCreated(_curr_displayed_cube);

   _xyout= new SeisLocOut(W()); 
   _xyout->addControl(insp);
   _xyout->addControl(xsp);
   _xyout->addControl(tssp);

   _zoom_op= new SeisZoomOpPop(p, "zoomop", hctx, insp);
   _zoom_op->addControl(xsp);
   _zoom_op->addControl(tssp);

   XtVaSetValues( _xyout->W(), XmNrightAttachment, XmATTACH_FORM,
                               XmNbottomAttachment,XmATTACH_FORM, NULL);

   XtVaSetValues( _winctl->W(), XmNleftAttachment,  XmATTACH_FORM,
                                XmNrightAttachment, XmATTACH_FORM,
                                XmNtopAttachment,   XmATTACH_FORM,
                                XmNbottomAttachment,XmATTACH_WIDGET,
                                XmNbottomWidget,    _xyout->W(), NULL);

   _bottom_push= new SLPushBox(W(), "bottom_push", hctx, NULL, 0);
   _bottom_push->addButton("control", _cube_movie_control->getMoviePop() );

   XtVaSetValues( _bottom_push->W(), XmNrightAttachment, XmATTACH_WIDGET,
                                     XmNrightWidget,     _xyout->W(),
                                     XmNbottomAttachment,XmATTACH_FORM, NULL);

   _inline    = new ViewWin(_winctl, insp->W(),     NULL); 
   _xline     = new ViewWin(_winctl, xsp->W(),      NULL); 
   _ts        = new ViewWin(_winctl, tssp->W(),     NULL); 
   _wireframe = new ViewWin(_winctl, _wire_sp->W(), NULL); 
   _winctl->setMajor(ColumnMajor);

   _ts->setOrder(0,0);
   _wireframe->setOrder(1,0);
   _inline->setOrder(0,1);
   _xline->setOrder(1,1);
   CubeMaster::instance()->add(this);
   _color_pop= CubeMaster::instance()->colorPop(this);

   _inline_hard_data = new SLpFileData ("inline_hardcopy", (long)0,
     "Inline Plot File:", "Inline Plot File", "cgm", SLpFile::_OUTPUT);
   _xline_hard_data = new SLpFileData ("xline_hardcopy", (long)0,
     "Crossline Plot File:", "Crossline Plot File", "cgm", SLpFile::_OUTPUT);
   _ts_hard_data = new SLpFileData ("ts_hardcopy", (long)0,
     "Timeslice Plot File:", "Timeslice Plot File", "cgm", SLpFile::_OUTPUT);

   _inline_hardpop= new HardCopySeisPop(p, "inline_hardcopy", hctx, insp,
     _inline_hard_data);
   _xline_hardpop=  new HardCopySeisPop(p, "xline_hardcopy",  hctx, xsp,
     _xline_hard_data);
   _ts_hardpop=     new HardCopySeisPop(p, "ts_hardcopy",     hctx, tssp,
     _ts_hard_data);
}

CubeDisplay::~CubeDisplay()
{
   void *x;
   Cube *cube;
   for(cube = top(&x); cube; cube = next(&x) )
     {
     if(cube)
      { 
      delete cube;
      cube = NULL;
      }
     }

   CubeMaster::instance()->delInformer(this);  

   delete _inline;
   delete _xline;
   delete _ts;
   delete _wireframe;
   delete _winctl;
   delete _cube_wf;
   delete _cube_overlay;
   delete _xyout;
   delete _inline_hardpop;
   delete _xline_hardpop;
   delete _ts_hardpop;
   delete _inline_hard_data;
   delete _xline_hard_data;
   delete _ts_hard_data;
   delete _bottom_push;
   delete _cube_movie_control;
   delete _wire_sp;
   delete _cbar;
   delete _cube_random_line_pop;
   delete _zoom_op;
}


Cube *CubeDisplay::newCube()
{
   SeisPlot *insp, *xsp, *tssp;
   Cube *old = _curr_displayed_cube;
   if (old) {
     insp = new SeisPlot (_curr_displayed_cube->inlineSP   ());
     xsp  = new SeisPlot (_curr_displayed_cube->crosslineSP());
     tssp = new SeisPlot (_curr_displayed_cube->timesliceSP());
   }
   else {
     insp = new SeisPlot (_winctl->W(), "inline"   );
     xsp  = new SeisPlot (_winctl->W(), "crossline");
     tssp = new SeisPlot (_winctl->W(), "timeslice");

     insp->getCubeTrcio ();   
      xsp->getCubeTrcio ();   
     tssp->getCubeTrcio ();   

     tssp->setLoadableColors (_colors);

     SeisPlot::setWatchOK(False);

     insp->setVerticalScrollBarOnRight (False);
     insp->showBorders (True,  False, True, False);
     xsp ->showBorders (False, True,  True, False);
   }

   _color_pop->addSP    (insp);
   _color_pop->addSP    (xsp );
   _color_pop->addSP    (tssp);
   _xyout->addControl   (insp);
   _xyout->addControl   (xsp );
   _xyout->addControl   (tssp);
   _zoom_op->addControl (insp);
   _zoom_op->addControl (xsp );
   _zoom_op->addControl (tssp);

   transferColorParams (insp, xsp);

   _curr_displayed_cube = new Cube (insp, xsp, tssp);
   if (old) syncSlices (old);
   _inline_hardpop->setSeisPlot(_curr_displayed_cube->inlineSP());
   _xline_hardpop->setSeisPlot(_curr_displayed_cube->crosslineSP());
   _ts_hardpop->setSeisPlot(_curr_displayed_cube->timesliceSP());
   addCube(_curr_displayed_cube);
   CubeMaster::instance()->notifyOfNewCubeCreated (_curr_displayed_cube);
   _curr_displayed_cube->makeCurrentInWindow ();
   return _curr_displayed_cube;
}

void CubeDisplay::destroyed(Cube *cube)
{
  if (cube == _validation_cube) {
      _validation_cube= NULL;
      if (count() > 0) {
           void *x;
           Boolean found= False;
           /*
            * search for a cube will a good filename defined
            */
           for(Cube *q= top(&x); (q && !found); q= next(&x) ) {
                   if (q->validCubeFile()) {
                           found= True;
                           _validation_cube= q;
                   } // end if
           } // end loop
      } // end if
      else {
	_curr_displayed_cube = NULL;
      }
  } // end if
  if (cube == _curr_displayed_cube) {
       //_curr_displayed_cube= bottom();
       //displayNextCube();
       displayCube(bottom());
  }
}

/*
 *  check to see if a new cube file matches the set of cubes we are
 *  currently using.  Call this function after a cube file has been set.
 */
Boolean CubeDisplay::cubeMatches(Cube *cube)
{
  Boolean retval= False;

  assert(cube->validCubeFile());
  if (_validation_cube) {
        if (cube->totalLines()      == _validation_cube->totalLines()      &&
            cube->totalCrossLines() == _validation_cube->totalCrossLines() &&
            cube->totalTimeSlices() == _validation_cube->totalTimeSlices())
                   retval= True; 
        else
                   retval= False;
  } // end if
  else {
     _validation_cube= cube;
     retval= True;
  } // end else

  return retval;
}

void CubeDisplay::syncSlices(Cube *oldcube)
{
  if (_sync_slices) {
       _curr_displayed_cube->setInlineSlice( oldcube->inlineSlice() );
       _curr_displayed_cube->setCrosslineSlice( oldcube->crosslineSlice() );
       _curr_displayed_cube->setTimeSlice( oldcube->timeSlice() );
  } // end if
}


Cube *CubeDisplay::displayNextCube()
{
  void *x;
  if (find(_curr_displayed_cube, &x)) {
        Cube *old= _curr_displayed_cube;
        _curr_displayed_cube= next(&x);
        if (_curr_displayed_cube == NULL) _curr_displayed_cube= top(&x);
        syncSlices(old);
        _curr_displayed_cube->makeCurrentInWindow();
        _inline_hardpop->setSeisPlot(_curr_displayed_cube->inlineSP());
        _xline_hardpop->setSeisPlot(_curr_displayed_cube->crosslineSP());
        _ts_hardpop->setSeisPlot(_curr_displayed_cube->timesliceSP());
        old->callCubeIsNolongerCurrent(_curr_displayed_cube);
        _curr_displayed_cube->plotIfNecessary();
  }
  return _curr_displayed_cube;
}

Cube *CubeDisplay::displayPrevCube()
{
  void *x;
  if (find(_curr_displayed_cube, &x)) {
        Cube *old= _curr_displayed_cube;
        _curr_displayed_cube= prev(&x);
        if (_curr_displayed_cube == NULL) _curr_displayed_cube= bottom(&x);
        syncSlices(old);
        _curr_displayed_cube->makeCurrentInWindow();
        _inline_hardpop->setSeisPlot(_curr_displayed_cube->inlineSP());
        _xline_hardpop->setSeisPlot(_curr_displayed_cube->crosslineSP());
        _ts_hardpop->setSeisPlot(_curr_displayed_cube->timesliceSP());
        old->callCubeIsNolongerCurrent(_curr_displayed_cube);
        _curr_displayed_cube->plotIfNecessary();
  }
  return _curr_displayed_cube;
}

Boolean CubeDisplay::displayCube(Cube *cube)
{
  Boolean retval= False;
  void *x;
  if (cube != _curr_displayed_cube) {
    if (find(cube, &x)) {
      Cube *old = _curr_displayed_cube;
      _curr_displayed_cube = cube;
      syncSlices (old);
      _curr_displayed_cube->makeCurrentInWindow ();
      _inline_hardpop->setSeisPlot (_curr_displayed_cube->inlineSP());
      _xline_hardpop->setSeisPlot (_curr_displayed_cube->crosslineSP());
      _ts_hardpop->setSeisPlot (_curr_displayed_cube->timesliceSP());
      old->callCubeIsNolongerCurrent (_curr_displayed_cube);
      _curr_displayed_cube->plotIfNecessary ();
      retval = True;
    } // end if
  } // end if
  else
     retval= False;
  return retval;
}


void CubeDisplay::showit()  // must be called after realize
{
   _ts->show();
   _wireframe->show();
   _inline->show();
   _xline->show();
   lockScrolling(_lock_scrolling);
}

Cube *CubeDisplay::currentDisplayedCube()
{
  return _curr_displayed_cube;
}

SeisColorPop *CubeDisplay::colorPop()
{
  return _color_pop;
}

CubeMovieControl *CubeDisplay::cubeMovieControl()
{
  return _cube_movie_control;
}

Boolean CubeDisplay::firstCube ()
{
  return _first_cube == _curr_displayed_cube;
}


void CubeDisplay::allCubesAccept(CubeVisitor *cv)
{
  void *x;
  for( Cube *cube= top(&x); (cube); cube= next(&x))
             cv->visitCube(cube);
}


void CubeDisplay::currentCubeAccept(CubeVisitor *cv)
{
  cv->visitCube(_curr_displayed_cube);
}


void CubeDisplay::plotAllCubes()
{
  void *x;
  for( Cube *cube= top(&x); (cube); cube= next(&x))
             cube->plot();
}

void CubeDisplay::lockScrolling(Boolean lock)
{
  if (!_inline->isShowing() && lock) {
      _restore_locking= True;
  } // end if
  else {
      SeisPlot *inlinesp= _curr_displayed_cube->inlineSP();
      SeisPlot *xlinesp = _curr_displayed_cube->crosslineSP();
      SeisScrWin *inline_scrwin= inlinesp->getSeisWinMan()->scrolledWindow(); 
      SeisScrWin *xline_scrwin=  xlinesp->getSeisWinMan()->scrolledWindow();
    
      if (lock) {
          if (!_lock_scrolling) xline_scrwin->slaveVertSBTo(inline_scrwin);
      }
      else {
          if (_lock_scrolling) xline_scrwin->freeVertSB();
      }
      _lock_scrolling= lock;
      _restore_locking= False;
  } // end else
}

void CubeDisplay::showOverlay(Boolean show)
{
   if (show) _cube_overlay->makeVisible();
   else      _cube_overlay->makeInvisible();
}

void CubeDisplay::plotCurrentCube()
{
  _curr_displayed_cube->plot();
}

void CubeDisplay::allCubesAcceptAndDelete(CubeVisitor *cv)
{
  allCubesAccept(cv);
  delete cv;
}

void CubeDisplay::currentCubeAcceptAndDelete(CubeVisitor *cv)
{
  currentCubeAccept(cv);
  delete cv;
}

void CubeDisplay::showTimeSlice(Boolean show)
{
  if (show) _ts->show();
  else      _ts->hide();
}

void CubeDisplay::showInline(Boolean show)
{
  if (show) {
       _inline->show();
       if (_restore_locking) lockScrolling(True);
  }
  else {
     if (_lock_scrolling) {
         lockScrolling(False);
         _restore_locking= True;
     }
     _inline->hide();
  } // end else
}

void CubeDisplay::showCrossline(Boolean show)
{
  if (show) _xline->show();
  else      _xline->hide();
}
void CubeDisplay::showWireFrame(Boolean show)
{
  if (show) _wireframe->show();
  else      _wireframe->hide();
}

void CubeDisplay::setMessageWidget(Widget w)
{
  void *x;
  for(Cube *q= top(&x); (q); q= next(&x)) {
         q->inlineSP()->setMessageWidget(w);
         q->crosslineSP()->setMessageWidget(w);
         q->timesliceSP()->setMessageWidget(w);
  }
}
void CubeDisplay::setModeWidget(Widget w)
{
  void *x;
  for(Cube *q= top(&x); (q); q= next(&x)) {
         q->inlineSP()->setModeWidget(w,    "Mode: Plot Line");
         q->crosslineSP()->setModeWidget(w, "Mode: Plot XLine");
         q->timesliceSP()->setModeWidget(w, "Mode: Plot TS");
  }
}

void CubeDisplay::setNormType(int norm_type)
{
  CubeMasterIterator iter;
  Cube *q;
  void *x;
  double amp= 0.0;
  _norm_type= norm_type;
  if (norm_type == Cube::ScaleAllCubes) { // find biggest amp in all cubes
      double tmp_amp= 0.0;
      for(q= iter.currentCube(); (q); q= iter.nextCube()) {
              tmp_amp= q->getMaxAmp();
              if (tmp_amp > amp) amp= tmp_amp;
      } // end biggest amp in all cubes loop
  for(q= top(&x); (q); q= next(&x))
           q->setNormType(norm_type, amp);
  } // end if
  else if (norm_type == Cube::ExternalAmp) {
    amp = _curr_displayed_cube->externalAmp();
    _curr_displayed_cube->setNormType(norm_type, amp);
  }
  else {
    _curr_displayed_cube->setNormType(norm_type, amp);
  }
}

int CubeDisplay::normType(){return _curr_displayed_cube->getAmplitudeType();}

HardCopySeisPop *CubeDisplay::inlineHardcopyPop()    {return _inline_hardpop;}
HardCopySeisPop *CubeDisplay::crosslineHardcopyPop() {return _xline_hardpop;}
HardCopySeisPop *CubeDisplay::timesliceHardcopyPop() {return _ts_hardpop;}

void CubeDisplay::setSyncSlices(Boolean sync) { _sync_slices= sync; }

SeisCbarPop *CubeDisplay::cbarPop() 
{
void *x;
int count = 0;


  //Currently CSV only has one color menu and color bar. If the user
  //has colored more than one cube or if they have displayed an inline
  //or crossline in color the color bar is only accurate for the last
  //display created.
  for(Cube *q= top(&x); (q); q= next(&x)) 
    {
      if(q->inlineSP()->plotType() >= PlotImage::PlotCOLOR)
        ++count;
      ++count;
    }

  if(count > 1)
    {
      SLErrorPop *errpop = new SLErrorPop(topWidget(), "WARNING !!!",
        "More than one display is in color so the color bar is not accurate\n\
         for all displays.");
    }

  return _cbar;


}

void CubeDisplay::addPushUp(char *name, SLShellContainer *pop)
{
  _bottom_push->addButton(name, pop);
}


SeisZoomOpPop *CubeDisplay::zoomOpPop()
{
 return _zoom_op;
}

void CubeDisplay::zoomUpSeparateWin(Cube::WhichPlot which)
{
  switch (which) {
       case Cube::InLine:    
                   _curr_displayed_cube->inlineSP()->zoomUpSeparateWin();
                   break;
       case Cube::CrossLine:
                   _curr_displayed_cube->crosslineSP()->zoomUpSeparateWin();
                   break;
       case Cube::TimeSlice:
                   _curr_displayed_cube->timesliceSP()->zoomUpSeparateWin();
                   break;
       case Cube::RandomLine:
                   break;
       case Cube::AllPlots:  assert(0);
                             break;
  } // end switch

}

Widget CubeDisplay::getAmpWidget()
{
   return _xyout->getAout();
}

void CubeDisplay::crosshairOff()
{
	if (_cube_overlay->getCrossHairVis())
		_cube_overlay->setCrossHairVis(0);
}

void CubeDisplay::crosshairSmall()
{
	if (_cube_overlay->getCrossHairBig())
		_cube_overlay->setCrossHairBig(0);

	if (!_cube_overlay->getCrossHairVis())
		_cube_overlay->setCrossHairVis(1);
}

void CubeDisplay::crosshairBig()
{
	if (!_cube_overlay->getCrossHairBig())
		_cube_overlay->setCrossHairBig(1);

	if (!_cube_overlay->getCrossHairVis())
		_cube_overlay->setCrossHairVis(1);
}


void CubeDisplay::transferColorParams(SeisPlot *insp, SeisPlot *xsp)
{


  switch( _color_pop->getColorTypeBox()->WhichSelected() )
    {
      case SeisColorPop::RAMTYPE:
        insp->setDoMedian(True);
        xsp->setDoMedian(True);
        insp->setDoColor(True);
        xsp->setDoColor(True);
        insp->setDoPercent(False);
        xsp->setDoPercent(False);
        insp->setDoAmplitude(False);
        xsp->setDoAmplitude(False);
        break;

      case SeisColorPop::GRAYTYPE:
        insp->setDoMedian(False);
        xsp->setDoMedian(False);
        insp->setDoColor(True);
        xsp->setDoColor(True);
        break;

      case SeisColorPop::COLORTYPE:
        insp->setDoMedian(False);
        xsp->setDoMedian(False);
        insp->setDoColor(True);
        xsp->setDoColor(True);
        break;
    }


  switch( _color_pop->getAmpTypeBox()->WhichSelected() )
    {
      case SeisColorPop::BARVALS:  
        insp->setDoPercent(False);
        xsp->setDoPercent(False);
        insp->setDoAmplitude(False);
        xsp->setDoAmplitude(False);
        break;
      case SeisColorPop::AMPVALS:
        insp->setDoPercent(False);
        xsp->setDoPercent(False);
        insp->setDoAmplitude(True);                              
        xsp->setDoAmplitude(True);    
        break;
      case SeisColorPop::PCNTVALS:
        insp->setDoPercent(True);
        xsp->setDoPercent(True);
        insp->setDoAmplitude(False); 
        xsp->setDoAmplitude(False); 
        break;
    }


  insp->setGradeVert((Boolean)_color_pop->gradingVertical());
  xsp->setGradeVert((Boolean)_color_pop->gradingVertical());
  insp->setGradeHorz((Boolean)_color_pop->gradingHorizontal());
  xsp->setGradeHorz((Boolean)_color_pop->gradingHorizontal());
  if(_color_pop->centerPercents())//Need hi resolution mode for best result
    {
      insp->setHiResolution(1);
      xsp->setHiResolution(1);
    }
  else
    {
      insp->setHiResolution((int)_color_pop->hiRes());
      xsp->setHiResolution((int)_color_pop->hiRes());
    }

  insp->setPNC( (int)(_color_pop->getPncScale()->GetScaleValue() * .10) );
  xsp->setPNC( (int)(_color_pop->getPncScale()->GetScaleValue() * .10) );
  insp->setPPC( (int)(_color_pop->getPpcScale()->GetScaleValue() * .10));
  xsp->setPPC( (int)(_color_pop->getPpcScale()->GetScaleValue() * .10));
  insp->setMinColorAmp(_color_pop->getMinAmp());
  xsp->setMinColorAmp(_color_pop->getMinAmp());
  insp->setMaxColorAmp(_color_pop->getMaxAmp());
  xsp->setMaxColorAmp(_color_pop->getMaxAmp());
  insp->setCenterPercent(_color_pop->centerPercents());
  xsp->setCenterPercent(_color_pop->centerPercents());
}
