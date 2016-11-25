// fgqc_compute_grid_type.cc: implementation file for FgQcComputeGridType class
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

#include "fgqc/fgqc_compute_grid_type.hh"
#include "cprim.h"
#include "named_constants.h"
#include "fgqc/fgqc_plot.hh"
#include "fgqc/fgqc_compute_grid.hh"
#include "fgqc/fgqc_compute_grid_pop.hh"
#include "dp/output_lut.hh"
#include "dp/grid_error_handler.hh"

enum {CHECK_COMPUTE_GRID, CHECK_COMPUTE_GRID_POP};

 FgQcComputeGridType::FgQcComputeGridType (FgQcPlot *fgqc_plot) :
  FgQcPlotType (fgqc_plot),
  _compute_grid           (0),
  _compute_grid_pop       (0)
{
  _compute_grid = new FgQcComputeGrid (_qcp);
  if (!_compute_grid) return;
  else if (!checkGridStatus(CHECK_COMPUTE_GRID)) return;
}

FgQcComputeGridType::~FgQcComputeGridType ()
{
  if (_compute_grid)     delete _compute_grid;
  if (_compute_grid_pop) delete _compute_grid_pop;
}

//============================================================================
//====================== When data has changed determine action ==============
//============================================================================
int FgQcComputeGridType::ValuesChanged(FieldGeometry * /*fg*/, long /*ixl*/,
                                int what_changed, long /*index*/, 
                                long /*nrem*/, long /*nins*/)
{                                    
int inform_action;

  switch(what_changed)
    {
    case FG_XLOC:
    case FG_YLOC:
    case FG_XGRID:
    case FG_YGRID:
    case FG_XSKID:
    case FG_YSKID:
    case FG_DIST:
      inform_action = MAKE_NEW_PLOT;
      break;

    case FG_ESKID:
    case FG_ELEV:
    case FG_SHOT:
    case FG_HD:
    case FG_TUH:
    case FG_RSTAT:
    case FG_SSTAT:
    case FG_SEL:
      inform_action = NO_ACTION;
      break;

    default:
      printf("did not find fg action number %d\n",what_changed);
      inform_action = NO_ACTION;
      break;
    }

  return(inform_action);
}

int FgQcComputeGridType::plot ()
{
  SeisPlot *sp = _qcp->sp();
  SeisColorPop *color_pop = _qcp->getColorPop();
  long i;
  int frame_num = 1, num_frames = 1; //may want movies later
  int stat = True;
  float xrange, yrange;
  double xperpix, yperpix;  
  float *image_data;
  float user_left = _qcp->getUserLeft();
  float user_right = _qcp->getUserRight();
  float user_top = _qcp->getUserTop();
  float user_bottom = _qcp->getUserBottom();

  if (_do_abort) _do_abort->setNewAction ();
// instantiate the popup
  if (!_compute_grid_pop) {
    _compute_grid_pop = new FgQcComputeGridPop (_qcp->plotWidget(),
      "Compute Grid", _compute_grid, _qcp, _qcp->hctx());
    if (!_compute_grid_pop)
       {
       if (_do_abort) _do_abort->actionComplete ();
       return(False);
       }
    else if (!checkGridStatus(CHECK_COMPUTE_GRID_POP))
       {
       if (_do_abort) _do_abort->actionComplete ();
       return(False);
       }
    _compute_grid_pop->makeAndManage ();
  }

// make space for the resultant float grid, initially set to zero
  _float_grid = new FloatGrid((int)_qcp->getNumx(), (int)_qcp->getNumy(),
    _do_abort);
  if(!FgQcPlotType::checkGridStatus(CHECK_GRID))
    {
    if (_do_abort) _do_abort->actionComplete ();
    return(False);
    }

// initialize the SeisPlot parameters
  sp->setMatchHeader(1);
  sp->setGridXYS(user_left, user_right, user_top, user_bottom);

// connect the resultant float grid to the SeisPlot object
  image_data = _float_grid->getArray ();
  if(!FgQcPlotType::checkGridStatus(CHECK_GRID))
    {
    if (_do_abort) _do_abort->actionComplete ();
    return(False);
    }

  stat = sp->initArrayTypeData(frame_num,num_frames, _qcp->getNumx(),
                               _qcp->getNumy(), image_data);
  if(!stat)
    {
    if (_do_abort) _do_abort->actionComplete ();
    return(stat);
    }

  xrange = user_right - user_left;
  yrange = user_top - user_bottom; //y is northing by default
  xperpix = xrange / (float)(_qcp->getNumx() - 1.0);
  yperpix = yrange / (float)(_qcp->getNumy() - 1.0);
  
  for(i=0;i<_qcp->getNumx();i++)
    sp->setHeader(i*sp->numHeaders() +
                    sp->matchHeader()-1, xperpix * i + user_left);

// set the initial amplitude range to be 0.0 to 1.0
  _minz = (float)0;
  _maxz = (float)1;

// set the spatial range to be the entire display
  _minx = MinimumValue(user_right,user_left);
  _maxx = MaximumValue(user_right,user_left);
  _miny = MinimumValue(user_bottom,user_top);
  _maxy = MaximumValue(user_bottom,user_top);

  _qcp->setMinx(_minx);
  _qcp->setMaxx(_maxx);
  _qcp->setMiny(_miny);
  _qcp->setMaxy(_maxy);
  _qcp->setMinz(_minz);
  _qcp->setMaxz(_maxz);

  _float_grid->setRange(_float_grid->getUndefined(), _minz, _maxz);
  if(!FgQcPlotType::checkGridStatus(CHECK_GRID))
    {
    if (_do_abort) _do_abort->actionComplete ();
    return(False);
    }

  _image_ncols = (int)_qcp->getNumx ();
  _image_nrows = (int)_qcp->getNumy ();
  _image_col1  = 0;
  _image_row1  = 0;

  _qcp->setReach ((float)1 ,(float)1);

// setup annotation
  sp->setSymetricalAnnotation(user_left,user_right,user_top,user_bottom);
  sp->setDrawXlines(True);
  sp->setGridWidth(_qcp->getPlotWidth());
  sp->setGridHeight(_qcp->getPlotHeight());
  sp->setNorm(PlotImage::EXTERNALNORM);
  sp->setExternalAmp(MaximumValue(_maxz,_minz));

  if(!color_pop->beenManaged() || _qcp->getResetColors()) {
    sp->setPreDefColor(PlotImage::STANDARD);
    sp->setDoAmplitude(True);
    sp->setDoPercent(False);
    sp->setGradeVert(False);
    sp->setGradeHorz(False);
    color_pop->presetAmpType(True);
    color_pop->presetGrading(False);
  }

  _compute_grid->initializeDisplay ();
  stat = postPlot ();  // initially a blank plot

  if (_do_abort) _do_abort->actionComplete ();
  return stat;
}

int FgQcComputeGridType::postPlot ()
{
  SeisPlot *sp = _qcp->sp();
  SeisColorPop *color_pop = _qcp->getColorPop();
  int stat = True;

  if (_do_abort) _do_abort->setNewAction ();
  if (!_compute_grid->compressionStateIsSet()) {
    _minz = _float_grid->findMinimum ();
    _maxz = _float_grid->findMaximum ();
  }
  else /* if (_compute_grid->compressionStateIsSet()) */ {
    _minz = _compute_grid->resultLUT()->findMinimum ();
    _maxz = _compute_grid->resultLUT()->findMaximum ();
  }
  if (_minz == _maxz) _maxz += .00001;


  if (!color_pop->beenManaged() || _qcp->getResetColors()) {
    if (!_compute_grid->compressionStateIsSet()) {
      sp->setMinColorAmp(_minz);
      sp->setMaxColorAmp(_maxz);
    }
    else /* if(_compute_grid->compressionStateIsSet()) */ {
      int max_code = (int)_compute_grid->resultLUT()->getSize ();
      sp->setMinColorAmp((int)0);
      sp->setMaxColorAmp(max_code);
    }
    color_pop->presetAmplitudes (_minz, _maxz);
    if (!_compute_grid->compressionStateIsSet())
            sp->setPreDefColor(PlotImage::STANDARD);
  }

  stat = sp->plot();
  sp->backingStore(True);

  //Set the mouse to do no interpolation if we dont have a smoothed image
  if(stat)
    {
    if(_qcp->getFgColorPop()->Texture() == FgSeisColorPop::INSERT_DATA)
      sp->setLocationInterpolation(False, _float_grid->getUndefined());
    else
      sp->setLocationInterpolation(True,  _float_grid->getUndefined());
    }

  if (stat && _qcp->getNumColors() && !_compute_grid->compressionStateIsSet()){
    _hill_shader = new HillShader(_qcp->getNumColors());
    if (!FgQcPlotType::checkGridStatus(CHECK_SHADER))
        {
        if (_do_abort) _do_abort->actionComplete ();
        return(False);
        }
  }

  if (stat & !_qcp->inApplicationWindow()) {
    _qcp->setTitles ();
    wprocShowMsg( _qcp->helpLine(), _qcp->getTitle() );
  }

  if (_do_abort) _do_abort->actionComplete ();
  return stat;
}

int FgQcComputeGridType::editData ()
{
// write this as time permits and users require.
// this will be challenging, because, one answer is to look at each of the
//   input plots and to see in what regions one or more of them have been
//   edited and then translate the scope of the edited regions into one
//   encompassing result region and only update the result in that specific
//   region (whew!!!).  this is a bit more complicated versions of the
//   prepareInput and prepareResult routines currently written.
  return 0;
}

Boolean FgQcComputeGridType::checkGridStatus (long check_type, int dont_post)
{
  Boolean status;

  switch (check_type) {

    case CHECK_COMPUTE_GRID:
      if (_compute_grid->failed()) {
        if (dont_post)
          _qcp->_grid_error = False;
        else
          new GridErrorHandler (_qcp->gridError(), "Error",
            _compute_grid->errorStatus());
        delete _compute_grid;
        _compute_grid = 0;
        status = False;
      }
      else
        status = True;
      break;

    case CHECK_COMPUTE_GRID_POP:
      if (_compute_grid_pop->failed()) {
        if (dont_post)
          _qcp->_grid_error = False;
        else
          new GridErrorHandler (_qcp->gridError(), "Error",
            _compute_grid_pop->errorStatus());
        delete _compute_grid_pop;
        _compute_grid_pop = 0;
        status = False;
      }
      else
        status = True;
      break;
  }
  return status;
}
