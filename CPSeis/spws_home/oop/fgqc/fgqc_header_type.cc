// fgqc_header_type.cc
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

#include <limits.h>
#include <stdio.h>
#include <string.h>
#include <float.h>
#include "fgqc/fgqc_header_type.hh"
#include "fgqc/fgqc_plot.hh"
#include "dp/grid_error_handler.hh"
#include "fgqc/fgqc_plot_constants.hh"
#include "named_constants.h"

FgQcHeaderType::FgQcHeaderType (FgQcPlot *fgqc_plot) 
                               : FgQcPlotType (fgqc_plot)
{
  _not_defined = -FLT_MAX;
}


FgQcHeaderType::~FgQcHeaderType()
{
}

//============================================================================
//====================== When data has changed determine action ==============
//============================================================================
int FgQcHeaderType::ValuesChanged(FieldGeometry * /*fg*/, long /*ixl*/,
                                  int what_changed, long /*index*/, 
                                  long nrem, long nins)
{                                    

// result may depend upon header word being plotted

int inform_action;

  switch(what_changed)
    {
    case FG_XLOC:
    case FG_YLOC:
    case FG_ELEV:
    case FG_XGRID:
    case FG_YGRID:
    case FG_XSKID:
    case FG_YSKID:
    case FG_ESKID:
      if(nrem != nins)
        inform_action = MAKE_NEW_PLOT;
      else
        inform_action = EDIT_PLOT;
      break;
   
    case FG_DIST:
      inform_action = MAKE_NEW_PLOT;
      break;

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
    }

  return(inform_action);
}


//============================================================================
//====================== Get data for plot             =======================
//============================================================================
int FgQcHeaderType::plot()
{
SeisPlot *sp = _qcp->sp();
FieldGeometry *fg = _qcp->fg();
SeisColorPop *color_pop = _qcp->getColorPop();
long i, j, index, k, l, trace;
int frame_num = 1, num_frames = 1; //may want movies later
int stat = True;
float xrange, yrange;
double xperpix, yperpix;  
float xloc, yloc, zval;
double dxloc, dyloc, dzval;
int min_reach;
int dont_resample;
int dont_smooth = 1;
int which_dimension;
int x_pixel_size=3, y_pixel_size=3;
float *image_data;
float user_left = _qcp->getUserLeft();
float user_right = _qcp->getUserRight();
float user_top = _qcp->getUserTop();
float user_bottom = _qcp->getUserBottom();
float minx = MinimumValue(user_left,user_right);
float maxx = MaximumValue(user_left,user_right);
float miny = MinimumValue(user_top,user_bottom);
float maxy = MaximumValue(user_top,user_bottom);


  if (_do_abort) _do_abort->setNewAction ();
  if (_float_grid) delete _float_grid, _float_grid = 0;
  _float_grid = new FloatGrid((int)_qcp->getNumx(), (int)_qcp->getNumy(),
    _do_abort);
  if(!checkGridStatus(CHECK_GRID)) {
    _do_abort->actionComplete ();
    return (False);
  }

  _float_grid_accessor = new FloatGridAccessor ();
  if(!checkGridStatus(CHECK_FGA)) {
    _do_abort->actionComplete ();
    return (False);
  }

  _float_grid_accessor->specifyData (_float_grid);
  _float_grid_accessor->setXBinData (user_left, user_right);
  _float_grid_accessor->setYBinData (user_bottom, user_top);
  if(!checkGridStatus(CHECK_FGA)) {
    _do_abort->actionComplete ();
    return (False);
  }

  sp->setMatchHeader(1);
  sp->setGridXYS(user_left, user_right, user_top, user_bottom);
  stat = sp->initArrayTypeData(frame_num,num_frames, _qcp->getNumx(),
                               _qcp->getNumy(), _float_grid->getArray());
  if(!stat) {
    _do_abort->actionComplete ();
    return (stat);
  }
  image_data = _float_grid->getArray();
  for(i=0;i<_qcp->getNumx()*_qcp->getNumy();i++) image_data[i] = _not_defined;

  long coord_system = getCoordinateSystem ();
  DataLocation data_location = (DataLocation)_qcp->getDataLocation ();
  _num_points = computeNumPoints (minx, maxx, miny, maxy,
                                  fg, coord_system, (long)data_location,
                                  _do_abort);

  if(!_num_points) {
    _do_abort->actionComplete ();
    return (False);
  }

  if (_control_points) delete _control_points, _control_points = 0;
  _control_points = new ControlPoints(_num_points);
  if(!checkGridStatus(CHECK_CONTROL)) {
    _do_abort->actionComplete ();
    return (False);
  }

  _fg_data = _control_points->getArray();


  xrange = user_right - user_left;
  yrange = user_top - user_bottom; //y is northing by default
  xperpix = xrange / (float)(_qcp->getNumx() - 1.0);
  yperpix = yrange / (float)(_qcp->getNumy() - 1.0);
  
  for(i=0;i<_qcp->getNumx();i++)
    sp->setHeader(i*sp->numHeaders() +
                   sp->matchHeader()-1, xperpix * i + user_left);

  fg->startHeadersFromScratch ();
  int header_word = (int)_qcp->getHeaderWord ();
  index = 0;
  long count = 0;
  if (data_location == PLOT_AT_CMP) {
    long num_gathers = fg->numCmpGathers ();
    long num_traces;
    for (i = 0; i < num_gathers; i++) {
      num_traces = fg->foldOfStack (i);
      for (j = 0; j < num_traces; j++) {
        if (coord_system == SURVEYSYSTEM) {
          fg->getCmpTraceLoc (i, j, &dxloc, &dyloc);
        }
        else /*if (coord_system == GRIDSYSTEM)*/ {
          fg->getCmpTraceGrid (i, j, &dxloc, &dyloc);
        }
        xloc = (float)dxloc;
        yloc = (float)dyloc;
        if (xloc >= minx && xloc <= maxx && yloc >= miny && yloc <= maxy) 
          {
          fg->calculateHeaderWords (fg->originalTraceIndex(i,j)+1, True);
          dzval = fg->getHeaderWordValue (header_word);
          zval = (float)dzval;
          _fg_data[index]   = xloc;
          _fg_data[++index] = yloc;
          _fg_data[++index] = zval;
          _minx = _minx > xloc ? xloc : _minx;
          _maxx = _maxx < xloc ? xloc : _maxx;
          _miny = _miny > yloc ? yloc : _miny;
          _maxy = _maxy < yloc ? yloc : _maxy;           
          _minz = _minz > zval ? zval : _minz;
          _maxz = _maxz < zval ? zval : _maxz;
          ++index;
	}
        if (_do_abort) {
          count++;
          if (!(count % (long)2000)) {
	    if (_do_abort->userAbort()) return False;
          }
        }
      }
    }
  }
  
  else /*if (data_location != PLOT_AT_CMP)*/ {

    long num_lines = fg->numLines ();
    long num_flags, num_sources, group, num_channels, num_receivers;
    double xgrid, ygrid;
    for (i = 0; i < num_lines; i++) {
      num_flags = fg->numFlagsOnLine (i);
      for (j = 0; j < num_flags; j++) {
        if (data_location == PLOT_AT_SOURCE) {
          num_sources = fg->numSourcesAtFlag (i, j);
          for (k = 0; k < num_sources; k++) {
            group = fg->sourceGroupNumber (i, j, k);
            if (group > 0) {
              num_channels = fg->findNumChannelsInGroup (group);
              fg->getSkiddedSourceCoords (group, &dxloc, &dyloc);
              if (coord_system == GRIDSYSTEM) {
                xgrid = fg->getXgridCoord (dxloc, dyloc);
                ygrid = fg->getYgridCoord (dxloc, dyloc);
                dxloc = xgrid;
                dyloc = ygrid;
              }
              xloc = (float)dxloc;
              yloc = (float)dyloc;
              if(xloc >= minx && xloc <= maxx && yloc >= miny && yloc <= maxy)
                {
                for (l = 0; l < num_channels; l++) {
                  trace = fg->findTraceNumber (group, l);
                  if (trace > 0) {
                    fg->calculateHeaderWords (trace, True);
                    dzval = fg->getHeaderWordValue (header_word);
                    zval = (float)dzval;
                    _fg_data[index]   = xloc;
                    _fg_data[++index] = yloc;
                    _fg_data[++index] = zval;
                    _minx = _minx > xloc ? xloc : _minx;
                    _maxx = _maxx < xloc ? xloc : _maxx;
                    _miny = _miny > yloc ? yloc : _miny;
                    _maxy = _maxy < yloc ? yloc : _maxy;           
                    _minz = _minz > zval ? zval : _minz;
                    _maxz = _maxz < zval ? zval : _maxz;
                    ++index;
	          }
                  if (_do_abort) {
                    count++;
                    if (!(count % (long)2000)) {
	              if (_do_abort->userAbort()) return False;
                    }
                  }
                }
              }
            }
            if (_do_abort) {
              count++;
              if (!(count % (long)2000)) {
	        if (_do_abort->userAbort()) return False;
              }
            }
	  }
        }
        else /*if (data_location == PLOT_AT_RECEIVER)*/ {
          num_receivers = fg->numReceiversAtFlag (i, j);
          for (k = 0; k < num_receivers; k++) {
            trace = fg->receiverTraceNumber (i, j, k);
            if (trace > 0) {
              fg->getSkiddedTraceCoords (i, j, k, &dxloc, &dyloc);
              if (coord_system == GRIDSYSTEM) {
                xgrid = fg->getXgridCoord (dxloc, dyloc);
                ygrid = fg->getYgridCoord (dxloc, dyloc);
                dxloc = xgrid;
                dyloc = ygrid;
              }
              xloc = (float)dxloc;
              yloc = (float)dyloc;
              if (xloc >= minx && xloc <= maxx && yloc >= miny && yloc <= maxy)
                {
                fg->calculateHeaderWords (trace, True);
                dzval = fg->getHeaderWordValue (header_word);
                zval = (float)dzval;
                _fg_data[index]   = xloc;
                _fg_data[++index] = yloc;
                _fg_data[++index] = zval;
                _minx = _minx > xloc ? xloc : _minx;
                _maxx = _maxx < xloc ? xloc : _maxx;
                _miny = _miny > yloc ? yloc : _miny;
                _maxy = _maxy < yloc ? yloc : _maxy;           
                _minz = _minz > zval ? zval : _minz;
                _maxz = _maxz < zval ? zval : _maxz;
                ++index;
	      }
	    }
            if (_do_abort) {
              count++;
              if (!(count % (long)2000)) {
	        if (_do_abort->userAbort()) return False;
              }
            }
          } 
	}
      }
    }
  }

  if(_minz == _maxz) _maxz += .00001;
  _qcp->setMinx(_minx);
  _qcp->setMaxx(_maxx);
  _qcp->setMiny(_miny);
  _qcp->setMaxy(_maxy);
  _qcp->setMinz(_minz);
  _qcp->setMaxz(_maxz);

  _float_grid->setRange(_not_defined, _minz, _maxz);
  if(!checkGridStatus(CHECK_GRID)) {
    _do_abort->actionComplete ();
    return (False);
  }
  _control_points->setExtremaLimits(0,_qcp->getUserLeft(),_qcp->getUserRight());
  if(!checkGridStatus(CHECK_CONTROL)) {
    _do_abort->actionComplete ();
    return (False);
  }
  _control_points->setExtremaLimits(1,_qcp->getUserTop(),_qcp->getUserBottom());
  if(!checkGridStatus(CHECK_CONTROL)) {
    _do_abort->actionComplete ();
    return (False);
  }
  _control_points->setExtremaLimits(2, _minz, _maxz);
  if(!checkGridStatus(CHECK_CONTROL)) {
    _do_abort->actionComplete ();
    return (False);
  }

  if (_texture_state == FgSeisColorPop::SMOOTH_DATA) {
    min_reach = 3;
    dont_resample = 0;
  }
  else /* if (_texture_state == FgSeisColorPop::INSERT_DATA) */ {
    min_reach = 0;
    dont_resample = 1;
    _float_grid->setInsertSize (_point_size, _point_size);
  }

  _dont_scale = 1;
  _max_hits   = 3;
  if (_auto_gridder_float) delete _auto_gridder_float, _auto_gridder_float = 0;

  which_dimension = getDimensions ();
  _auto_gridder_float = new AutoGridderFloat (min_reach, dont_resample,
                                              _dont_scale, _max_hits,
                                              dont_smooth, which_dimension);

  if (!checkGridStatus(CHECK_AUTO)) {
    _do_abort->actionComplete ();
    return (False);
  }

  _auto_gridder_float->initializeCalculations(_control_points,
                                 _float_grid_accessor, &_x_reach, &_y_reach);
  if(!checkGridStatus(CHECK_AUTO)) {
    _do_abort->actionComplete ();
    return (False);
  }

  if (which_dimension == X_ONLY) {  // 2d data, need to insure height of image
    y_pixel_size = (int)(((float)_qcp->getNumy()) * 0.5);
    if (y_pixel_size < 1) {
      _do_abort->actionComplete ();
      return (False);
    }
    _auto_gridder_float->setPointSize (_float_grid, x_pixel_size, y_pixel_size);
  }
  else if (which_dimension == Y_ONLY) { // 2d data, need to insure image width
    x_pixel_size = (int)(((float)_qcp->getNumx()) * 0.5);
    if (x_pixel_size < 1) {
      _do_abort->actionComplete ();
      return (False);
    }
    _auto_gridder_float->setPointSize (_float_grid, x_pixel_size, y_pixel_size);
  }

  _auto_gridder_float->analyze(_control_points, _float_grid);
  if(!checkGridStatus(CHECK_AUTO)) {
    _do_abort->actionComplete ();
    return (False);
  }

  _qcp->setReach (_x_reach, _y_reach);
  _image_col1 = _auto_gridder_float->firstColumn();
  _image_row1 = _auto_gridder_float->firstRow();
  _image_ncols= _auto_gridder_float->columnCount(); 
  _image_nrows= _auto_gridder_float->rowCount();

  delete _control_points, _control_points = 0;

  sp->setSymetricalAnnotation(user_left,user_right,user_top,user_bottom);
  sp->setDrawXlines(True);
  sp->setDrawYlines(True);
  sp->setGridWidth(_qcp->getPlotWidth());
  sp->setGridHeight(_qcp->getPlotHeight());
  sp->setNorm(PlotImage::EXTERNALNORM);
  sp->setExternalAmp(MaximumValue(_maxz,_minz)); 

  if(!color_pop->beenManaged() || _qcp->getResetColors())
    {
    if(_minz == _maxz) _maxz += .00001;
    sp->setPreDefColor(PlotImage::STANDARD);
    sp->setDoAmplitude(True);
    sp->setDoPercent(False);
    sp->setGradeVert(False);
    sp->setGradeHorz(False);
    sp->setMinColorAmp(_minz);
    sp->setMaxColorAmp(_maxz);
    color_pop->presetAmplitudes(_minz, _maxz);
    color_pop->presetAmpType(True);
    color_pop->presetGrading(False);
    }

  sp->setUndefinedValue(_float_grid->getUndefined());

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

  if(stat && _qcp->getNumColors()) 
    {
     if (_hill_shader) delete _hill_shader, _hill_shader = 0;
     _hill_shader = new HillShader(_qcp->getNumColors());
     if(!checkGridStatus(CHECK_SHADER)) {
       _do_abort->actionComplete ();
       return (False);
     } 
    }
  if(stat && !_qcp->inApplicationWindow()) {
     _qcp->setTitles ();
     wprocShowMsg( _qcp->helpLine(), _qcp->getTitle() );
    }

  if (_do_abort) _do_abort->actionComplete ();
  return(stat);
}


//============================================================================
//====================== Edit existing plot            =======================
//============================================================================
int FgQcHeaderType::editData()
{
long index = 0;
long first_col, last_col, first_row, last_row;
long tempval;
double xperpix, xrange;
long i, j, k, l, trace, num_found = 0;
float xloc, yloc, zval;
double dxloc, dyloc, dzval;
const float *hd = _qcp->sp()->firstDisplayedHeaderData();
FieldGeometry *fg = _qcp->fg();
SeisPlot *sp = _qcp->sp();
float minx = MinimumValue(_qcp->getPostMinx(),_qcp->getPostMaxx());
float maxx = MaximumValue(_qcp->getPostMinx(),_qcp->getPostMaxx());
float miny = MinimumValue(_qcp->getPostMiny(),_qcp->getPostMaxy());
float maxy = MaximumValue(_qcp->getPostMiny(),_qcp->getPostMaxy());


  if (_do_abort) _do_abort->setNewAction ();
  long coord_system = getCoordinateSystem ();
  DataLocation data_location = (DataLocation)_qcp->getDataLocation ();
  _num_points = computeNumPoints (MinimumValue(minx,maxx),
                                  MaximumValue(maxx,minx), 
                                  MinimumValue(miny,maxy),
                                  MaximumValue(maxy,miny),
                                  fg, coord_system, (long)data_location,
                                  _do_abort);
  if (!_num_points) {
    _do_abort->actionComplete ();
    return (False);
  }

  if (_control_points) delete _control_points, _control_points = 0;
  _control_points = new ControlPoints(_num_points);
  if(!checkGridStatus(CHECK_CONTROL)) {
    _do_abort->actionComplete ();
    return (False);
  }
  _fg_data = _control_points->getArray();
  if(_fg_data == NULL) {
    _do_abort->actionComplete ();
    return (False);
  }

  fg->startHeadersFromScratch ();
  int header_word = (int)_qcp->getHeaderWord ();
  long count = 0;
  if (data_location == PLOT_AT_CMP) {
    long num_gathers = fg->numCmpGathers ();
    long num_traces;
    for (i = 0; i < num_gathers; i++) {
      num_traces = fg->foldOfStack (i);
      for (j = 0; j < num_traces; j++) {
        if (coord_system == SURVEYSYSTEM) {
          fg->getCmpTraceLoc (i, j, &dxloc, &dyloc);
        }
        else /*if (coord_system == GRIDSYSTEM)*/ {
          fg->getCmpTraceGrid (i, j, &dxloc, &dyloc);
        }
        xloc = (float)dxloc;
        yloc = (float)dyloc;
        if (xloc >= minx && xloc <= maxx &&
            yloc >= miny && yloc <= maxy   ) {
          fg->calculateHeaderWords (fg->originalTraceIndex(i,j)+1, True);
          dzval = fg->getHeaderWordValue (header_word);
          zval = (float)dzval;
          _fg_data[index]   = xloc;
          _fg_data[++index] = yloc;
          _fg_data[++index] = zval;
          _minz = _minz > zval ? zval : _minz;
          _maxz = _maxz < zval ? zval : _maxz;
          ++index;
          num_found++;
	}
        if (_do_abort) {
          count++;
          if (!(count % (long)2000)) {
	    if (_do_abort->userAbort()) return False;
          }
        }
      }
    }
  }
  
  else /*if (data_location != PLOT_AT_CMP)*/ {

    long num_lines = fg->numLines ();
    long num_flags, num_sources, group, num_channels, num_receivers;
    double xgrid, ygrid;
    for (i = 0; i < num_lines; i++) {
      num_flags = fg->numFlagsOnLine (i);
      for (j = 0; j < num_flags; j++) {
        if (data_location == PLOT_AT_SOURCE) {
          num_sources = fg->numSourcesAtFlag (i, j);
          for (k = 0; k < num_sources; k++) {
            group = fg->sourceGroupNumber (i, j, k);
            if (group > 0) {
              num_channels = fg->findNumChannelsInGroup (group);
              fg->getSkiddedSourceCoords (group, &dxloc, &dyloc);
              if (coord_system == GRIDSYSTEM) {
                xgrid = fg->getXgridCoord (dxloc, dyloc);
                ygrid = fg->getYgridCoord (dxloc, dyloc);
                dxloc = xgrid;
                dyloc = ygrid;
              }
              xloc = (float)dxloc;
              yloc = (float)dyloc;
              if (xloc >= minx && xloc <= maxx &&
                  yloc >= miny && yloc <= maxy   ) {
                for (l = 0; l < num_channels; l++) {
                  trace = fg->findTraceNumber (group, l);
                  if (trace > 0) {
                    fg->calculateHeaderWords (trace, True);
                    dzval = fg->getHeaderWordValue (header_word);
                    zval = (float)dzval;
                    _fg_data[index]   = xloc;
                    _fg_data[++index] = yloc;
                    _fg_data[++index] = zval;
                    _minz = _minz > zval ? zval : _minz;
                    _maxz = _maxz < zval ? zval : _maxz;
                    ++index;
                    num_found++;
	          }
                  if (_do_abort) {
                    count++;
                    if (!(count % (long)2000)) {
	              if (_do_abort->userAbort()) return False;
                    }
                  }
                }
              }
            }
            if (_do_abort) {
              count++;
              if (!(count % (long)2000)) {
	        if (_do_abort->userAbort()) return False;
              }
            }
	  }
        }
        else /*if (data_location == PLOT_AT_RECEIVER)*/ {
          num_receivers = fg->numReceiversAtFlag (i, j);
          for (k = 0; k < num_receivers; k++) {
            trace = fg->receiverTraceNumber (i, j, k);
            if (trace > 0) {
              fg->getSkiddedTraceCoords (i, j, k, &dxloc, &dyloc);
              if (coord_system == GRIDSYSTEM) {
                xgrid = fg->getXgridCoord (dxloc, dyloc);
                ygrid = fg->getYgridCoord (dxloc, dyloc);
                dxloc = xgrid;
                dyloc = ygrid;
              }
              xloc = (float)dxloc;
              yloc = (float)dyloc;
              if (xloc >= minx && xloc <= maxx &&
                  yloc >= miny && yloc <= maxy   ) {
                fg->calculateHeaderWords (trace, True);
                dzval = fg->getHeaderWordValue (header_word);
                zval = (float)dzval;
                _fg_data[index]   = xloc;
                _fg_data[++index] = yloc;
                _fg_data[++index] = zval;
                _minz = _minz > zval ? zval : _minz;
                _maxz = _maxz < zval ? zval : _maxz;
                ++index;
                num_found++;
	      }
	    }
            if (_do_abort) {
              count++;
              if (!(count % (long)2000)) {
	        if (_do_abort->userAbort()) return False;
              }
            }
          } 
	}
      }
    }
  }

  if(!num_found || _num_points != num_found ) {
    _do_abort->actionComplete ();
    return (False);
  }


  _auto_gridder_float->analyze(_control_points, _float_grid);
  if(!checkGridStatus(CHECK_AUTO)) {
    _do_abort->actionComplete ();
    return (False);
  }

  xrange    = sp->plottedGridX2() - sp->plottedGridX1();
  xperpix   = xrange / (float)(_qcp->getNumx() - 1.0);
  first_col = (long)((minx - sp->plottedGridX1()) / xperpix);
  last_col  = (long)((maxx - sp->plottedGridX1()) / xperpix);
  first_row = (long)( (miny - sp->plottedGridY1())
            / sp->sampleRate());
  last_row  = (long)( (maxy - sp->plottedGridY1())
            / sp->sampleRate());
  if(first_col < 0) first_col = (-first_col);
  if(last_col  < 0) last_col  = (-last_col );
  if(first_row < 0) first_row = (-first_row);
  if(last_row  < 0) last_row  = (-last_row );
  if(first_col > last_col)
    {
    tempval   = first_col;
    first_col = last_col;
    last_col  = tempval;
    }
  if(first_row > last_row)
    {
    tempval   = first_row;
    first_row = last_row;
    last_row  = tempval;
    }
 
  sp->modArrayTypeImage( first_col, last_col, first_row, last_row,
                         sp->currentFrame());


  delete _control_points, _control_points = 0;

  if (_do_abort) _do_abort->actionComplete ();
  return(True);
}


//============================================================================
//====================== Find number of data points  =========================
//============================================================================
long FgQcHeaderType::computeNumPoints (float xmin, float xmax,
  float ymin, float ymax, FieldGeometry *fg, long coord_system,
  long data_loc, DoAbort *do_abort)
{
long num_points = 0;
long i, j, k, l, trace;
float xloc, yloc;
double dxloc, dyloc;
DataLocation data_location = (DataLocation)data_loc;

  long count = 0;
  if (data_location == PLOT_AT_CMP) {
    long num_gathers = fg->numCmpGathers ();
    long num_traces;
    for (i = 0; i < num_gathers; i++) {
      num_traces = fg->foldOfStack (i);
      for (j = 0; j < num_traces; j++) {
        if (coord_system == SURVEYSYSTEM) {
          fg->getCmpTraceLoc (i, j, &dxloc, &dyloc);
        }
        else /*if (coord_system == GRIDSYSTEM)*/ {
          fg->getCmpTraceGrid (i, j, &dxloc, &dyloc);
        }
        xloc = (float)dxloc;
        yloc = (float)dyloc;
        if (xloc >= xmin && xloc <= xmax &&
            yloc >= ymin && yloc <= ymax   ) num_points++;
        if (do_abort) {
          count++;
	  if (!(count % (long)2000)) {
	    if (do_abort->userAbort()) {
              return (long)0;
	    }
          }
        }
      }
    }
  }
  
  else /*if (data_location != PLOT_AT_CMP)*/ {

    long num_lines = fg->numLines ();
    long num_flags, num_sources, group, num_channels, num_receivers;
    double xgrid, ygrid;
    for (i = 0; i < num_lines; i++) {
      num_flags = fg->numFlagsOnLine (i);
      for (j = 0; j < num_flags; j++) {
        if (data_location == PLOT_AT_SOURCE) {
          num_sources = fg->numSourcesAtFlag (i, j);
          for (k = 0; k < num_sources; k++) {
            group = fg->sourceGroupNumber (i, j, k);
            if (group > 0) {
              num_channels = fg->findNumChannelsInGroup (group);
              fg->getSkiddedSourceCoords (group, &dxloc, &dyloc);
              if (coord_system == GRIDSYSTEM) {
                xgrid = fg->getXgridCoord (dxloc, dyloc);
                ygrid = fg->getYgridCoord (dxloc, dyloc);
                dxloc = xgrid;
                dyloc = ygrid;
              }
              xloc = (float)dxloc;
              yloc = (float)dyloc;
              if (xloc >= xmin && xloc <= xmax &&
                  yloc >= ymin && yloc <= ymax   ) {
                for (l = 0; l < num_channels; l++) {
                  trace = fg->findTraceNumber (group, l);
                  if (trace > 0) num_points++;
                }
              }
              if (do_abort) {
                count++;
	        if (!(count % (long)2000)) {
	          if (do_abort->userAbort()) {
                    return (long)0;
	          }
                }
              }
            }
            if (do_abort) {
              count++;
              if (!(count % (long)2000)) {
	        if (do_abort->userAbort()) return False;
              }
            }
	  }
        }
        else /*if (data_location == PLOT_AT_RECEIVER)*/ {
          num_receivers = fg->numReceiversAtFlag (i, j);
          for (k = 0; k < num_receivers; k++) {
            trace = fg->receiverTraceNumber (i, j, k);
            if (trace > 0) {
              fg->getSkiddedTraceCoords (i, j, k, &dxloc, &dyloc);
              if (coord_system == GRIDSYSTEM) {
                xgrid = fg->getXgridCoord (dxloc, dyloc);
                ygrid = fg->getYgridCoord (dxloc, dyloc);
                dxloc = xgrid;
                dyloc = ygrid;
              }
              xloc = (float)dxloc;
              yloc = (float)dyloc;
              if (xloc >= xmin && xloc <= xmax &&
                  yloc >= ymin && yloc <= ymax   ) num_points++;
              if (do_abort) {
                count++;
	        if (!(count % (long)2000)) {
	          if (do_abort->userAbort()) {
                    return (long)0;
	          }
                }
              }
	    }
            if (do_abort) {
              count++;
              if (!(count % (long)2000)) {
	        if (do_abort->userAbort()) return False;
              }
            }
          } 
	}
      }
    }
  }
  return num_points;
}
