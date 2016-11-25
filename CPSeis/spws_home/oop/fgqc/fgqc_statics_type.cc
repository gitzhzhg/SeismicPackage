// fgqc_statics_type.cc:  Implementation file for FgQcStaticsType class
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

#include "fgqc/fgqc_statics_type.hh"
#include "fgqc/fgqc_plot.hh"
#include "fgqc/statics_file.hh"
#include "cprim.h"
#include "named_constants.h"
#include <float.h>

FgQcStaticsType::FgQcStaticsType (FgQcPlot *fgqc_plot):
  FgQcPlotType (fgqc_plot)
{
  _not_defined = -FLT_MAX;
}


FgQcStaticsType::~FgQcStaticsType ()
{
}


void FgQcStaticsType::setStaticsFilename(char *filename)
{
  strcpy(_statics_filename, filename);
}

int FgQcStaticsType::checkStaticsFilename()
{
int error = 1;
char errmsg[512];

  // If a different statics file has been read since this plot was made
  // reread the correct file.
  if(strcmp(_statics_filename, 
             (char *)_qcp->getStaticsFile()->getCurrentFilename()))
    {
      error = _qcp->getStaticsFile()->readFile(_statics_filename, errmsg);
    }
  else
    {
      error = 0;
    }

  return error;
}


//============================================================================
//====================== Find number of data points  =========================
//============================================================================
void FgQcStaticsType::computeNumPoints (float xmin, float xmax, float ymin,
  float ymax, float *min_ave_flag_dist)
{
long i, j, k;
double xloc, yloc, xgrid, ygrid;
float zval, zero = 0;
float znil = ZNIL;
FieldGeometry *fg = _qcp->fg();

  if(checkStaticsFilename())
    return;

  StaticsFile *sf = _qcp->getStaticsFile();

  _num_points = 0;
  if (min_ave_flag_dist != 0) *min_ave_flag_dist = computeMinAveFlagDist ();

  int file_coordinate_system = sf->getCoordinateSystem ();
  int plot_coordinate_system = getCoordinateSystem ();
  long count = 0;
  if (file_coordinate_system == StaticsFile::SURVEYSYSTEM ||
      file_coordinate_system == StaticsFile::GRIDSYSTEM     )
    {
    float xval, yval;
    long num_xy_coords = sf->getNumX() * sf->getNumY();
    sf->getFirstPoint (&xval, &yval, &zval);
    for (i = 0; i < num_xy_coords;i++)
      {
      if ((file_coordinate_system == StaticsFile::SURVEYSYSTEM  &&
           plot_coordinate_system ==              SURVEYSYSTEM) ||
          (file_coordinate_system == StaticsFile::GRIDSYSTEM    &&
           plot_coordinate_system ==              GRIDSYSTEM)     )
        {
        xloc = (double)xval;
        yloc = (double)yval;
        }
      else if (file_coordinate_system == StaticsFile::SURVEYSYSTEM &&
               plot_coordinate_system ==              GRIDSYSTEM     )
        {
        xloc = fg->getXgridCoord (xval, yval);
        yloc = fg->getYgridCoord (xval, yval);
        }
      else if (file_coordinate_system == StaticsFile::GRIDSYSTEM   &&
               plot_coordinate_system ==              SURVEYSYSTEM   )
        {
        xloc = fg->getXlocCoord (xval, yval);
        yloc = fg->getYlocCoord (xval, yval);
        }
      else return;

      if(xloc >= xmin && xloc <= xmax && yloc >= ymin && 
         yloc <= ymax && zval != znil && zval != zero   )
        {
        _num_points++;
        }
      if (_do_abort)
        {
        count++;
        if (!(count % 5000))
          {
          if (_do_abort->userAbort())
            {
            return;
            }
          }
        }
      sf->getNextPoint (&xval, &yval, &zval);
      }
    }
  else if (file_coordinate_system == StaticsFile::GROUNDPOSITION)
// file: seqnt grnd posn sys
    {
    float grnd_posn, unused_y;
    long num_grnd_posns = sf->getNumX() * sf->getNumY();
    sf->getFirstPoint (&grnd_posn, &unused_y, &zval);
    long num_lines = fg->numLines ();
    for (i = 0; i < num_grnd_posns; i++)
      {
      for (j = 0; j < num_lines; j++)
	{
        k = fg->findGroundPosition (j, (long)grnd_posn);
        if (k != -1)
	  {
          if(plot_coordinate_system == SURVEYSYSTEM)
	    {
            xloc = fg->getXloc (j, k);
            yloc = fg->getYloc (j, k);
	    }
          else // plot coordinate system is GRIDSYSTEM
	    {
            xloc = fg->getXgrid (j, k);
            yloc = fg->getYgrid (j, k);
            }
          if(xloc >= xmin && xloc <= xmax && yloc >= ymin && yloc <= ymax
            && zval != znil && zval != zero)
            _num_points++;
          if (_do_abort)
            {
            count++;
            if (!(count % 5000))
              {
              if (_do_abort->userAbort())
                {
                _num_points = 0;
                return;
                }
              }
            }
          }
        }
      sf->getNextPoint (&grnd_posn, &unused_y, &zval);
      }
    }
  else if (file_coordinate_system == StaticsFile::GROUPSYSTEM)
// file: original group no.s
    {
    float group_num, unused_y;
    long num_groups = sf->getNumX() * sf->getNumY();
    sf->getFirstPoint (&group_num, &unused_y, &zval);
    for (i = 0; i < num_groups; i++)
      {
      fg->getSkiddedSourceCoords ((long)group_num, &xloc, &yloc);
      if (plot_coordinate_system == GRIDSYSTEM) {
        xgrid = fg->getXgridCoord (xloc, yloc);
        ygrid = fg->getYgridCoord (xloc, yloc);
        xloc = xgrid;
        yloc = ygrid;
      }
      if(xloc >= xmin && xloc <= xmax && yloc >= ymin && yloc <= ymax
        && zval != znil && zval != zero)
        _num_points++;
      if (_do_abort)
        {
        count++;
        if (!(count % 5000))
          {
          if (_do_abort->userAbort())
            {
            _num_points = 0;
            return;
            }
          }
        }
      sf->getNextPoint (&group_num, &unused_y, &zval);
      }
    }
}

//============================================================================
//============= Find the minimum average distance between flags  =============
//============================================================================
float FgQcStaticsType::computeMinAveFlagDist ()
{
float retval;
long i;
FieldGeometry *fg = _qcp->fg();

  if(checkStaticsFilename())
    return 0.0F;

  StaticsFile *sf = _qcp->getStaticsFile();

  double x_length, y_length, ave_flag_dist, divisor, divisor1, divisor2;
  long num_lines = fg->numLines ();
  long num_flags;

  long num_xy_coords = sf->getNumX() * sf->getNumY();
  divisor1 = sqrt ((double)num_xy_coords) - (double)1;
  
  int file_coordinate_system = sf->getCoordinateSystem ();
  int plot_coordinate_system = getCoordinateSystem ();
  if (file_coordinate_system == StaticsFile::SURVEYSYSTEM   ||
      file_coordinate_system == StaticsFile::GRIDSYSTEM     ||
      file_coordinate_system == StaticsFile::GROUNDPOSITION ||
      file_coordinate_system == StaticsFile::GROUPSYSTEM      )
    {
    retval = 0.0;

    for(i=0; i<num_lines; i++)
      {
      num_flags = fg->numFlagsOnLine (i);
      if(num_flags > 1)
	{
        if(plot_coordinate_system == SURVEYSYSTEM)
	  {
          x_length = fg->getXloc(i,0) - fg->getXloc(i,num_flags-1);
          y_length = fg->getYloc(i,0) - fg->getYloc(i,num_flags-1);
	  }
        else // GRIDSYSTEM
	  {
          x_length= fg->getXgrid(i,0) - fg->getXgrid(i,num_flags-1);
          y_length= fg->getYgrid(i,0) - fg->getYgrid(i,num_flags-1);
          }
        divisor2 = (double)(num_flags - 1);
        divisor = divisor1 > divisor2 ? divisor1 : divisor2;
        ave_flag_dist = sqrt (x_length*x_length+y_length*y_length) / divisor;
        if(retval == 0.0) // first time, use as is
          retval = (float)ave_flag_dist;
        else // after first, choose the smallest
	  {
          if((float)ave_flag_dist < retval)
            retval = (float)ave_flag_dist;
          }
        }
      }
    }
  if(retval == 0.0) retval = 1.0;
  return retval;
}


//============================================================================
//====================== When data has changed determine action ==============
//============================================================================
int FgQcStaticsType::ValuesChanged(FieldGeometry * /*fg*/, long /*ixl*/,
                                     int what_changed, long /*index*/, 
                                     long nrem, long nins)
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
      if(nrem != nins)
        inform_action = MAKE_NEW_PLOT;
      else
        inform_action = EDIT_PLOT;
      break;
   
    case FG_DIST:
      inform_action = MAKE_NEW_PLOT;
      break;

    case FG_ELEV:
    case FG_ESKID:
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
int FgQcStaticsType::plot ()
{
SeisPlot *sp = _qcp->sp();
FieldGeometry *fg = _qcp->fg();
SeisColorPop *color_pop = _qcp->getColorPop();
long i, j, k, index;
int frame_num = 1, num_frames = 1; //may want movies later
int stat = True;
float xrange, yrange, zval, zero = 0;
double xperpix, yperpix;  
double xloc, yloc;
int min_reach = 3;
int dont_resample;
int dont_smooth = 1;
int which_dimension;
int x_pixel_size = 3, y_pixel_size = 3;
float znil = ZNIL;
float *image_data;
float user_left = _qcp->getUserLeft();
float user_right = _qcp->getUserRight();
float user_top = _qcp->getUserTop();
float user_bottom = _qcp->getUserBottom();
float minx = MinimumValue(user_left,user_right);
float maxx = MaximumValue(user_left,user_right);
float miny = MinimumValue(user_top,user_bottom);
float maxy = MaximumValue(user_top,user_bottom);


  if(checkStaticsFilename())
    return 0;

  StaticsFile *sf = _qcp->getStaticsFile();
  if (sf->getCoordinateSystem() == StaticsFile::UNRECOGNIZEDSYSTEM)
    return(False);
 

  if (_do_abort) _do_abort->setNewAction ();
  long num_x_coords = _qcp->getNumx ();
  long num_y_coords = _qcp->getNumy ();
  if (_float_grid) delete _float_grid, _float_grid = 0;
  _float_grid = new FloatGrid((int)num_x_coords, (int)num_y_coords,
    _do_abort);
  if (!checkGridStatus(CHECK_GRID)) {
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
  if(!checkGridStatus(CHECK_FGA))  {
    _do_abort->actionComplete ();
    return (False);
  }

  sp->setMatchHeader(1);
  sp->setGridXYS(user_left, user_right, user_top, user_bottom);
  stat = sp->initArrayTypeData(frame_num,num_frames, num_x_coords,
                               num_y_coords, _float_grid->getArray());
  if(!stat) {
    _do_abort->actionComplete ();
    return (stat);
  }
  image_data = _float_grid->getArray();
  for(i=0;i<num_x_coords*num_y_coords;i++) image_data[i] = _not_defined;

  float xwidth, ywidth;
  if (_texture_state == FgSeisColorPop::SMOOTH_DATA)
    computeNumPoints(minx, maxx, miny, maxy);
  else /* if (_texture_state == FgSeisColorPop::INSERT_DATA) */ {
    computeNumPoints(minx, maxx, miny, maxy, &xwidth);
    ywidth = xwidth;
  }

  if(!_num_points) {
    _do_abort->actionComplete ();
    return (False);
  }

  if (_control_points) delete _control_points, _control_points = 0;
  _control_points = new ControlPoints(_num_points);
  if (!checkGridStatus(CHECK_CONTROL)) {
    _do_abort->actionComplete ();
    return (False);
  }

  _fg_data = _control_points->getArray();

  xrange = user_right - user_left;
  yrange = user_top - user_bottom; //y is northing by default
  xperpix = xrange / (float)(num_x_coords - 1);
  yperpix = yrange / (float)(num_y_coords - 1);

  for (i = 0; i < num_x_coords; i++)
    sp->setHeader(i*sp->numHeaders() +
                  sp->matchHeader()-1, xperpix * i + user_left);

  index = 0;

  int file_coordinate_system = sf->getCoordinateSystem ();
  int plot_coordinate_system = getCoordinateSystem ();
  long count = 0;
  if (file_coordinate_system == StaticsFile::SURVEYSYSTEM ||
      file_coordinate_system == StaticsFile::GRIDSYSTEM     )
    {
    float xval, yval;
    long num_xy_coords = sf->getNumX() * sf->getNumY();
    sf->getFirstPoint (&xval, &yval, &zval);
    for (i = 0; i < num_xy_coords;i++)
      {
      if ((file_coordinate_system == StaticsFile::SURVEYSYSTEM  &&
           plot_coordinate_system ==              SURVEYSYSTEM) ||
          (file_coordinate_system == StaticsFile::GRIDSYSTEM    &&
           plot_coordinate_system ==              GRIDSYSTEM)     )
        {
        xloc = (double)xval;
        yloc = (double)yval;
        }
      else if (file_coordinate_system == StaticsFile::SURVEYSYSTEM &&
               plot_coordinate_system ==              GRIDSYSTEM     )
        {
        xloc = fg->getXgridCoord (xval, yval);
        yloc = fg->getYgridCoord (xval, yval);
        }
      else if (file_coordinate_system == StaticsFile::GRIDSYSTEM   &&
               plot_coordinate_system ==              SURVEYSYSTEM   )
        {
        xloc = fg->getXlocCoord (xval, yval);
        yloc = fg->getYlocCoord (xval, yval);
        }
      else {
        _do_abort->actionComplete ();
        return (False);
      }

      if(xloc >= minx && xloc <= maxx && yloc >= miny && 
         yloc <= maxy && zval != znil && zval != zero)
        {
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
      if (_do_abort)
        {
        count++;
        if (!(count % 5000))
          {
          if (_do_abort->userAbort())
            {
            return(False);
            }
          }
        }
      sf->getNextPoint (&xval, &yval, &zval);
      }
    }
  else if (file_coordinate_system == StaticsFile::GROUNDPOSITION)
// file: seqnt grnd posn sys
    {
    long num_lines = fg->numLines ();
    float grnd_posn, unused_y;
    long num_grnd_posns = sf->getNumX() * sf->getNumY();
    sf->getFirstPoint (&grnd_posn, &unused_y, &zval);
    for (i = 0; i < num_grnd_posns; i++)
      {
      for (j = 0; j < num_lines; j++)
	{
        k = fg->findGroundPosition (j, (long)grnd_posn);
        if (k != -1)
	  {
          if(plot_coordinate_system == SURVEYSYSTEM)
	    {
            xloc = fg->getXloc (j, k);
            yloc = fg->getYloc (j, k);
	    }
          else // plot coordinate system is GRIDSYSTEM
	    {
            xloc = fg->getXgrid (j, k);
            yloc = fg->getYgrid (j, k);
            }
          if(xloc >= minx && xloc <= maxx && yloc >= miny &&
             yloc <= maxy && zval != znil && zval != zero)
            {
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
        if (_do_abort)
          {
          count++;
          if (!(count % 5000))
            {
            if (_do_abort->userAbort())
              {
              return(False);
              }
            }
          }
        }
      sf->getNextPoint (&grnd_posn, &unused_y, &zval);
      }
    }
  else if (file_coordinate_system == StaticsFile::GROUPSYSTEM)
// file: original group no.s
    {
    float group_num, unused_y;
    long num_groups = sf->getNumX() * sf->getNumY();
    sf->getFirstPoint (&group_num, &unused_y, &zval);
    for (i = 0; i < num_groups; i++)
      {
      fg->getSkiddedSourceCoords ((long)group_num, &xloc, &yloc);
      if(xloc >= minx && xloc <= maxx && yloc >= miny &&
         yloc <= maxy && zval != znil && zval != zero)
        {
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
      if (_do_abort)
        {
        count++;
        if (!(count % 5000))
          {
          if (_do_abort->userAbort())
            {
            return(False);
            }
          }
        }
      sf->getNextPoint (&group_num, &unused_y, &zval);
      }
    }
  else {
    _do_abort->actionComplete ();
    return (False);
  }

// check for no statics corrections input
  if(_maxz == znil) {
    _do_abort->actionComplete ();
    return (False);
  }

  if(_minz == _maxz) _maxz += .00001;
  _qcp->setMinx(_minx);
  _qcp->setMaxx(_maxx);
  _qcp->setMiny(_miny);
  _qcp->setMaxy(_maxy);
  _qcp->setMinz(_minz);
  _qcp->setMaxz(_maxz);

  _float_grid->setRange (_not_defined, _minz, _maxz);
  if(!checkGridStatus(CHECK_GRID)) {
    _do_abort->actionComplete ();
    return (False);
  }

  _control_points->setExtremaLimits(0,_qcp->getUserLeft(),_qcp->getUserRight());
  _control_points->setExtremaLimits(1,_qcp->getUserTop(),_qcp->getUserBottom());
  _control_points->setExtremaLimits(2, _minz, _maxz);
  if (!checkGridStatus(CHECK_CONTROL)) {
    _do_abort->actionComplete ();
    return (False);
  }

  if (_texture_state == FgSeisColorPop::SMOOTH_DATA) {
    min_reach = 3;
    dont_resample = 0;
  }
  else /*if (_texture_state == FgSeisColorPop::INSERT_DATA) */ {
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

  _auto_gridder_float->initializeCalculations (_control_points,
                                 _float_grid_accessor, &xwidth, &ywidth);
  if (!checkGridStatus(CHECK_AUTO)) {
    _do_abort->actionComplete ();
    return (False);
  }

  if (which_dimension == X_ONLY) { // 2d data, need to insure height of image
    y_pixel_size = (int)(((float)_qcp->getNumy()) * 0.5);
    if (y_pixel_size < 1) {
      _do_abort->actionComplete ();
      return (False);
    }
    _auto_gridder_float->setPointSize (_float_grid, x_pixel_size, y_pixel_size);
  }

  if (which_dimension == Y_ONLY) { // 2d data, need to insure width of image
    x_pixel_size = (int)(((float)_qcp->getNumx()) * 0.5);
    if (x_pixel_size < 1) {
      _do_abort->actionComplete ();
      return (False);
    }
    _auto_gridder_float->setPointSize (_float_grid, x_pixel_size, y_pixel_size);
  }

  _auto_gridder_float->analyze (_control_points, _float_grid);
  if (!checkGridStatus(CHECK_AUTO)) {
    _do_abort->actionComplete ();
    return (False);
  }

  _qcp->setReach (xwidth ,ywidth);
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
    if (!checkGridStatus(CHECK_SHADER)) {
      _do_abort->actionComplete ();
      return (False);
    }
    }
  if(stat && _qcp->inApplicationWindow() == False)
    {
     _qcp->setTitles ();
     wprocShowMsg( _qcp->helpLine(), _qcp->getTitle() );
    }

  if (_do_abort) _do_abort->actionComplete ();
  return(stat);
}

//============================================================================
//====================== Edit existing plot            =======================
//============================================================================
int FgQcStaticsType::editData ()
{
long index = 0;
long first_col, last_col, first_row, last_row;
long tempval;
double xperpix, xrange;
long i, j, k, num_found = 0;
double xloc, yloc;
float zval, zero = 0;
float znil = ZNIL;
const float *hd = _qcp->sp()->firstDisplayedHeaderData();
FieldGeometry *fg = _qcp->fg();
SeisPlot *sp = _qcp->sp();
float minx = MinimumValue(_qcp->getPostMinx(),_qcp->getPostMaxx());
float maxx = MaximumValue(_qcp->getPostMinx(),_qcp->getPostMaxx());
float miny = MinimumValue(_qcp->getPostMiny(),_qcp->getPostMaxy());
float maxy = MaximumValue(_qcp->getPostMiny(),_qcp->getPostMaxy());


  if(checkStaticsFilename())
    return 0;

  StaticsFile *sf = _qcp->getStaticsFile();

  if (sf->getCoordinateSystem() == StaticsFile::UNRECOGNIZEDSYSTEM)
  return(False);


  if (_do_abort) _do_abort->setNewAction ();
  computeNumPoints(minx, maxx, miny, maxy);
  if (_control_points) delete _control_points, _control_points = 0;
  _control_points = new ControlPoints(_num_points);
  if (!checkGridStatus(CHECK_CONTROL)) {
    _do_abort->actionComplete ();
    return (False);
  }
  _fg_data = _control_points->getArray();
  if(_fg_data == NULL) {
    _do_abort->actionComplete ();
    return (False);
  }

  index = 0;

  int file_coordinate_system = sf->getCoordinateSystem ();
  int plot_coordinate_system = getCoordinateSystem ();
  long count = 0;
  if (file_coordinate_system == StaticsFile::SURVEYSYSTEM ||
      file_coordinate_system == StaticsFile::GRIDSYSTEM     )
    {
    float xval, yval;
    long num_xy_coords = sf->getNumX() * sf->getNumY();
    sf->getFirstPoint (&xval, &yval, &zval);
    for (i = 0; i < num_xy_coords;i++)
      {
      if ((file_coordinate_system == StaticsFile::SURVEYSYSTEM  &&
           plot_coordinate_system ==              SURVEYSYSTEM) ||
          (file_coordinate_system == StaticsFile::GRIDSYSTEM    &&
           plot_coordinate_system ==              GRIDSYSTEM)     )
        {
        xloc = (double)xval;
        yloc = (double)yval;
        }
      else if (file_coordinate_system == StaticsFile::SURVEYSYSTEM &&
               plot_coordinate_system ==              GRIDSYSTEM     )
        {
        xloc = fg->getXgridCoord (xval, yval);
        yloc = fg->getYgridCoord (xval, yval);
        }
      else if (file_coordinate_system == StaticsFile::GRIDSYSTEM   &&
               plot_coordinate_system ==              SURVEYSYSTEM   )
        {
        xloc = fg->getXlocCoord (xval, yval);
        yloc = fg->getYlocCoord (xval, yval);
        }
      else {
        _do_abort->actionComplete ();
        return (False);
      }

      if(xloc >= minx && xloc <= maxx && yloc >= miny && yloc <= maxy &&
         zval != znil && zval != zero)
        {
        _fg_data[index]   = xloc;
        _fg_data[++index] = yloc;
        _fg_data[++index] = zval;
        ++index;
        num_found++;
        }
      if (_do_abort)
        {
        count++;
        if (!(count % 5000))
          {
          if (_do_abort->userAbort())
            {
            return(False);
            }
          }
        }
      }
    }
  else if (file_coordinate_system == StaticsFile::GROUNDPOSITION)
// file: seqnt grnd posn sys
    {
    long num_lines = fg->numLines ();
    float grnd_posn, unused_y;
    long num_grnd_posns = sf->getNumX() * sf->getNumY();
    sf->getFirstPoint (&grnd_posn, &unused_y, &zval);
    for (i = 0; i < num_grnd_posns; i++)
      {
      for (j = 0; j < num_lines; j++)
	{
        k = fg->findGroundPosition (j, (long)grnd_posn);
        if (k != -1)
	  {
          if(plot_coordinate_system == SURVEYSYSTEM)
	    {
            xloc = fg->getXloc (j, k);
            yloc = fg->getYloc (j, k);
	    }
          else // plot coordinate system is GRIDSYSTEM
	    {
            xloc = fg->getXgrid (j, k);
            yloc = fg->getYgrid (j, k);
            }
          if(xloc >= minx && xloc <= maxx && yloc >= miny && yloc <= maxy &&
            zval != znil && zval != zero)
            {
            _fg_data[index]   = xloc;
            _fg_data[++index] = yloc;
            _fg_data[++index] = zval;
            ++index;
            num_found++;
            }
          }
        if (_do_abort)
          {
          count++;
          if (!(count % 5000))
            {
            if (_do_abort->userAbort())
              {
              return(False);
              }
            }
          }
        }
      }
    }
  else if (file_coordinate_system == StaticsFile::GROUPSYSTEM)
// file: original group no.s
    {
    float group_num, unused_y;
    long num_groups = sf->getNumX() * sf->getNumY();
    sf->getFirstPoint (&group_num, &unused_y, &zval);
    for (i = 0; i < num_groups; i++)
      {
      fg->getSkiddedSourceCoords ((long)group_num, &xloc, &yloc);
      if(xloc >= minx && xloc <= maxx && yloc >= miny && yloc <= maxy &&
        zval != znil && zval != zero)
        {
        _fg_data[index]   = xloc;
        _fg_data[++index] = yloc;
        _fg_data[++index] = zval;
        ++index;
        num_found++;
        }
      if (_do_abort)
        {
        count++;
        if (!(count % 5000))
          {
          if (_do_abort->userAbort())
            {
            return(False);
            }
          }
        }
      sf->getNextPoint (&group_num, &unused_y, &zval);
      }
    }

  if(!num_found || _num_points != num_found ) {
    _do_abort->actionComplete ();
    return (False);
  }
  _auto_gridder_float->analyze (_control_points, _float_grid);
  if (!checkGridStatus(CHECK_AUTO)) {
    _do_abort->actionComplete ();
    return (False);
  }

  xrange    = sp->plottedGridX2() - sp->plottedGridX1();
  xperpix   = xrange / (float)(_qcp->getNumx() - 1.0);
  first_col = (long)((minx - sp->plottedGridX1()) / xperpix);
  last_col  = (long)((maxx - sp->plottedGridX1()) / xperpix);
  first_row = (long)((miny - sp->plottedGridY1())
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
