//**************************************************************************
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
//                Author Michael L. Sherrill  08/95
//                      Fold plot type class
//**************************************************************************

#include "fgqc/fgqc_fold_type.hh"
#include "fgqc/fgqc_plot.hh"
#include "fgqc/fgqc_plot_constants.hh"
#include "dp/uchar_grid.hh"
#include "dp/uchar_grid_accessor.hh"
#include "dp/float_grid_accessor.hh"
#include "dp/distribution_slicer.hh"
#include "dp/distr_slicer_pop.hh"
#include "dp/grid_error_handler.hh"
#include "sl/sl_error_pop.hh"
#include "cprim.h"
#include "named_constants.h"
#include <math.h>

#define EPSILON 0.0001

enum {CHECK_SLICER, CHECK_UCGRID, CHECK_UGACCESS, CHECK_DSPOP, CHECK_FGACCESS,
      CHECK_TGACCESS, CHECK_UGRACCESS};
enum {FOLD_PLOT, OFFSET_DISTRIBUTION_PLOT, AZIMUTH_DISTRIBUTION_PLOT};
enum {MEDIAN, AVERAGE, SELECT};
enum {DONT_ALLOW_INSERT_VECTOR, ALLOW_INSERT_VECTOR};

FgQcFoldType::FgQcFoldType(FgQcPlot *fgqc_plot) 
                         : FgQcPlotType(fgqc_plot)
{
  _uchar_grid = NULL;
  _uga        = NULL;
  _ugar       = NULL;
  _ds         = NULL;
  _dsp        = NULL;
  _tga        = NULL;
  _fga        = NULL;

  _cmp_list               = NULL;
  _live_fold_of_stack     = NULL;
  _cmp_trace_offsets      = NULL;
  _cmp_offset_pntrs       = NULL;
  _cmp_trace_azimuths     = NULL;
  _cmp_azimuth_pntrs      = NULL;

  _cmp_list_size = 0;
  _cmp_count     = 0;
  _total_traces  = 0;
  _max_live_fold = 0;

  _deg_per_radian = 90.0 / atan2(1,0);

  _veto_extended = False;
}


FgQcFoldType::~FgQcFoldType()
{
  if(_uchar_grid) delete _uchar_grid;
  if(_uga) delete _uga;
  if(_ugar) delete _ugar;
  if(_dsp) {
    delete _dsp;
    _qcp->getFgColorPop()->assignDistrSlicerPop (0);
  }
  if(_ds) delete _ds;
  if(_tga) delete _tga;
  if(_fga) delete _fga;  
  if(_cmp_list)               delete [] _cmp_list;
  if(_live_fold_of_stack)     delete [] _live_fold_of_stack;
  if(_cmp_trace_offsets)      delete [] _cmp_trace_offsets;
  if(_cmp_offset_pntrs)       delete [] _cmp_offset_pntrs;
  if(_cmp_trace_azimuths)     delete [] _cmp_trace_azimuths;
  if(_cmp_azimuth_pntrs)      delete [] _cmp_azimuth_pntrs;
}

//============================================================================
//====================== Find number of gathers  =============================
//============================================================================
int FgQcFoldType::computeNumGathers(float xmin, float xmax, 
                                    float ymin, float ymax)
{

  //make image area limits 10% larger than actual data limits so no points
  //will be excluded by rounding errors
  extendROI (&xmin, &xmax, &ymin, &ymax, 0.1); // the inputs are changed here

  long i;
  double xloc, yloc;
  FieldGeometry *fg = _qcp->fg();

  if (!createCmpList()) return (int)0;
  for(i=0; i<(long)_cmp_list_size; i++)
    {
    if(getCoordinateSystem() == SURVEYSYSTEM)
      fg->getCmpLocBinCenter(i, &xloc , &yloc );
    else
      fg->getCmpGridBinCenter(i, &xloc , &yloc );
    if(xloc >= xmin && xloc <= xmax && yloc >= ymin && yloc <= ymax &&
       xloc != DNIL && yloc != DNIL)
      {
        storeCmp (i);
      }
    if (_do_abort)
      {
      if (!(i % 5000))
	{
        if (_do_abort->userAbort())
	  {
          return (int)0;
          }
        }
      }
    }
  if (!sortCmpList()) return (int)0;
  return (int)1;
}
//============================================================================
//====================== Find number of data points  =========================
//============================================================================
void FgQcFoldType::computeNumPoints(float xmin, float xmax, 
                                    float ymin, float ymax)
{
long i, j, fold;
double xloc, yloc, azimuth;
float zval;
FieldGeometry *fg = _qcp->fg();

  //make image area limits 10% larger than actual data limits so no points
  //will be excluded by rounding errors
  extendROI (&xmin, &xmax, &ymin, &ymax, 0.1); // the inputs are changed here

  _num_points = 0;
  long count = 0;
  int num_gathers = (int)fg->numCmpGathers ();
  for(i=0; i<num_gathers; i++)
    {
    if(getCoordinateSystem() == SURVEYSYSTEM)
      fg->getCmpLocBinCenter(i, &xloc , &yloc );
    else
      fg->getCmpGridBinCenter(i, &xloc , &yloc );
    if(xloc >= xmin && xloc <= xmax && yloc >= ymin && yloc <= ymax &&
       xloc != DNIL && yloc != DNIL)
      {
      if(_qcp->_limit_offsets_azimuths == FOLD_PLOT)
        {
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
      else if(_qcp->_limit_offsets_azimuths == OFFSET_DISTRIBUTION_PLOT)
        {
        fold = grabLiveFoldOfStack (i);
        for(j = 0; j < fold; j++)
	  {
          zval = grabCmpTraceOffset(i,j);
          if(zval >= _qcp->_offset_min &&
            zval <= _qcp->_offset_max  &&
            zval != FNIL               )
            {
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
            j = fold; //in range so stop this for loop
            }
	  }
        }
      else /* if(_qcp->_limit_offsets_azimuths == AZIMUTH_DISTRIBUTION_PLOT) */
	{
        fold = (long)grabLiveFoldOfStack (i);
        for(j = 0; j < fold; j++)
	  {
          azimuth = grabCmpTraceAzimuth (i, j);
          if(azimuth >= (double)_qcp->_azimuth_min &&
             azimuth <= (double)_qcp->_azimuth_max &&
             azimuth != DNIL                      )
            {
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
             j = fold; // in range so stop this for loop
            }
	  }
        }
      }
    }
}

//============================================================================
//====================== When data has changed determine action ==============
//============================================================================
int FgQcFoldType::ValuesChanged(FieldGeometry * /*fg*/, long /*ixl*/,
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

//============================================================================
//====================== Get data for plot             =======================
//============================================================================
int FgQcFoldType::plot()
{
SeisPlot *sp = _qcp->sp();
FieldGeometry *fg = _qcp->fg();
SeisColorPop *color_pop = _qcp->getColorPop();
long i, j, k, index, fold;
int frame_num = 1, num_frames = 1; //may want movies later
int stat = True;
float zval, xrange, yrange;
double xperpix, yperpix;  
double xloc, yloc, azimuth;
float min_fold = 1.0;
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
  _float_grid = new FloatGrid((int)_qcp->getNumx(), (int)_qcp->getNumy(),
    _do_abort);
  if(!FgQcPlotType::checkGridStatus(CHECK_GRID))
    {
    if (_do_abort) _do_abort->actionComplete ();
    return(False);
    }

  if (!computeNumGathers(minx,maxx,miny,maxy))
    {
    if (_do_abort) _do_abort->actionComplete ();
    return(False);
    }
  if (!createLiveFoldOfStack()) {
// warn user of the invalid live fold of stack found (i.e. > 254)
    static char message[100];
    sprintf (message,
      "If the maximum live fold is really %d then contact a developer\n",
      _max_live_fold);
    new SLErrorPop (_qcp->gridError(), "Error", (const char *)message);
    if (_do_abort) _do_abort->actionComplete ();
    return(False);
  }
  if(_qcp->_limit_offsets_azimuths == OFFSET_DISTRIBUTION_PLOT) {
    if (!createCmpTraceOffsets())
       {
       if (_do_abort) _do_abort->actionComplete ();
       return(False);
       }
  }
  else if(_qcp->_limit_offsets_azimuths == AZIMUTH_DISTRIBUTION_PLOT) {
    if (!createCmpTraceAzimuths())
       {
       if (_do_abort) _do_abort->actionComplete ();
       return(False);
       }
  }

  if (_qcp->_limit_type == DYNAMIC_LIMITS && !_veto_extended)
    {
    int num_zval_ranges = 9; // empirically determined to be an upper limit
    if (!setFoldGathers(num_zval_ranges))
       {
       if (_do_abort) _do_abort->actionComplete ();
       return(False);
       }
    if (!gatherFolds(minx, maxx, miny, maxy))
       {
       if (_do_abort) _do_abort->actionComplete ();
       return(False);
       }
    }
  else
    {    
    _num_points = 0;
    computeNumPoints(minx, maxx, miny, maxy);
    if(!_num_points)
      {
      if (_do_abort) _do_abort->actionComplete ();
      return(False);
      }
    }

  sp->setMatchHeader(1);
  sp->setGridXYS(_minx, _maxx, _maxy, _miny);

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

  long count = 0;
  if (_qcp->_limit_type != DYNAMIC_LIMITS || _veto_extended)
    for(i=0;i<_qcp->getNumx()*_qcp->getNumy();i++)image_data[i] = _not_defined;

  xrange = _maxx - _minx;
  yrange = _miny - _maxy; //y is northing by default
  xperpix = xrange / (float)(_qcp->getNumx() - 1.0);
  yperpix = yrange / (float)(_qcp->getNumy() - 1.0);
  
  for(i=0;i<_qcp->getNumx();i++)
    sp->setHeader(i*sp->numHeaders() + 
                  sp->matchHeader()-1, xperpix * i + _minx);

  count = 0;
  if (_qcp->_limit_type != DYNAMIC_LIMITS || _veto_extended)
    {
    for(i=0; i<_qcp->getNumx()*_qcp->getNumy(); i++)
      {
      index = i / _qcp->getNumy();
      xloc  = index * xperpix + _minx;
      index = i - index * _qcp->getNumy();
      yloc  = index * yperpix + _maxy; 

      if(getCoordinateSystem() == SURVEYSYSTEM)
        index = fg->getMatchingCmp( xloc , yloc );
      else
        index = fg->getMatchingCmpUsingGrid( xloc , yloc );

      if(index != -1)
        {
        if(_qcp->_limit_offsets_azimuths == FOLD_PLOT)
          {
          image_data[i] =  zval = (float)grabLiveFoldOfStack(index);
          _minz = _minz > zval ? zval : _minz;
          _maxz = _maxz < zval ? zval : _maxz;    
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
        else if(_qcp->_limit_offsets_azimuths == OFFSET_DISTRIBUTION_PLOT)
          {
            fold = (long)grabLiveFoldOfStack (index);
            for(j = 0, k = 0; j < fold; j++)
	      {
              zval = grabCmpTraceOffset (index, j);
              if(zval >= _qcp->_offset_min &&
                 zval <= _qcp->_offset_max &&
                 zval != FNIL              )
                ++k;
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
            if(k)
              {
              image_data[i]  = k;
              _minz = _minz > k ? k : _minz;
              _maxz = _maxz < k ? k : _maxz;
              }
          }
        else /*if(_qcp->_limit_offsets_azimuths == AZIMUTH_DISTRIBUTION_PLOT)*/
	  {
          fold = grabLiveFoldOfStack(index);
          for(j = 0, k = 0; j < fold; j++)
	    {
            azimuth = grabCmpTraceAzimuth (index, j);
            if(azimuth >= _qcp->_azimuth_min &&
               azimuth <= _qcp->_azimuth_max &&
               azimuth != DNIL              )
              ++k;
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
          if(k)
            {
            image_data[i] = k;
            _minz = _minz > k ? k : _minz;
            _maxz = _maxz < k ? k : _maxz;
            }
          }
        }//end index != -1   
      }//end for i

    if(_maxz < min_fold) 
       {
       if (_do_abort) _do_abort->actionComplete ();
       return(False);
       }
    if(_minz == _not_defined) _minz += .00001;
    if(_minz >= _maxz) _maxz += 00001;

    
    _qcp->setMinx(_minx);
    _qcp->setMaxx(_maxx);
    _qcp->setMiny(_miny);
    _qcp->setMaxy(_maxy);
    _qcp->setMinz(_minz);
    _qcp->setMaxz(_maxz);

    _float_grid->setRange(_not_defined, MaximumValue(_minz,min_fold), _maxz);
    if(!FgQcPlotType::checkGridStatus(CHECK_GRID))
      {
      if (_do_abort) _do_abort->actionComplete ();
      return(False);
      }
    }
  else//DYNAMIC LIMITS
    {
    int col1 = _uga->getGridZ(_uga->ROIMinimumZ());
    int row1 = _uga->getGridX(_uga->ROIMinimumX());
    int coln = _uga->getGridZ(_uga->ROIMaximumZ());
    int rowm = _uga->getGridX(_uga->ROIMaximumX());
    _image_ncols = MaximumValue(col1,coln) - MinimumValue(col1,coln) + 1;
    _image_nrows = MaximumValue(row1,rowm) - MinimumValue(row1,rowm) + 1;
    _image_col1  = MinimumValue(col1,coln);
    _image_row1  = MinimumValue(row1,rowm);
    sp->drawColorBarOnHardCopy(SeisPlot::Off);
    }
  _qcp->setReach(1.0 ,1.0);
 

  sp->setSymetricalAnnotation(_minx,_maxx,_miny,_maxy);
  sp->setDrawXlines(False);
  sp->setDrawYlines(False);
  sp->setGridWidth(_qcp->getPlotWidth());
  sp->setGridHeight(_qcp->getPlotHeight());
  sp->setNorm(PlotImage::EXTERNALNORM);
  sp->setExternalAmp(MaximumValue(_maxz,_minz));

  if(!color_pop->beenManaged() || _qcp->getResetColors())
    {
    if(_minz == _maxz) _maxz += .00001;
    if(_qcp->_limit_type != DYNAMIC_LIMITS || _veto_extended) 
         sp->setPreDefColor(PlotImage::STANDARD);
    sp->setDoAmplitude(True);
    sp->setDoPercent(False);
    sp->setGradeVert(False);
    sp->setGradeHorz(False);
    if(_qcp->_limit_type != DYNAMIC_LIMITS || _veto_extended) {
      sp->setMinColorAmp(0.0);
      sp->setMaxColorAmp(_maxz);
    }
    else /* if(_qcp->_limit_type == DYNAMIC_LIMITS && !_veto_extended) */ {
      sp->setMinColorAmp(_dsp->getMinimumCode());
      sp->setMaxColorAmp(_dsp->getMaximumCode());
    }
    color_pop->presetAmplitudes (0.0, _maxz);
    color_pop->presetAmpType(True);
    color_pop->presetGrading(False);
    }


  _float_grid->resetExtremaReadyFlag (); // data in the grid changed

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

  if(stat && _qcp->getNumColors() && _qcp->_limit_type != DYNAMIC_LIMITS &&
    !_veto_extended) 
    {
    _hill_shader = new HillShader(_qcp->getNumColors());
    if(!FgQcPlotType::checkGridStatus(CHECK_SHADER))
      {
      if (_do_abort) _do_abort->actionComplete ();
      return(False);
      }
    }

  if(stat &! _qcp->inApplicationWindow())
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
int FgQcFoldType::editData()
{
long index = 0;
long first_col, last_col, first_row, last_row;
long tempval;
double xperpix, yperpix, xrange, yrange;
long i, j, k, fold, num_found = 0;
double xloc, yloc, azimuth;
float zval;
const float *hd = _qcp->sp()->firstDisplayedHeaderData();
FieldGeometry *fg = _qcp->fg();
SeisPlot *sp = _qcp->sp();
float *image_data = _float_grid->getArray ();
float minx = MinimumValue(_qcp->getPostMinx(),_qcp->getPostMaxx());
float maxx = MaximumValue(_qcp->getPostMinx(),_qcp->getPostMaxx());
float miny = MinimumValue(_qcp->getPostMiny(),_qcp->getPostMaxy());
float maxy = MaximumValue(_qcp->getPostMiny(),_qcp->getPostMaxy());
long first_index, last_index; 


  if (_do_abort) _do_abort->setNewAction ();
  xrange    = sp->plottedGridX2() - sp->plottedGridX1();
  xperpix   = xrange / (float)(_qcp->getNumx() - 1.0);
  yrange    = sp->plottedGridX1() - sp->plottedGridX2();
  yperpix   = yrange / (float)(_qcp->getNumy() - 1.0);
  first_col = (long)( (minx - sp->plottedGridX1()) / xperpix);
  last_col  = (long)( (maxx - sp->plottedGridX1()) / xperpix);
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

  first_index = MinimumValue(first_col*_qcp->getNumy(),first_row*_qcp->getNumx());
  last_index  = MaximumValue(last_col*_qcp->getNumy(),last_row*_qcp->getNumx());

  long count = 0;
  if(_qcp->_limit_type != DYNAMIC_LIMITS || _veto_extended)
    {
    computeNumPoints(MinimumValue(minx,maxx), MaximumValue(maxx,minx), 
                     MinimumValue(miny,maxy), MaximumValue(maxy,miny));
    for(i=first_index; i<last_index; i++)
      {
      index = i / _qcp->getNumy();
      xloc  = index * xperpix + minx;
      index = i - index * _qcp->getNumy();
      yloc  = index * yperpix + maxy; 

      if(getCoordinateSystem() == SURVEYSYSTEM)
        index = fg->getMatchingCmp( xloc , yloc );
      else
        index = fg->getMatchingCmpUsingGrid( xloc , yloc );

      if(index != -1)
        {
        ++num_found;
        if(_qcp->_limit_offsets_azimuths == FOLD_PLOT)
          {
          image_data[i] = zval = (float)grabLiveFoldOfStack(index);
          _minz = _minz > zval ? zval : _minz;
          _maxz = _maxz < zval ? zval : _maxz;
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
        else if(_qcp->_limit_offsets_azimuths == OFFSET_DISTRIBUTION_PLOT)
          {
          fold = (long)grabLiveFoldOfStack (index);
	  for(j = 0, k = 0; j < fold; j++)
	    {
            zval = grabCmpTraceOffset (index, j);
            if(zval >= _qcp->_offset_min &&
               zval <= _qcp->_offset_max &&
               zval != FNIL              )
              ++k;
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
          if(k)
            {
            image_data[i]  = k;
            _minz = _minz > k ? k : _minz;
            _maxz = _maxz < k ? k : _maxz;
            }
          }
        else /*if(_qcp->_limit_offsets_azimuths == AZIMUTH_DISTRIBUTION_PLOT)*/
	  {
          fold = grabLiveFoldOfStack (index);
          for(j = 0, k = 0; j < fold; j++)
	    {
            azimuth = grabCmpTraceAzimuth (index, j);
            if(azimuth >= _qcp->_azimuth_min &&
              azimuth <= _qcp->_azimuth_max &&
              azimuth != DNIL              )
              ++k;
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
          if(k)
            {
            image_data[i] = k;
            _minz = _minz > k ? k : _minz;
            _maxz = _maxz < k ? k : _maxz;
            }
          }
        }//end index != 1
      }//end for i


    if(!num_found || _num_points != num_found )
      {
      if (_do_abort) _do_abort->actionComplete ();
      return(False);
      }
    }
  else
    if (!gatherFolds(minx,maxx,maxy,miny))
      {
      if (_do_abort) _do_abort->actionComplete ();
      return(False);
      }



  _float_grid->resetExtremaReadyFlag (); // data in the grid changed
  sp->modArrayTypeImage( first_col, last_col, first_row, last_row,
                         sp->currentFrame());


  if (_do_abort) _do_abort->actionComplete ();
  return(True);

}

// talk to Tom about getting aconvenience routine in "geom/field_geometry.hh"
double FgQcFoldType::getCmpTraceAzimuth (long ixcmp, long ixfold)
{
// Previously, a compass coordinate system had been used such that...
//   azimuth is zero at noon, 90 at 3:00, 180 at 6:00, 270 at 9:00... (i.e. CW)
//   where the direction is from the receiver to the source
// However, now a Cartesian RHS is being used such that...
//   azimuth is zero at 3:00, 90 at noon, 180 at 9:00, 270 at 6:00... (i.e. CCW)
//   where the direction is FROM the source to the receiver to be compatible
//   with Tom Stoekley's calculations
  FieldGeometry *fg = _qcp->fg ();
  long trindx = fg->originalTraceIndex (ixcmp, ixfold);
  if (!fg->calculateHeaderWords (trindx+1, 0)) {
    double x_source, y_source, x_receiver, y_receiver;
// always use survey system to compute azimuth
    x_source = fg->getHeaderWordValue (XLOCSOURCE);
    if (x_source == DNIL) return DNIL;
    y_source = fg->getHeaderWordValue (YLOCSOURCE);
    if (y_source == DNIL) return DNIL;
    x_receiver = fg->getHeaderWordValue (XLOCRECEIVER);
    if (x_receiver == DNIL) return DNIL;
    y_receiver = fg->getHeaderWordValue (YLOCRECEIVER);
    if (x_receiver == DNIL) return DNIL;
    if( fg->getHeaderWordValue(XOFFSET) < _qcp->_offset_min ||
        fg->getHeaderWordValue(XOFFSET) > _qcp->_offset_max   )
      return DNIL;


    double dx = x_receiver - x_source;
    double dy = y_receiver - y_source;
//  double theta = 90.0 - (atan2 (dy, dx) * _deg_per_radian); // like a compass
    double theta = atan2 (dx, dy) * _deg_per_radian; // Cartesian RHS
    theta = theta > 0.0 ? theta : 360.0 + theta;
    return theta;
  }
  else
    return DNIL;
}

int FgQcFoldType::setFoldGathers (int num_zval_ranges)
{
  FieldGeometry *fg = _qcp->fg ();
  if (num_zval_ranges < 2) num_zval_ranges = 2;
  if (_uchar_grid) delete _uchar_grid, _uchar_grid = 0;

// get the user specified region of interest
  float user_left = _qcp->getUserLeft();
  float user_right = _qcp->getUserRight();
  float user_top = _qcp->getUserTop();
  float user_bottom = _qcp->getUserBottom();

// determine how big to make the unsigned char array.  it will be created
//   in the Grid system to be inclusive of every potentially displayed grid
  findGridROI (user_left, user_right, user_top, user_bottom);

// store the X-Y-grid extrema
  _user_xgmin = _xgmin;
  _user_xgmax = _xgmax;
  _user_ygmin = _ygmin;
  _user_ygmax = _ygmax;

  if (getCoordinateSystem() == SURVEYSYSTEM)
    findLocBinData (user_left, user_right, user_top, user_bottom);

// find number of grids
  int num_x_grids = _user_xgmax - _user_xgmin + 1;
  int num_y_grids = _user_ygmax - _user_ygmin + 1;

// zero the unsigned char grid
  _uchar_grid = new UCharGrid (num_y_grids, num_zval_ranges, num_x_grids,
    _do_abort);
  if(!checkGridStatus(CHECK_UCGRID)) return(False);

// create a unsigned char grid accessor object for the input 3D grid
  if (_uga) delete _uga, _uga = 0;
  _uga = new UCharGridAccessor ();
  if(!checkGridStatus(CHECK_UGACCESS)) return(False);
  _uga->specifyData (_uchar_grid);
  if(!checkGridStatus(CHECK_UGACCESS)) return(False);

// the cube's coordinate system has its origin at the upper,right,front corner
//   hence, all minimum arguments are greater than maximum arguments in the
//   _uga->set*BinData calls
// note also:  the Y-bin coordinates are used as the attribute dimension
//             this is because it is known that the Y-coordinate changes
//             fastest.  The NTree compression class requires this.
//             the X-bin coordinates are used as the Y-dimension
//             and the Z-bin coordinates are used as the X-dimension
//             this is because it is known that the Y-X plane face on the
//             cube is organized analogously to the X-Z plane face on the
//             cube in terms of the order in which the coordinates change

// set up the X-bin coordinates:  They are really the Y-grid coordinates
  _uga->setXBinData((float)_ygmax, (float)_ygmin);
  if(!checkGridStatus(CHECK_UGACCESS)) return(False);

// set up the Y-bin coordinates:  They are really the attribute coordinates
  if (_qcp->_limit_offsets_azimuths == OFFSET_DISTRIBUTION_PLOT) {
    _min_zval = fg->getMinimumOffset ();
    _max_zval = fg->getMaximumOffset ();
  }
  else /*if (_qcp->_limit_offsets_azimuths == AZIMUTH_DISTRIBUTION_PLOT)*/ {
    _min_zval =   (float)0;
    _max_zval = (float)360;
  }
  _uga->setYBinData(_max_zval,_min_zval);
  if(!checkGridStatus(CHECK_UGACCESS)) return(False);

// set up the Z-bin coordinates:  They are really the X-grid coordinates
  _uga->setZBinData((float)_xgmax, (float)_xgmin);
  if(!checkGridStatus(CHECK_UGACCESS)) return(False);

  _uchar_clear = 1;
  return True;
}

int FgQcFoldType::gatherFolds (float xmin, float xmax, float ymin, float ymax)
{
FieldGeometry *fg = _qcp->fg();
long i, j;
double xgrid, ygrid;

  if (!_uchar_grid || !_uga) return False;

// find grid region of interest
  findGridROI (xmin, xmax, ymin, ymax);

// zero out the area of interest in the unsigned char grid
  int first_time = _uchar_clear;
  if (!_uchar_clear) {
    if (!_uchar_grid->setSubStart (_uga->getGridX((float)_ygmax),
                                   _uga->getGridY(_max_zval),
                                   _uga->getGridZ((float)_xgmax))) return False;

    if (!_uchar_grid->setSubSize (_uga->getGridX((float)_ygmin)-
                                  _uga->getGridX((float)_ygmax)+1,
                                  _uga->getGridY(_min_zval)
                                  -_uga->getGridY(_max_zval)+1,
                                  _uga->getGridZ((float)_xgmin)-
                                  _uga->getGridZ((float)_xgmax)+1
                                 )) return False;
    if (!_uchar_grid->fill((float)0)) return False;
  }

  long fold;
  float zval;
  double azimuth;

  _uga->startROIWatch ();

  long count = 0;
// analyze the grid region of interest
  int num_gathers = (int)fg->numCmpGathers ();
  for(i=0; i<num_gathers; i++)
    {
    fg->getCmpGridBinCenter(i, &xgrid , &ygrid );
    if(xgrid >= (double)_xgmin && xgrid <= (double)_xgmax  && 
       ygrid >= (double)_ygmin && ygrid <= (double)_ygmax  &&
       xgrid != DNIL && ygrid != DNIL)
      {
      fold = grabLiveFoldOfStack (i);
      if(_qcp->_limit_offsets_azimuths == OFFSET_DISTRIBUTION_PLOT)
        {
        for(j = 0; j < fold; j++)
	  {
          zval = grabCmpTraceOffset (i, j);
          if (zval != FNIL)
            _uga->accumulateXYZ (1.0, (float)ygrid, zval, (float)xgrid);
          if (_do_abort)
	    {
            count++;
            if (!(count % 5000))
	      {
              if (_do_abort->userAbort())
	        {
                return False;
                }
              }
            }
          }
        }
      else /* if(_qcp->_limit_offsets_azimuths == AZIMUTH_DISTRIBUTION_PLOT) */
	{
        for(j = 0; j < fold; j++)
	  {
          azimuth = grabCmpTraceAzimuth (i, j);
          if (azimuth != DNIL)
            _uga->accumulateXYZ (1.0, (float)ygrid, (float)azimuth,
              (float)xgrid);
          if (_do_abort)
	    {
            count++;
            if (!(count % 5000))
	      {
              if (_do_abort->userAbort())
	        {
                return False;
                }
              }
            }
          }
        }
      }
    }

// after accumulating the fold numbers, quantize the results
  _uchar_grid->resetExtremaReadyFlag (); // data in the grid changed
  if (_uchar_clear) {
    if (!quantizeFoldsInitially()) return False;
    if (getCoordinateSystem() == SURVEYSYSTEM) {
      findLocROI (_uga->ROIMinimumZ (), _uga->ROIMaximumZ(),
                  _uga->ROIMinimumX (), _uga->ROIMaximumX());
      _minx = _xlmin;
      _maxx = _xlmax;
      _miny = _ylmin;
      _maxy = _ylmax;
    }
    else /* if (getCoordinateSystem() == GRIDSYSTEM) */ {
      _minx = _uga->ROIMinimumZ ();
      _maxx = _uga->ROIMaximumZ ();
      _miny = _uga->ROIMinimumX ();
      _maxy = _uga->ROIMaximumX ();
    }
    _minz = (float)0;
    _maxz = (float)_max_live_fold;
    _qcp->setMinx(_minx);
    _qcp->setMaxx(_maxx);
    _qcp->setMiny(_miny);
    _qcp->setMaxy(_maxy);
    _qcp->setMinz(_minz);
    _qcp->setMaxz(_maxz);
    _dsp = new DistrSlicerPop (_qcp->plotWidget(), "distr_slicer_pop",
      _ds, _qcp->sp(), _qcp->hctx());
    if(!checkGridStatus(CHECK_DSPOP)) return(False);
    _dsp->initialize ();
    if(!checkGridStatus(CHECK_DSPOP)) return(False);
    _qcp->getFgColorPop()->assignDistrSlicerPop (_dsp);
    _dsp->makeAndManage ();
    _uchar_clear = 0;
  }
  else {
    if (!quantizeFoldsSubsequently()) return False;
  }

  int num_x_grids, num_y_grids;
  float user_left, user_right, user_top, user_bottom;
  if (_temp_grid) delete _temp_grid, _temp_grid = 0;
  count = 0;
  if (getCoordinateSystem() == SURVEYSYSTEM) {

// get the user specified region of interest
    user_left = _qcp->getUserLeft();
    user_right = _qcp->getUserRight();
    user_top = _qcp->getUserTop();
    user_bottom = _qcp->getUserBottom();

// set up a temporary grid that will hold the rotated analyzed grid data
//   formatted in survey coordinates
    _temp_grid = new FloatGrid (_num_x_loc_bins, _num_y_loc_bins, _do_abort);
    if(!FgQcPlotType::checkGridStatus(CHECK_GRID)) return(False);

    float *temp_array = _temp_grid->getArray();
    if(!FgQcPlotType::checkGridStatus(CHECK_GRID)) return(False);

// set up to access the temporary survey data grid
    _tga = new FloatGridAccessor ();
    if(!checkGridStatus(CHECK_TGACCESS)) return(False);

    _tga->specifyData (_temp_grid);
    if(!checkGridStatus(CHECK_TGACCESS)) return(False);

    _tga->setXBinData (user_left, user_right);
    if(!checkGridStatus(CHECK_TGACCESS)) return(False);

    _tga->setYBinData (user_top, user_bottom);
    if(!checkGridStatus(CHECK_TGACCESS)) return(False);

// if this is not the first time, copy from current resultant _float_grid
//   into the temporary array.  this insures that any pixels outside of the
//   irregular rotation boundaries maintain the current result
    if (!first_time) {
// set up to access the _float_grid
      _fga = new FloatGridAccessor ();
      if(!checkGridStatus(CHECK_FGACCESS)) return(False);

      _fga->specifyData (_float_grid);
      if(!checkGridStatus(CHECK_FGACCESS)) return(False);

      _fga->setXBinData (user_left, user_right);
      if(!checkGridStatus(CHECK_FGACCESS)) return(False);

      _fga->setYBinData (user_top, user_bottom);
      if(!checkGridStatus(CHECK_FGACCESS)) return(False);

      float *float_array = _float_grid->getArray ();
      long temp_size = _temp_grid->getArraySize ();
      long index;
      for (index = 0; index < temp_size; index++)
        temp_array[index] = float_array[
          _float_grid->getIndex
            (_fga->getGridX(_tga->getX(_temp_grid->getX(index))),
             _fga->getGridY(_tga->getY(_temp_grid->getY(index))))];
    }
    else first_time = 0;

// get the resultant distribution slicer grid object
    UCharGrid *result_grid = _ds->getResult ();

// get the resultant distribution slicer array
    unsigned char *result_array = _ds->getArray ();
    if(!checkGridStatus(CHECK_SLICER)) return(False);

// rotate the unsigned char array into the temp array
    long temp_size = _temp_grid->getArraySize ();
    long index;
    float x, y, xgrid, ygrid;
    for (index = 0; index < temp_size; index++) {

// find the survey (x,y) given the temporary grid index
      x = _tga->getX(_temp_grid->getX(index));
      y = _tga->getY(_temp_grid->getY(index));

// find the grid (xgrid, ygrid) given the survey (x,y)
      xgrid = (float)fg->getXgridCoord ((double)x, (double)y);
      ygrid = (float)fg->getYgridCoord ((double)x, (double)y);

// store the resultant grid value at an index defined by (xgrid, ygrid) into
//   the temporary array at the given index
      temp_array[index] = (float)result_array[
        result_grid->getIndex(_ugar->getGridX(xgrid),_ugar->getGridY(ygrid))];
      if (_do_abort) {
        count++;
        if (!(count % 5000)) {
          if (_do_abort->userAbort()) {
            return False;
          }
        }
      }
    }
  }
  else /* if (getCoordinateSystem() == GRIDSYSTEM) */ {
    num_x_grids = _user_xgmax - _user_xgmin + 1;
    num_y_grids = _user_ygmax - _user_ygmin + 1;

// copy analyzed grid data into a temporary float grid
    _temp_grid = new FloatGrid (num_x_grids, num_y_grids, _do_abort);
    if(!FgQcPlotType::checkGridStatus(CHECK_GRID)) return(False);

    float *temp_array = _temp_grid->getArray();
    if(!FgQcPlotType::checkGridStatus(CHECK_GRID)) return(False);

    unsigned char *result_array = _ds->getArray ();
    if(!checkGridStatus(CHECK_SLICER)) return(False);

// equivalence the resultant unsigned char array to the float array
    long index, temp_size = _temp_grid->getArraySize ();
    for (index = 0; index < temp_size; index++)
      temp_array[index] = (float)result_array[index];
  }

  if (_uchar_clear) _uchar_clear = 0;

// use NN resampling to enlarge the temporary float grid to the correct size
  _temp_grid->resample (_float_grid, (int)1);
  if(!FgQcPlotType::checkGridStatus(CHECK_GRID)) return(False);

  delete _temp_grid, _temp_grid = 0;
  if (_tga) delete _tga, _tga = 0;
  if (_fga) delete _fga, _fga = 0;

  return True;
}

int FgQcFoldType::quantizeFoldsInitially ()
{
  if (!_uchar_grid) return False;
// quantize the accumulated folds using the distribution slicer
  if (_ds) delete _ds, _ds = 0;

  if(_qcp->_limit_offsets_azimuths == AZIMUTH_DISTRIBUTION_PLOT)
    _ds = new DistributionSlicer (_qcp->getNumColors(), _min_zval, _max_zval,
                             (int)0, _max_live_fold, &(_qcp->_azimuth_max),
                             &(_qcp->_azimuth_min));
  else
    _ds = new DistributionSlicer (_qcp->getNumColors(), _min_zval, _max_zval,
                             (int)0, _max_live_fold, &(_qcp->_offset_max),
                             &(_qcp->_offset_min));


  if(!checkGridStatus(CHECK_SLICER)) return(False);
  if(_qcp->_limit_offsets_azimuths == OFFSET_DISTRIBUTION_PLOT)
    _ds->setValueDisposition (AVERAGE);
  else /* if (_qcp->_limit_offsets_azimuths == AZIMUTH_DISTRIBUTION_PLOT) */ {
//    _ds->setValueDisposition (SELECT);
//    _ds->setInsertVectorFlag (ALLOW_INSERT_VECTOR);
    _ds->setValueDisposition (AVERAGE);
  }
  _ds->initialize (_uchar_grid);
  if(!checkGridStatus(CHECK_SLICER,1)) return(False);

  if (getCoordinateSystem() == SURVEYSYSTEM) {
// create an unsigned char grid accessor object for the resultant grid
    findGridROI (_qcp->getUserLeft(), _qcp->getUserRight(),
                 _qcp->getUserTop(), _qcp->getUserBottom());
    if (_ugar) delete _ugar, _ugar = 0;
    _ugar = new UCharGridAccessor ();
    if(!checkGridStatus(CHECK_UGRACCESS)) return(False);

    _ugar->specifyData (_ds->getResult());
    if(!checkGridStatus(CHECK_UGRACCESS)) return(False);

    _ugar->setXBinData ((float)_xgmin, (float)_xgmax);
    if(!checkGridStatus(CHECK_UGRACCESS)) return(False);

    _ugar->setYBinData ((float)_ygmin, (float)_ygmax);
    if(!checkGridStatus(CHECK_UGRACCESS)) return(False);
  }

  return True;
}

int FgQcFoldType::quantizeFoldsSubsequently ()
{
  if (!_ds) return False;
// quantize the accumulated folds using the distribution slicer
  _ds->quantizeBinCounts ();
  if(!checkGridStatus(CHECK_SLICER)) return(False);
  return True;
}

Boolean FgQcFoldType::checkGridStatus( int check_type, int dont_post )
{
Boolean stat;

  switch(check_type)
    {
     case CHECK_SLICER:
       if (_ds)
	 {
         if(_ds->failed())
           {
           if (dont_post)
             _qcp->_grid_error = False;
           else
             new GridErrorHandler(_qcp->gridError(),"Error",
                                  _ds->errorStatus());
           delete _ds;
           _ds = NULL;
           stat = False;
           }
         else
           {
           stat = True;
           }
         }
       else
	 {
         if (dont_post)
           _qcp->_grid_error = False;
         else
           new SLErrorPop (_qcp->gridError(),"Error",
                           "Distribution Slicer object not created");
         stat = False;
         }
       break;

     case CHECK_UCGRID:
       if (_uchar_grid)
	 {
         if(_uchar_grid->failed())
           {
           if (dont_post)
             _qcp->_grid_error = False;
           else
             new GridErrorHandler(_qcp->gridError(),"Error",
                                  _uchar_grid->errorStatus());
           delete _uchar_grid;
           _uchar_grid = NULL;
           stat = False;
           }
         else
           {
           stat = True;
           }
         }
       else
	 {
         if (dont_post)
           _qcp->_grid_error = False;
         else
           new SLErrorPop (_qcp->gridError(),"Error",
                           "Unsigned Char Grid object not created");
         stat = False;
         }
       break;

     case CHECK_UGACCESS:
       if (_uga)
	 {
         if(_uga->failed())
           {
           if (dont_post)
             _qcp->_grid_error = False;
           else
             new GridErrorHandler(_qcp->gridError(),"Error",
                                  _uga->errorStatus());
           delete _uga;
           _uga = NULL;
           stat = False;
           }
         else
           {
           stat = True;
           }
         }
       else
	 {
         if (dont_post)
           _qcp->_grid_error = False;
         else
           new SLErrorPop (_qcp->gridError(),"Error",
                           "UChar Grid Accessor object not created");
         stat = False;
         }
       break;

     case CHECK_DSPOP:
       if (_dsp)
	 {
         if(_dsp->failed())
           {
           if (dont_post)
             _qcp->_grid_error = False;
           else
             new GridErrorHandler(_qcp->gridError(),"Error",
                                  _dsp->errorStatus());
           delete _dsp;
           _dsp = NULL;
           stat = False;
           }
         else
           {
           stat = True;
           }
         }
       else
	 {
         if (dont_post)
           _qcp->_grid_error = False;
         else
           new SLErrorPop (_qcp->gridError(),"Error",
                           "Distribution Slicer Popup object not created");
         stat = False;
         }
       break;

     case CHECK_FGACCESS:
       if (_fga)
         {
         if(_fga->failed())
           {
           if (dont_post)
             _qcp->_grid_error = False;
           else
             new GridErrorHandler(_qcp->gridError(),"Error",
                                  _fga->errorStatus());
           delete _fga;
           _fga = NULL;
           stat = False;
           }
         else
	   {
           stat = True;
           }
         }
       else
	 {
         if (dont_post)
           _qcp->_grid_error = False;
         else
           new SLErrorPop (_qcp->gridError(),"Error",
                           "Float Grid Accessor object not created");
         stat = False;
         }
       break;

     case CHECK_TGACCESS:
       if (_tga)
         {
         if(_tga->failed())
           {
           if (dont_post)
             _qcp->_grid_error = False;
           else
             new GridErrorHandler(_qcp->gridError(),"Error",
                                  _tga->errorStatus());
           delete _tga;
           _tga = NULL;
           stat = False;
           }
         else
	   {
           stat = True;
           }
         }
       else
	 {
         if (dont_post)
           _qcp->_grid_error = False;
         else
           new SLErrorPop (_qcp->gridError(),"Error",
                           "Temporary Grid Accessor object not created");
         stat = False;
         }
       break;

     case CHECK_UGRACCESS:
       if (_ugar)
	 {
         if(_ugar->failed())
           {
           if (dont_post)
             _qcp->_grid_error = False;
           else
             new GridErrorHandler(_qcp->gridError(),"Error",
                                  _ugar->errorStatus());
           delete _ugar;
           _ugar = NULL;
           stat = False;
           }
         else
           {
           stat = True;
           }
         }
       else
	 {
         if (dont_post)
           _qcp->_grid_error = False;
         else
           new SLErrorPop (_qcp->gridError(),"Error",
                           "UChar Grid Accessor object not created");
         stat = False;
         }
       break;


    }


     

  return(stat);

}

void FgQcFoldType::extendROI (float *xmin, float *xmax, float *ymin,
  float *ymax, float amount)
{
  _minx = *xmin;
  _miny = *ymin;
  _maxx = *xmax;
  _maxy = *ymax;

// extend region of interest limits by given amount
  amount /= 2.0;
  float add_amount = amount * (_maxx - _minx);
  _minx -= add_amount;
  _maxx += add_amount;
  add_amount = amount * (_maxy - _miny);
  _miny -= add_amount;
  _maxy += add_amount;
}

void FgQcFoldType::findGridROI (float xmin, float xmax, float ymin, float ymax)
{
  FieldGeometry *fg = _qcp->fg();
// find the extrema in the Grid system
  float x_grid_min, x_grid_max, y_grid_min, y_grid_max;
  if (getCoordinateSystem() == SURVEYSYSTEM) {
    float x1 = (float)fg->getXgridCoord ((double)xmin, (double)ymin);
    float x2 = (float)fg->getXgridCoord ((double)xmin, (double)ymax);
    float x3 = (float)fg->getXgridCoord ((double)xmax, (double)ymin);
    float x4 = (float)fg->getXgridCoord ((double)xmax, (double)ymax);
    x_grid_min = MinimumValue(MinimumValue(MinimumValue(x1,x2),x3),x4);
    x_grid_max = MaximumValue(MaximumValue(MaximumValue(x1,x2),x3),x4);

    float y1 = (float)fg->getYgridCoord ((double)xmin, (double)ymin);
    float y2 = (float)fg->getYgridCoord ((double)xmin, (double)ymax);
    float y3 = (float)fg->getYgridCoord ((double)xmax, (double)ymin);
    float y4 = (float)fg->getYgridCoord ((double)xmax, (double)ymax);
    y_grid_min = MinimumValue(MinimumValue(MinimumValue(y1,y2),y3),y4);
    y_grid_max = MaximumValue(MaximumValue(MaximumValue(y1,y2),y3),y4);
  }
  else /* if (getCoordinateSystem() == GRIDSYSTEM) */ {
    x_grid_min = MinimumValue(xmin,xmax);
    x_grid_max = MaximumValue(xmin,xmax);
    y_grid_min = MinimumValue(ymin,ymax);
    y_grid_max = MaximumValue(ymin,ymax);
  }

// round to nearest whole grid
  if (x_grid_min < 0)
    _xgmin = (int)(x_grid_min - 0.5);
  else
    _xgmin = (int)(x_grid_min + 0.5);

  if (x_grid_max < 0)
    _xgmax = (int)(x_grid_max - 0.5);
  else
    _xgmax = (int)(x_grid_max + 0.5);

  if (y_grid_min < 0)
    _ygmin = (int)(y_grid_min - 0.5);
  else
    _ygmin = (int)(y_grid_min + 0.5);

  if (y_grid_max < 0)
    _ygmax = (int)(y_grid_max - 0.5);
  else
    _ygmax = (int)(y_grid_max + 0.5);
}

// based on grid size and given survey region of interest, determine survey
//   bin sizes
void FgQcFoldType::findLocBinData (float xmin, float xmax, float ymin,
  float ymax)
{
  FieldGeometry *fg = _qcp->fg();
// determine the size in survey coordinates of a grid bin
  float x1 = (float)fg->getXlocCoord ((double)0, (double)0);
  float x2 = (float)fg->getXlocCoord ((double)0, (double)1);
  float x3 = (float)fg->getXlocCoord ((double)1, (double)0);
  float x4 = (float)fg->getXlocCoord ((double)1, (double)1);
  float xbmin = MinimumValue(MinimumValue(MinimumValue(x1,x2),x3),x4);
  float xbmax = MaximumValue(MaximumValue(MaximumValue(x1,x2),x3),x4);
  float dx = xbmax - xbmin;  // first approximation of survey bin size in X

  float y1 = (float)fg->getYlocCoord ((double)0, (double)0);
  float y2 = (float)fg->getYlocCoord ((double)0, (double)1);
  float y3 = (float)fg->getYlocCoord ((double)1, (double)0);
  float y4 = (float)fg->getYlocCoord ((double)1, (double)1);
  float ybmin = MinimumValue(MinimumValue(MinimumValue(y1,y2),y3),y4);
  float ybmax = MaximumValue(MaximumValue(MaximumValue(y1,y2),y3),y4);
  float dy = ybmax - ybmin; // first approximation of survey bin size in Y

  float xlmin = MinimumValue(xmin,xmax);
  float xlmax = MaximumValue(xmin,xmax);
  float ylmin = MinimumValue(ymin,ymax);
  float ylmax = MaximumValue(ymin,ymax);

// assume extrema are center points, determine the number of bins to use 
  _num_x_loc_bins = (int)((xlmax - xlmin) / dx + 1.5);
  _num_y_loc_bins = (int)((ylmax - ylmin) / dy + 1.5);

// based on the number of bins and assuming that the extrema are exact center
//   points, determine the exact survey bin size
  _xperbin = (xlmax - xlmin) / (_num_x_loc_bins - 1);
  if (xmin > xmax) _xperbin *= -1;
  _yperbin = (ylmax - ylmin) / (_num_y_loc_bins - 1);
  if (ymin > ymax) _yperbin *= -1;
}

// given grid extrema coordinates, find survey extrema coordinates
void FgQcFoldType::findLocROI (float xgmin, float xgmax, float ygmin,
  float ygmax)
{
  FieldGeometry *fg = _qcp->fg();
// find the extrema in the survey location system
  float x1 = (float)fg->getXlocCoord ((double)xgmin, (double)ygmin);
  float x2 = (float)fg->getXlocCoord ((double)xgmin, (double)ygmax);
  float x3 = (float)fg->getXlocCoord ((double)xgmax, (double)ygmin);
  float x4 = (float)fg->getXlocCoord ((double)xgmax, (double)ygmax);
  _xlmin = MinimumValue(MinimumValue(MinimumValue(x1,x2),x3),x4);
  _xlmax = MaximumValue(MaximumValue(MaximumValue(x1,x2),x3),x4);

  float y1 = (float)fg->getYlocCoord ((double)xgmin, (double)ygmin);
  float y2 = (float)fg->getYlocCoord ((double)xgmin, (double)ygmax);
  float y3 = (float)fg->getYlocCoord ((double)xgmax, (double)ygmin);
  float y4 = (float)fg->getYlocCoord ((double)xgmax, (double)ygmax);
  _ylmin = MinimumValue(MinimumValue(MinimumValue(y1,y2),y3),y4);
  _ylmax = MaximumValue(MaximumValue(MaximumValue(y1,y2),y3),y4);
}

int FgQcFoldType::createCmpList ()
{
  if (!_cmp_list) delete [] _cmp_list, _cmp_list = 0;
  FieldGeometry *fg = _qcp->fg();
  _cmp_list_size = (int)fg->numCmpGathers ();
  _cmp_count = (int)0;
  if (_cmp_list_size < (int)1) return (int)0;
  _cmp_list = new int[_cmp_list_size];
  if (!_cmp_list) return (int)0;
  return (int)1;
}

void FgQcFoldType::storeCmp (long ixcmp)
{
  _cmp_list[_cmp_count++] = (int)ixcmp;
}

int FgQcFoldType::sortCmpList ()
{
  if (_cmp_count < (int)1) return (int)0;
  if (_cmp_count != _cmp_list_size) {
    int *temp = 0;
    temp = new int[_cmp_count];
    if (!temp) return (int)0;
    memcpy (temp, _cmp_list, (size_t)_cmp_count*sizeof(int));
    delete [] _cmp_list;
    _cmp_list = temp;
    _cmp_list_size = _cmp_count;
  }
  qsort ((void *)_cmp_list, (size_t)_cmp_count, sizeof(int),
   compareCmps);
  _previous_ixcmp = _cmp_list[0];
  _previous_cmp_index = 0;
  return (int)1;
}

int FgQcFoldType::compareCmps (const void *ixcmp1, const void *ixcmp2)
{
  return *((int *)ixcmp1) - *((int *)ixcmp2);
}

int FgQcFoldType::cmpIndex (long ixcmp)
{
  int ixc = (int)ixcmp;
  int *ixptr;
  if (_previous_ixcmp == (int)ixcmp) return _previous_cmp_index;
  ixptr = (int *)bsearch ((const void *)(&ixc), (const void *)_cmp_list,
    (size_t)_cmp_count, sizeof(int), compareCmps);

  if (!ixptr) {
    _previous_cmp_index = (int)0; // assumed won't ever happen
  }
  else {
    _previous_cmp_index = (int)(ixptr - _cmp_list);
  }

  _previous_ixcmp = (int)ixcmp;
  return _previous_cmp_index;
}

void FgQcFoldType::destroyCmpList ()
{
  if (_cmp_list) delete [] _cmp_list, _cmp_list = 0;
  _cmp_list_size = 0;
  _cmp_count = 0;
}

int FgQcFoldType::createLiveFoldOfStack ()
{
  FieldGeometry *fg = _qcp->fg();
  if (_live_fold_of_stack)
    delete [] _live_fold_of_stack, _live_fold_of_stack = 0;
  _total_traces = 0;
  if (_cmp_count < (int)1) return (int)0;
  _live_fold_of_stack = new int[_cmp_count];
  if (!_live_fold_of_stack) return (int)0;
  int fold;
  _max_live_fold = 0;
  int k2;
  for (k2 = 0; k2 < _cmp_count; k2++) {
    fold = (int)fg->liveFoldOfStack ((long)_cmp_list[k2]);
    if (fold > _max_live_fold) _max_live_fold = fold;
    _total_traces += fold;
    _live_fold_of_stack[k2] = fold;
    if (_do_abort) {
      if (!(k2 % 5000)) {
        if (_do_abort->userAbort()) {
          return (int)0;
        }
      }
    }
  }
  if (_max_live_fold > (int)254) {
    _veto_extended = True;
  }
  else {
    // 255 must be reserved for undefined
    _veto_extended = False;
  }
  return (int)1;
}

long FgQcFoldType::grabLiveFoldOfStack (long ixcmp)
{
  return (long)_live_fold_of_stack[cmpIndex(ixcmp)];
}

void FgQcFoldType::destroyLiveFoldOfStack ()
{
  if (_live_fold_of_stack)
    delete [] _live_fold_of_stack, _live_fold_of_stack = 0;
  _total_traces = 0;
}

int FgQcFoldType::createCmpTraceOffsets ()
{
  FieldGeometry *fg = _qcp->fg();
  if (_cmp_trace_offsets)
    delete [] _cmp_trace_offsets, _cmp_trace_offsets = 0;
  if (_cmp_offset_pntrs)
    delete [] _cmp_offset_pntrs, _cmp_offset_pntrs = 0;

  if (_total_traces < (int)1) return (int)0;
  _cmp_trace_offsets = new float[_total_traces];
  if (!_cmp_trace_offsets) return (int)0;
  _cmp_offset_pntrs = new float*[_cmp_count];
  if (!_cmp_offset_pntrs) return (int)0;

  int k2;
  long k3;
  int trace_count = 0;
  int fold;
  for (k2 = 0; k2 < _cmp_count; k2++) {
    fold = _live_fold_of_stack[k2];
    if (fold > (int)0) {
      _cmp_offset_pntrs[k2] = &_cmp_trace_offsets[trace_count];
      for (k3 = 0; k3 < (long)fold; k3++) {
        _cmp_trace_offsets[trace_count++]
          = fg->getCmpTraceOffset ((long)_cmp_list[k2], k3);
        if (_do_abort) {
          if (!(trace_count % 5000)) {
            if (_do_abort->userAbort()) {
              return (int)0;
            }
          }
        }
      }
    }
  }
  return (int)1;
}

float FgQcFoldType::grabCmpTraceOffset (long ixcmp, long ixfold)
{
  float *offsets = _cmp_offset_pntrs[cmpIndex(ixcmp)];
  return offsets[ixfold];
}

void FgQcFoldType::destroyCmpTraceOffsets ()
{
  if (_cmp_trace_offsets)
    delete [] _cmp_trace_offsets, _cmp_trace_offsets = 0;
  if (_cmp_offset_pntrs)
    delete [] _cmp_offset_pntrs,  _cmp_offset_pntrs  = 0;
}

int FgQcFoldType::createCmpTraceAzimuths ()
{
/*FieldGeometry *fg = _qcp->fg();*/
  if (_cmp_trace_azimuths)
    delete [] _cmp_trace_azimuths, _cmp_trace_azimuths = 0;
  if (_cmp_azimuth_pntrs)
    delete [] _cmp_azimuth_pntrs, _cmp_azimuth_pntrs = 0;

  if (_total_traces < (int)1) return (int)0;
  _cmp_trace_azimuths = new float[_total_traces];
  if (!_cmp_trace_azimuths) return (int)0;
  _cmp_azimuth_pntrs = new float*[_cmp_count];
  if (!_cmp_azimuth_pntrs) return (int)0;

  int k2;
  long k3;
  int trace_count = (int)0;
  int fold;
  double azimuth;
  for (k2 = 0; k2 < _cmp_count; k2++) {
    fold = _live_fold_of_stack[k2];
    if (fold > (int)0) {
      _cmp_azimuth_pntrs[k2] = &_cmp_trace_azimuths[trace_count];
      for (k3 = 0; k3 < (long)fold; k3++) {
        azimuth = /*fg->*/getCmpTraceAzimuth ((long)_cmp_list[k2], k3);
        if (azimuth == DNIL)
          _cmp_trace_azimuths[trace_count++] = FNIL;
        else
          _cmp_trace_azimuths[trace_count++] = (float)azimuth;
        if (_do_abort) {
          if (!(trace_count % 5000)) {
            if (_do_abort->userAbort()) {
              return (int)0;
            }
          }
        }
      }
    }
  }
  return (int)1;
}

double FgQcFoldType::grabCmpTraceAzimuth (long ixcmp, long ixfold)
{
  float *azimuths = _cmp_azimuth_pntrs[cmpIndex(ixcmp)];
  float azimuth   = azimuths[ixfold];
  if (azimuth == FNIL)
    return DNIL;
  else
    return azimuth;
}

void FgQcFoldType::destroyCmpTraceAzimuths ()
{
  if (_cmp_trace_azimuths)
    delete [] _cmp_trace_azimuths, _cmp_trace_azimuths = 0;
  if (_cmp_azimuth_pntrs)
    delete [] _cmp_azimuth_pntrs,  _cmp_azimuth_pntrs  = 0;
}
