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
//                Author Michael L. Sherrill  10/95
//             Displays trace cmps and offset azimuths
//**************************************************************************

#include "fgmap/fg_map_anno.hh"
#include "fgqc/fgqc_cmp_type.hh"
#include "fgqc/fgqc_plot.hh"
#include "dp/grid_error_handler.hh"
#include "fgqc/fgqc_pick_menu.hh"
#include "cprim.h"
#include "named_constants.h"
#include "vect/vector.hh"
#include "vect/vect_data.hh"
#include "vect/ll_seis_vect.hh"
#include "oprim/static_utils.hh"
#include "sl/paintset_collection.hh"

#ifdef max
#undef max
#endif
#ifdef min
#undef min
#endif
#include "oprim/static_utils.hh"

FgQcCmpType::FgQcCmpType(FgQcPlot *fgqc_plot)
                         : FgQcPlotType(fgqc_plot), _showing_azimuths(0)
{
  _xvect              = NULL;
  _yvect              = NULL;
  _gxvect             = NULL;
  _gyvect             = NULL;
  _Vector             = NULL;
  _GVector            = NULL;
  _vect_data          = NULL;
  _gvect_data         = NULL;
  _vect_ll = new SeisVectLinkedList();
  _vect_ll_grid = new SeisVectLinkedList();
  _grid_lines = NULL;
  _fgmap_anno = NULL;
  _picker_menu = new FgQcGridDefinitionPickerMenu(_qcp->topWidget(),
                                                  "Pick Menu",
                                                  _qcp->getHelpCtx(), 
                                                  _qcp, this, CMP_DISTRIBUTION);
}


FgQcCmpType::~FgQcCmpType()
{
BaseData *data;
Vector *ptr;
Vector *nextptr = (Vector *) NULL;
void *p;

  if(_xvect != NULL) free(_xvect);
  if(_yvect != NULL) free(_yvect);
  if(_gxvect!= NULL) free(_gxvect);
  if(_gyvect!= NULL) free(_gyvect);
  if(_grid_lines != NULL) delete _grid_lines;

  if(_vect_data != NULL)
    {
    SU::holdVectors();
    for( ptr = _vect_ll->top(&p); ptr; ptr = nextptr)
      {
      data = ptr->getData();
      nextptr = _vect_ll->next(&p);
      _vect_ll->remove(ptr);
      delete data;
      }
    SU::flushVectors();
    }

  if(_gvect_data != NULL)
    {
    SU::holdVectors();
    for( ptr = _vect_ll_grid->top(&p); ptr; ptr = nextptr)
      {
      data = ptr->getData();
      nextptr = _vect_ll_grid->next(&p);
      _vect_ll_grid->remove(ptr);
      delete data;
      }
    SU::flushVectors();
    }

  if(_vect_ll != NULL)      delete _vect_ll;
  if(_vect_ll_grid != NULL) delete _vect_ll_grid;
  if(_fgmap_anno != NULL) delete _fgmap_anno;

}

//============================================================================
//====================== Find number of data points  =========================
//============================================================================
void FgQcCmpType::computeNumPoints(float xmin, float xmax, 
                                    float ymin, float ymax)
{
long i, j;
double xloc, yloc;
double sx, sy, zval;
FieldGeometry *fg = _qcp->fg();
double add_amount;
int trace_offset = 6;
int trace_x      = 58;
int trace_y      = 59;
long trace_index;
int error;

  _num_points = 0;
  _minz =  MAX_NUM;
  _maxz =  MIN_NUM;

  for(i=0; i<fg->numCmpGathers(); i++)
    {
    if(getCoordinateSystem() == SURVEYSYSTEM)
      fg->getCmpLocBinCenter(i, &xloc , &yloc );
    else
      fg->getCmpGridBinCenter(i, &xloc , &yloc );
    if(xloc >= xmin && xloc <= xmax && 
       yloc >= ymin && yloc <= ymax &&
       xloc != DNIL && yloc != DNIL)
      {
      for(j = 0; j < fg->foldOfStack(i); j++)
        {
        trace_index = fg->originalTraceIndex(i, j);
        error = fg->calculateHeaderWords(trace_index + 1, False);
        if(!error)
          {
          zval = (float)fg->getHeaderWordValue(trace_offset);
          if(getCoordinateSystem() == SURVEYSYSTEM)
            {
            sx   = fg->getHeaderWordValue(trace_x);
            sy   = fg->getHeaderWordValue(trace_y);
            }
          else
            {
            sx = fg->getXgridCoord(fg->getHeaderWordValue(trace_x),
                                   fg->getHeaderWordValue(trace_y));
            sy = fg->getYgridCoord(fg->getHeaderWordValue(trace_x),
                                   fg->getHeaderWordValue(trace_y));
            }
          _minx = _minx > sx   ? sx   : _minx;
          _maxx = _maxx < sx   ? sx   : _maxx;
          _miny = _miny > sy   ? sy   : _miny;
          _maxy = _maxy < sy   ? sy   : _maxy;           
          _minz = _minz > zval ? zval : _minz;
          _maxz = _maxz < zval ? zval : _maxz;
          ++_num_points;
          }
        }
      }
    }



  if(_minz == _not_defined) _minz += .001;
  if(_minz == _maxz || _maxz == _not_defined) _maxz += .002;
  if(_minx == _maxx) _maxx += .001;
  if(_miny == _maxy) _maxy += .001;

  //make image area limits 5% larger than actual data limits so no points
  //will be excluded by rounding errors
  //next four lines allow user to select outside of actual data limits
  _minx = SU::min(_minx, xmin);
  _maxx = SU::max(_maxx, xmax);
  _miny = SU::min(_miny, ymin);
  _maxy = SU::max(_maxy, ymax); 
  add_amount = .05 * (_maxx - _minx);
  _minx -= add_amount;
  _maxx += add_amount;
  add_amount = .05 * (_maxy - _miny);
  _miny -= add_amount;
  _maxy += add_amount; 


}

//============================================================================
//====================== When data has changed determine action ==============
//============================================================================
int FgQcCmpType::ValuesChanged(FieldGeometry * /*fg*/, long /*ixl*/,
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
//====================== Setup vector offset matrix    =======================
//============================================================================
int FgQcCmpType::OffsetMatrix( Boolean get_data)
{
unsigned int line_width = 0;
FieldGeometry *fg = _qcp->fg();
long num_vectors = 2 * _num_points;
long i, j, jj, vindex;
long trace_index;
double xloc, yloc;
float user_left = _qcp->getUserLeft();
float user_right = _qcp->getUserRight();
float user_top = _qcp->getUserTop();
float user_bottom = _qcp->getUserBottom();
float minx = SU::min(user_left, user_right);
float maxx = SU::max(user_left, user_right);
float miny = SU::min(user_top, user_bottom);
float maxy = SU::max(user_top, user_bottom);
int error;
long change_color;
int source_x   = 11;
int source_y   = 12;
int receiver_x = 14;
int receiver_y = 15;
int trace_x    = 58;
int trace_y    = 59;
float offset_reduction;
float grid_width, modula;
char v_color[8];
float tx, ty, sx, sy, rx, ry;

  //getting new data
  if(get_data)
    {
    if(_xvect == NULL)
       {
       _xvect = (float *)calloc(1,(unsigned int)(num_vectors
                                   *sizeof(float)));
       _yvect = (float *)calloc(1,(unsigned int)(num_vectors
                                   *sizeof(float)));
       }
    else if(_vect_data && _vect_data->getNumPts() != num_vectors){
       _xvect  =(float *)realloc(_xvect,(unsigned int)(num_vectors
                                 *sizeof(float)));
       _yvect  =(float *)realloc(_yvect,(unsigned int)(num_vectors
                                 *sizeof(float)));
       }
    if(_xvect == NULL || _yvect == NULL)
      {
      printf("Couldnt allocate vectors for showing offset azimuths");
      return(False);
      }

    grid_width =  SU::min(fg->getXgridWidth(),fg->getYgridWidth());
    if(grid_width <= 1.0) 
       grid_width = SU::max(fg->getXgridWidth(),fg->getYgridWidth());
    offset_reduction = (1.5 * (float)grid_width) / fg->getMaximumOffset();

    vindex = 0;
    for(i=0; i<fg->numCmpGathers(); i++)
      {
      if(getCoordinateSystem() == SURVEYSYSTEM)
        fg->getCmpLocBinCenter(i, &xloc , &yloc );
      else
        fg->getCmpGridBinCenter(i, &xloc , &yloc );
      if(xloc >= minx && xloc <= maxx && yloc >= miny && yloc <= maxy &&
         xloc != DNIL && yloc != DNIL)
        {
        for(j = 0; j < fg->foldOfStack(i); j++)
          {
          trace_index = fg->originalTraceIndex(i, j);
          error = fg->calculateHeaderWords(trace_index + 1, False);
          if(!error)
            {
            if(getCoordinateSystem() == SURVEYSYSTEM)
              {
              tx = fg->getHeaderWordValue(trace_x);
              ty = fg->getHeaderWordValue(trace_y);
              sx = fg->getHeaderWordValue(source_x);
              sy = fg->getHeaderWordValue(source_y);
              rx = fg->getHeaderWordValue(receiver_x);
              ry = fg->getHeaderWordValue(receiver_y);
              }
            else
              {
              tx = fg->getXgridCoord(fg->getHeaderWordValue(trace_x),
                                     fg->getHeaderWordValue(trace_y));
              ty = fg->getYgridCoord(fg->getHeaderWordValue(trace_x),
                                     fg->getHeaderWordValue(trace_y));
              sx = fg->getXgridCoord(fg->getHeaderWordValue(source_x),
                                     fg->getHeaderWordValue(source_y));
              sy = fg->getYgridCoord(fg->getHeaderWordValue(source_x),
                                     fg->getHeaderWordValue(source_y));
              rx = fg->getXgridCoord(fg->getHeaderWordValue(receiver_x),
                                     fg->getHeaderWordValue(receiver_y));
              ry = fg->getYgridCoord(fg->getHeaderWordValue(receiver_x),
                                     fg->getHeaderWordValue(receiver_y));
              }
            _xvect[vindex]   = tx + offset_reduction * (sx - tx);
            _yvect[vindex]   = ty + offset_reduction * (sy - ty);
            _xvect[++vindex] = tx + offset_reduction * (rx - tx); 
            _yvect[vindex]   = ty + offset_reduction * (ry - ty); 
            ++vindex;
            }
          }
        }
      }
    }
    //end of getting new data if



  if(_vect_data == NULL && _Vector == NULL)
    {
    for(i=0, jj = 0; i<fg->numCmpGathers(); i++)
      {
      if(getCoordinateSystem() == SURVEYSYSTEM)
        fg->getCmpLocBinCenter(i, &xloc , &yloc );
      else
        fg->getCmpGridBinCenter(i, &xloc , &yloc );
      if(xloc >= minx && xloc <= maxx && yloc >= miny && yloc <= maxy &&
         xloc != DNIL && yloc != DNIL)
        {
        for(j = 0; j < fg->foldOfStack(i); j++)
          {
          trace_index = fg->originalTraceIndex(i, j);
          error = fg->calculateHeaderWords(trace_index + 1, False);
          if(!error)
            {
            modula = fmod(xloc + yloc, 2);
            change_color = NearestInteger(modula);
            if(change_color)
              strcpy(v_color,"red");
            else
              strcpy(v_color,"blue");
            _vect_data = new VectData((int)(2),&_xvect[jj],&_yvect[jj]);
            _Vector = _vect_ll->add(_vect_data, v_color, line_width, False);
            _Vector->makeVisible();
            jj += 2;
            }
          }
        }
      }
    }
  else // this will require work if replacing is ever implemented
    {
    for(i=0, j=0; i<_num_points; i++, j+=2)
      {
       _vect_data->replace(0,(int)2 ,(int)2, 
                           &_xvect[j],&_yvect[j]);
      }
    }   


  if(_fgmap_anno != NULL) delete _fgmap_anno;
  _fgmap_anno = new FgMapAnno(fg, _qcp->sp(), False);
  _fgmap_anno->setIncrement((int)1);
  _fgmap_anno->show();

  return(True);
}


//============================================================================
//====================== Setup vector grid matrix      =======================
//============================================================================
int FgQcCmpType::GridMatrix()
{
long i;
long stride = 1;
const float *xgrid, *ygrid;
unsigned int line_width = 0;
FieldGeometry *fg = _qcp->fg();


  if(_grid_lines != NULL) delete _grid_lines;
  _grid_lines = new GridLines(fg);
  if(getCoordinateSystem() == SURVEYSYSTEM)
    _grid_lines->usingDistanceSystem();
  else
    _grid_lines->usingGridSystem();
  _grid_lines->calculateActualGridLines(_minx, _miny, _maxx, _maxy, stride);
  if(_gxvect == NULL)
     {
     _gxvect = (float *)calloc(1,(unsigned int)(_grid_lines->nPoints()
                                 *sizeof(float)));
     _gyvect = (float *)calloc(1,(unsigned int)(_grid_lines->nPoints()
                                 *sizeof(float)));
     }
  else if(_gvect_data && _gvect_data->getNumPts() != _grid_lines->nPoints()){
     _gxvect  =(float *)realloc(_gxvect,(unsigned int)(_grid_lines->nPoints()
                               *sizeof(float)));
     _gyvect  =(float *)realloc(_gyvect,(unsigned int)(_grid_lines->nPoints()
                               *sizeof(float)));
     }

  if(_gxvect == NULL || _gyvect == NULL)
    {
    printf("Couldnt allocate vectors for showing grid boxes");
    return(False);
    }

  xgrid = _grid_lines->xValues();
  ygrid = _grid_lines->yValues();
  for(i = 0; i < _grid_lines->nPoints(); i++)
    {
    _gxvect[i] = xgrid[i];
    _gyvect[i] = ygrid[i];
    }


  if(_gvect_data == NULL && _GVector == NULL)
    {
    _gvect_data = new VectData((int)(_grid_lines->nPoints()),_gxvect,_gyvect);
    _GVector = _vect_ll_grid->add(_gvect_data,"white",line_width, False);
    _GVector->makeVisible();
    }
  else
    {
    _gvect_data->replace(0,_gvect_data->getNumPts(),
                        (int)_grid_lines->nPoints(),&_gxvect[0],&_gyvect[0]);
    }   

  return(True);
}


//============================================================================
//====================== Show or hide grid lines        ======================
//============================================================================
void FgQcCmpType::ShowGridMatrix(Boolean show)
{

  if(_GVector == NULL) return;

  if(show)
    _GVector->makeVisible();
  else
    _GVector->makeInvisible();    
}



//============================================================================
//====================== Show or hide azimuths          ======================
//============================================================================
void FgQcCmpType::showAzimuths()
{
  if(_vect_data == NULL && _Vector == NULL)
    OffsetMatrix(False);

  if(_Vector == NULL) return;
  
  if(_showing_azimuths)
    {
    _vect_ll->makeInvisible();
    _showing_azimuths = 0;
    }
  else
    {
    _vect_ll->makeVisible();
    _showing_azimuths = 1;
    }
}


//============================================================================
//====================== Modify vector grid matrix      ======================
//============================================================================
int FgQcCmpType::modifyGridMatrix()
{

  return(GridMatrix());

}


Boolean FgQcCmpType::modifyTestingGridMatrix()
{

  return(_picker_menu->createVectors());

}
//============================================================================
//====================== Get data for plot             =======================
//============================================================================
int FgQcCmpType::plot()
{
SeisPlot *sp = _qcp->sp();
FieldGeometry *fg = _qcp->fg();
SeisColorPop *color_pop = _qcp->getColorPop();
long i, j, index;
int frame_num = 1, num_frames = 1; //may want movies later
int stat = True;
float xrange, yrange;
double xperpix, yperpix;  
double xloc, yloc, zval;
float *image_data;
int min_reach = 0;
int dont_resample= 1;
float user_left  = _qcp->getUserLeft();
float user_right  = _qcp->getUserRight();
float user_top  = _qcp->getUserTop();
float user_bottom  = _qcp->getUserBottom();
float minx = SU::min(user_left, user_right);
float maxx = SU::max(user_left, user_right);
float miny = SU::min(user_top, user_bottom);
float maxy = SU::max(user_top, user_bottom);
int pixel_width  = 5;
int pixel_height = 5;
int trace_offset = 6;
int trace_x      = 58;
int trace_y      = 59;
int source_x     = 11;
int source_y     = 12;
int receiver_x   = 14;
int receiver_y   = 15;
int error;
long trace_index;
long num_vectors;
long vindex;
float offset_reduction;
float grid_width;
float tx, ty, sx, sy, rx, ry;

  if (_do_abort) _do_abort->setNewAction ();
  _num_points = 0;
  computeNumPoints(minx, maxx, miny, maxy);
  if(!_num_points)
    {
    if (_do_abort) _do_abort->actionComplete ();
    return(False);
    }
  num_vectors = 2 * _num_points;

  if(_xvect == NULL)
     {
     _xvect = (float *)calloc(1,(unsigned int)(num_vectors
                                 *sizeof(float)));
     _yvect = (float *)calloc(1,(unsigned int)(num_vectors
                                 *sizeof(float)));
     }
  else if(_vect_data && _vect_data->getNumPts() != num_vectors){
     _xvect  =(float *)realloc(_xvect,(unsigned int)(num_vectors
                               *sizeof(float)));
     _yvect  =(float *)realloc(_yvect,(unsigned int)(num_vectors
                               *sizeof(float)));
     }
 

  if(_xvect == NULL || _yvect == NULL)
    {
    printf("Couldnt allocate vectors for showing offset azimuths");
    if (_do_abort) _do_abort->actionComplete ();
    return(False);
    }


  _float_grid = new FloatGrid((int)_qcp->getNumx(), (int)_qcp->getNumy(),
    _do_abort);
  if(!checkGridStatus(CHECK_GRID))
    {
    if (_do_abort) _do_abort->actionComplete ();
    return(False);
    }

  _float_grid_accessor = new FloatGridAccessor ();
  if(!checkGridStatus(CHECK_FGA))
    {
    if (_do_abort) _do_abort->actionComplete ();
    return(False);
    }

  _float_grid_accessor->specifyData (_float_grid);
  _float_grid_accessor->setXBinData (user_left, user_right);
  _float_grid_accessor->setYBinData (user_bottom, user_top);
  if(!checkGridStatus(CHECK_FGA))
    {
    if (_do_abort) _do_abort->actionComplete ();
    return(False);
    }

  sp->setMatchHeader(1);

  sp->setGridXYS(user_left, user_right, user_top, user_bottom);
  stat = sp->initArrayTypeData(frame_num,num_frames, _qcp->getNumx(),
                               _qcp->getNumy(), _float_grid->getArray());
  if(!stat)
    {
    if (_do_abort) _do_abort->actionComplete ();
    return(stat);
    }

  image_data = _float_grid->getArray();
  for(i=0;i<_qcp->getNumx()*_qcp->getNumy();i++) image_data[i] = _not_defined;

  _control_points = new ControlPoints(_num_points);
  if(!checkGridStatus(CHECK_CONTROL))
    {
    if (_do_abort) _do_abort->actionComplete ();
    return(False);
    }

  _fg_data = _control_points->getArray();
  if(_fg_data == NULL)
    {
    if (_do_abort) _do_abort->actionComplete ();
    return(False);
    }

  xrange = user_right - user_left;
  yrange = user_top - user_bottom; //y is northing by default
  xperpix = xrange / (float)(_qcp->getNumx() - 1.0);
  yperpix = yrange / (float)(_qcp->getNumy() - 1.0);

  grid_width =  SU::min(fg->getXgridWidth(),fg->getYgridWidth());
    if(grid_width <= 1.0) 
       grid_width = SU::max(fg->getXgridWidth(),fg->getYgridWidth());
  offset_reduction = grid_width / fg->getMaximumOffset();

  for(i=0;i<_qcp->getNumx();i++)
    sp->setHeader(i*sp->numHeaders() +
                  sp->matchHeader()-1, xperpix * i + user_left);


//Fill image array and vector arrays
  index  = 0;
  vindex = 0;
  for(i=0; i<fg->numCmpGathers(); i++)
    {
    if(getCoordinateSystem() == SURVEYSYSTEM)
      fg->getCmpLocBinCenter(i, &xloc , &yloc );
    else
      fg->getCmpGridBinCenter(i, &xloc , &yloc );
    if(xloc >= minx && xloc <= maxx && yloc >= miny && yloc <= maxy &&
       xloc != DNIL && yloc != DNIL)
      {
      for(j = 0; j < fg->foldOfStack(i); j++)
        {
        trace_index = fg->originalTraceIndex(i, j);
        error = fg->calculateHeaderWords(trace_index + 1, False);
        if(!error)
          {
          zval =  (float)fg->getHeaderWordValue(trace_offset);
          if(getCoordinateSystem() == SURVEYSYSTEM)
            {
            tx = fg->getHeaderWordValue(trace_x);
            ty = fg->getHeaderWordValue(trace_y);
            sx = fg->getHeaderWordValue(source_x);
            sy = fg->getHeaderWordValue(source_y);
            rx = fg->getHeaderWordValue(receiver_x);
            ry = fg->getHeaderWordValue(receiver_y);
            }
          else
            {
            tx = fg->getXgridCoord(fg->getHeaderWordValue(trace_x),
                                   fg->getHeaderWordValue(trace_y));
            ty = fg->getYgridCoord(fg->getHeaderWordValue(trace_x),
                                   fg->getHeaderWordValue(trace_y));
            sx = fg->getXgridCoord(fg->getHeaderWordValue(source_x),
                                   fg->getHeaderWordValue(source_y));
            sy = fg->getYgridCoord(fg->getHeaderWordValue(source_x),
                                   fg->getHeaderWordValue(source_y));
            rx = fg->getXgridCoord(fg->getHeaderWordValue(receiver_x),
                                   fg->getHeaderWordValue(receiver_y));
            ry = fg->getYgridCoord(fg->getHeaderWordValue(receiver_x),
                                   fg->getHeaderWordValue(receiver_y));
            }

          _fg_data[index]   = tx;
          _fg_data[++index] = ty;
          _fg_data[++index] = zval;
          _xvect[vindex]   = tx + offset_reduction * (sx - tx);
          _yvect[vindex]   = ty + offset_reduction * (sy - ty);
          _xvect[++vindex] = tx + offset_reduction * (rx - tx); 
          _yvect[vindex]   = ty + offset_reduction * (ry - ty);          
          ++index;
          ++vindex;
          }
        }
      }
    }

  if(_minz == _not_defined)_minz += 00001;
  if(_maxz == _not_defined)_maxz += 00001;
  if(_minz >= _maxz) _maxz = _minz + .00001;
  _float_grid->setRange(_not_defined, _minz, _maxz);
  if(!checkGridStatus(CHECK_GRID))
   {
    if (_do_abort) _do_abort->actionComplete ();
    return(False);
    } 

  _control_points->setExtremaLimits(0, user_left, user_right);
  if(!checkGridStatus(CHECK_CONTROL))
    {
    if (_do_abort) _do_abort->actionComplete ();
    return(False);
    }
  _control_points->setExtremaLimits(1, user_top, user_bottom);
  if(!checkGridStatus(CHECK_CONTROL))
    {
    if (_do_abort) _do_abort->actionComplete ();
    return(False);
    }
  _control_points->setExtremaLimits(2, _minz, _maxz);
  if(!checkGridStatus(CHECK_CONTROL))
    {
    if (_do_abort) _do_abort->actionComplete ();
    return(False);
    }

  _float_grid->setInsertSize (pixel_width, pixel_height);

  _dont_scale = 1;
  _max_hits   = 3;

  _auto_gridder_float = new AutoGridderFloat(min_reach, dont_resample,
                                             _dont_scale, _max_hits);
  if(!checkGridStatus(CHECK_AUTO))
    {
    if (_do_abort) _do_abort->actionComplete ();
    return(False);
    }

  _auto_gridder_float->initializeCalculations(_control_points,
                                 _float_grid_accessor, &_x_reach, &_y_reach);
  if(!checkGridStatus(CHECK_AUTO))
    {
    if (_do_abort) _do_abort->actionComplete ();
    return(False);
    } 
  
  _auto_gridder_float->analyze(_control_points, _float_grid); 
  if(!checkGridStatus(CHECK_AUTO))
    {
    if (_do_abort) _do_abort->actionComplete ();
    return(False);
    }

  _qcp->setReach(_x_reach, _y_reach);
  _image_col1 = _auto_gridder_float->firstColumn();
  _image_row1 = _auto_gridder_float->firstRow();
  _image_ncols= _auto_gridder_float->columnCount(); 
  _image_nrows= _auto_gridder_float->rowCount();

  delete _control_points;
  _control_points = NULL;

  sp->setSymetricalAnnotation(user_left,user_right,user_top,user_bottom);
  sp->setDrawXlines(False);
  sp->setDrawYlines(False);  
  sp->setGridWidth(_qcp->getPlotWidth());
  sp->setGridHeight(_qcp->getPlotHeight());
  sp->setNorm(PlotImage::EXTERNALNORM);
  sp->setExternalAmp(SU::max(_maxz,_minz));
  if(!color_pop->beenManaged() || _qcp->getResetColors())
    {
    if(_minz == _not_defined) _minz += .00001;
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

  if(stat)
    { 
    if(_qcp->getNumColors()) 
      {
      _hill_shader = new HillShader(_qcp->getNumColors());
      if(!checkGridStatus(CHECK_SHADER))
        {
        if (_do_abort) _do_abort->actionComplete ();
        return(False);
        } 
      }
    if(!_qcp->inApplicationWindow())
      {
      _qcp->setTitles ();
      wprocShowMsg( _qcp->helpLine(), _qcp->getTitle() );
      }
    //reset background color so rubber bands will show up
    Screen *scr = XtScreen(sp->getWidget());
    sp->setGridColor(PaintsetCollection::white(scr));
    //OffsetMatrix(False);
    if(!fg->getFixdist()) GridMatrix();
    if(!_vect_ll->isAdded(_qcp->sp()))
       _vect_ll->addPlot(_qcp->sp(),True);
    if(!_vect_ll_grid->isAdded(_qcp->sp()))
      _vect_ll_grid->addPlot(_qcp->sp(),True);

//   ehs 06sep00
//  _picker_menu->createPicker();
    }

  if (_do_abort) _do_abort->actionComplete ();
  return(stat);
}


//============================================================================
//====================== Currently just make a new plot ======================
//============================================================================
int FgQcCmpType::editData()
{
  return( plot() );
}

