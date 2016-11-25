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
//             Displays trace cmp in normalized locations (i.e. -.5 to .5)
//**************************************************************************

#include "fgmap/fg_map_anno.hh"
#include "fgqc/fgqc_bin_type.hh"
#include "fgqc/fgqc_plot.hh"
#include "dp/grid_error_handler.hh"
#include "sp/seis_color.hh"
#include "sl/paintset_collection.hh"

#ifdef max
#undef max
#endif
#ifdef min
#undef min
#endif
#include "oprim/static_utils.hh"

#include "cprim.h"
#include "named_constants.h"


FgQcBinType::FgQcBinType(FgQcPlot *fgqc_plot)
                         : FgQcPlotType(fgqc_plot)
{
  _fgmap_anno = NULL;
//_picker_menu = new FgQcGridDefinitionPickerMenu(_qcp->topWidget(),
//                                                "Pick Menu",
//                                                _qcp->getHelpCtx(), 
//                                                _qcp, this, 
//                                                NORMALIZED_BINNING);
//set predefined color bar
  _qcp->sp()->setPreDefColor(PlotImage::SECTOR);

//set top border so there will be enough room for color box
  _qcp->sp()->setTopBorder(110);
}


FgQcBinType::~FgQcBinType()
{
  if(_fgmap_anno != NULL) delete _fgmap_anno;
}

//============================================================================
//====================== Find number of data points  =========================
//============================================================================
void FgQcBinType::computeNumPoints(float xmin, float xmax, 
                                   float ymin, float ymax)
{
long i, j;
double xloc, yloc;
FieldGeometry *fg = _qcp->fg();
double add_amount;
float increment;
float counter;

  _num_points = 0;
  _minx =  MAX_NUM;
  _maxx =  MIN_NUM;
  _miny =  MAX_NUM;
  _maxy =  MIN_NUM;
  _minz =  MAX_NUM;
  _maxz =  MIN_NUM;

//_qcp->_percent is a rough percentage in the menu to reduce the number of
//data points by since I always get at least one point for every cmp
//requested. Otherwise we would not get a good representation of every cmp
//in the survey.
  increment = 100.0 / _qcp->_percent;

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
      for(j = 0,counter = 0.0; j < fg->foldOfStack(i); j+=(long)counter)
        {
        counter += increment;
        ++_num_points;
        }
      }
    }

  _minx = -0.5;
  _maxx =  0.5;
  _miny = -0.5;
  _maxy =  0.5;

  //make image area limits 10% larger than actual data limits so no points
  //will be excluded by rounding errors
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
int FgQcBinType::ValuesChanged(FieldGeometry * /*fg*/, long /*ixl*/,
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
//====================== Get x and y sectors           =======================
//== This function divides the survey grid into 9 sectors, each of which    ==
//== will have its own color matched to a predefined seisplot color bar     ==
//== The sectors number 123
//==                    456
//==                    789
//== Their corresponding xy ids are
//==                1,3 2,3 3,3
//==                1,2 2,2 3,2
//==                1,1 2,1 3,1
//============================================================================
float FgQcBinType::getSectors(double xgrid, double ygrid)
{
float factor;
float location;
long xsector, ysector;
FieldGeometry *fg = _qcp->fg();


  factor   = 3.0 
           / (fg->getMaximumXgridBinCenter() - fg->getMinimumXgridBinCenter() );
  location = (xgrid - fg->getMinimumXgridBinCenter()) * factor + 0.5;
  xsector  = NearestInteger(location); 

  factor   = 3.0
           / (fg->getMaximumYgridBinCenter() - fg->getMinimumYgridBinCenter() );
  location = (ygrid - fg->getMinimumYgridBinCenter()) * factor + 0.5;
  ysector  = NearestInteger(location);

  if     (xsector == 1 && ysector == 1)
    return (1.0);
  else if(xsector == 2 && ysector == 1)
    return (2.0);
  else if(xsector == 3 && ysector == 1)
    return (3.0);
  else if(xsector == 1 && ysector == 2)
    return (4.0);
  else if(xsector == 2 && ysector == 2)
    return (5.0);
  else if(xsector == 3 && ysector == 2)
    return (6.0);
  else if(xsector == 1 && ysector == 3)
    return (7.0);
  else if(xsector == 2 && ysector == 3)
    return (8.0);
  else if(xsector == 3 && ysector == 3)
    return (9.0);
  else
    return (0.0);
}

//============================================================================
//====================== Draw the colored sectors box  =======================
//============================================================================
void FgQcBinType::drawColorBox()
{
SeisPlot *sp = _qcp->sp();
SeisColor *sc = new SeisColor(sp);
Display  *dpy = XtDisplay(sp->getWidget());
Screen   *scr = XtScreen(sp->getWidget());
Window win = XtWindow(sp->getWidget());
GC customgc = XCreateGC( dpy, win, None, NULL);
XFontStruct *font_fixed = XLoadQueryFont(dpy,"fixed");
int x1,y1;
float red, green, blue, amplitude;
char label[20];
int cellsize  = 25;
int i;
Pixmap pixmap = sp->imagePixmap((int)sp->currentFrame());

  if(font_fixed == NULL || customgc == NULL)
    {
    printf("Font or gc failed in bin type's drawColorBox");
    return;
    }
  XSetFont(dpy,customgc,font_fixed->fid);
 
  x1 = (int)sp->leftBorder() + cellsize;
  y1 = 2 * cellsize + 12;

  for(i=1; i < 10; i++)
    {
    sc->getAColor(i, &red, &green, &blue, &amplitude);
    XSetForeground(dpy, customgc, (int)sc->getXlibPixel(i));
    XFillRectangle(dpy, pixmap, customgc, x1,y1,cellsize,cellsize);    

//  sprintf(label,"%1.0f",amplitude);
    sprintf(label,"%1d",i);
    XSetForeground(dpy, customgc, PaintsetCollection::black(scr));
    XDrawString(dpy,pixmap,customgc,(x1+cellsize/2),(int)(y1+cellsize/1.5),
                label,1);

    if(x1 <=  2 * cellsize + sp->leftBorder())
      {
      x1 += cellsize;
      }
    else
      {
      x1 = (int)sp->leftBorder() + cellsize;
      y1 -= cellsize;
      }
    }

    x1 = (int)sp->leftBorder() + cellsize;
    y1 = 12;
//give box a 3d shadow look
    XDrawRectangle(dpy, pixmap, customgc, x1,y1, 3*cellsize, 3*cellsize);
    XDrawLine(dpy,pixmap,customgc,x1+5,y1+3*cellsize+1, x1+3*cellsize,
              y1+3*cellsize+1);
    XDrawLine(dpy,pixmap,customgc,x1+6,y1+3*cellsize+2, x1+3*cellsize,
              y1+3*cellsize+2);
    XDrawLine(dpy,pixmap,customgc,x1+3*cellsize+1,y1+5,x1+3*cellsize+1,
              y1+2+3*cellsize);
    XDrawLine(dpy,pixmap,customgc,x1+3*cellsize+2,y1+6,x1+3*cellsize+2,
              y1+2+3*cellsize); 
                   
   sp->redraw();

   XFreeGC(dpy,customgc);
   XFreeFont(dpy,font_fixed); 
}


//============================================================================
//====================== Get data for plot             =======================
//============================================================================
int FgQcBinType::plot()
{
SeisPlot *sp = _qcp->sp();
FieldGeometry *fg = _qcp->fg();
SeisColorPop *color_pop = _qcp->getColorPop();
long i, j, index;
int frame_num = 1, num_frames = 1; //may want movies later
int stat = True;
float xrange, yrange;
double xperpix, yperpix;  
double xloc, yloc;
float zval;
double xgrid, ygrid;
float *image_data;
float increment;
float counter;
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
int pixel_width  = 1;
int pixel_height = 1;


  if (_do_abort) _do_abort->setNewAction ();
  _num_points = 0;
  computeNumPoints(minx, maxx, miny, maxy);
  if(!_num_points)
    {
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
  _float_grid_accessor->setXBinData (_minx, _maxx);
  _float_grid_accessor->setYBinData (_miny, _maxy);


  if(!checkGridStatus(CHECK_FGA)) 
    {
    if (_do_abort) _do_abort->actionComplete ();
    return(False);
    }

  sp->setMatchHeader(1);

  sp->setGridXYS(_minx, _maxx, _maxy, _miny);
  stat = sp->initArrayTypeData(frame_num, num_frames, _qcp->getNumx(),
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

  xrange = _maxx - _minx;
  yrange = _miny - _maxy; //y is northing by default
  xperpix = xrange / (float)(_qcp->getNumx() - 1.0);
  yperpix = yrange / (float)(_qcp->getNumy() - 1.0);

  for(i=0;i<_qcp->getNumx();i++)
    sp->setHeader(i*sp->numHeaders() +
                   sp->matchHeader()-1, xperpix * i + _minx);


//Fill image array and vector arrays
  increment = 100.0 / _qcp->_percent;//see note in computeNumPoints
  index  = 0;
  for(i=0; i<fg->numCmpGathers(); i++)
    {
    if(getCoordinateSystem() == SURVEYSYSTEM)
      fg->getCmpLocBinCenter(i, &xloc , &yloc );
    else
      fg->getCmpGridBinCenter(i, &xloc , &yloc );
    if(xloc >= minx && xloc <= maxx && yloc >= miny && yloc <= maxy &&
       xloc != DNIL && yloc != DNIL)
      {
      for(j = 0, counter = 0.0; j < fg->foldOfStack(i); j+=(long)counter)
        {
          fg->getCmpTraceGrid(i, j, &xgrid, &ygrid);
          zval = getSectors(xgrid,ygrid);
          _fg_data[index]   = (float)(NearestInteger(xgrid) - xgrid);
          _fg_data[++index] = (float)(NearestInteger(ygrid) - ygrid);
          _fg_data[++index] = zval;
          _minz = _minz > zval ? zval : _minz;
          _maxz = _maxz < zval ? zval : _maxz;
          ++index;
          counter += increment;
        }
      }
    }

  if(_minz == _not_defined) _minz += .00001;
  if(_maxz == _not_defined) _maxz += .00001;
  if(_minz >= _maxz) _maxz = _minz + .00001;
  _float_grid->setRange(_not_defined, _minz, _maxz);
  if(!checkGridStatus(CHECK_GRID))
    {
    if (_do_abort) _do_abort->actionComplete ();
    return(False);
    }

  _control_points->setExtremaLimits(0, _minx, _maxx);
  if(!checkGridStatus(CHECK_CONTROL))
    {
    if (_do_abort) _do_abort->actionComplete ();
    return(False);
    }
  _control_points->setExtremaLimits(1, _miny, _maxy);
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

//  sp->setSymetricalAnnotation(_minx,_maxx,_miny,_maxy);
//  sp->setDrawXlines(True);
//  sp->setSeisAnnotation(False); 

  sp->setSeisAnnotation(True);
  sp->setSymetricalAnnotation(-.5,.5,-.5,.5);
  sp->setDrawXlines(False);

  sp->setNorm(PlotImage::EXTERNALNORM);
  sp->setExternalAmp(SU::max(_maxz,_minz));

  sp->setGridWidth(_qcp->getPlotWidth());
  sp->setGridHeight(_qcp->getPlotHeight());
  sp->setDoAmplitude(True);
  sp->setDoPercent(False);
  sp->setGradeVert(False);
  sp->setGradeHorz(False);
  sp->setMinColorAmp(0.0);
  sp->setMaxColorAmp(9.0);
  sp->setTopBorder(110);
  color_pop->presetAmplitudes(0.0, 9.0);
  color_pop->presetAmpType(True);
  color_pop->presetGrading(False);

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
    drawColorBox();
    if(!_qcp->inApplicationWindow())
      {
      _qcp->setTitles ();
      wprocShowMsg( _qcp->helpLine(), _qcp->getTitle() );
      }
    sp->backingStore(True);
    }


  if (_do_abort) _do_abort->actionComplete ();
  return(stat);
}


//============================================================================
//====================== Currently just make a new plot ======================
//============================================================================
int FgQcBinType::editData()
{
  return( plot() );
}

