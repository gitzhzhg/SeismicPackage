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
//                  Author Michael L. Sherrill  04/94
//             Divides the fold bin into 9 segements and displays
//             the fattest bin using 9 colors
//**************************************************************************

#include "fgmap/fg_map_anno.hh"
#include "fgqc/fgqc_bincenter_type.hh"
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

FgQcBinCenterType::FgQcBinCenterType(FgQcPlot *fgqc_plot)
                       : FgQcPlotType(fgqc_plot)
{
  _gxvect             = NULL;
  _gyvect             = NULL;
  _GVector            = NULL;
  _gvect_data         = NULL;
  _gvect_ll           = new SeisVectLinkedList();
  _grid_lines         = NULL;
  _fgmap_anno         = NULL;
  _picker_menu        = new FgQcGridDefinitionPickerMenu(_qcp->topWidget(),
                                                         "Pick Menu",
                                                         _qcp->getHelpCtx(), 
                                                         _qcp, this,
                                                         BIN_CENTERS);
  _qcp->sp()->setPreDefColor(PlotImage::SECTOR);
  _qcp->sp()->setTopBorder(110);
}


FgQcBinCenterType::~FgQcBinCenterType()
{
BaseData *data;
Vector *ptr;
Vector *nextptr = (Vector *) NULL;
void *p;

  if(_gxvect!= NULL) free(_gxvect);
  if(_gyvect!= NULL) free(_gyvect);
  if(_grid_lines != NULL) delete _grid_lines;

  if(_gvect_data != NULL)
    {
    SU::holdVectors();
    for( ptr = _gvect_ll->top(&p); ptr; ptr = nextptr)
      {
      data = ptr->getData();
      nextptr = _gvect_ll->next(&p);
      _gvect_ll->remove(ptr);
      delete data;
      }
    SU::flushVectors();
    }

  if(_gvect_ll != NULL)    delete _gvect_ll;

  if(_fgmap_anno != NULL) delete _fgmap_anno;

}




//============================================================================
//====================== Find number of data points  =========================
//============================================================================
void FgQcBinCenterType::computeNumPoints(float xmin, float xmax, 
                                                float ymin, float ymax)
{
long i;
double xloc, yloc;
FieldGeometry *fg = _qcp->fg();
float add_amount;

  _num_points = 0;
  for(i=0; i<fg->numCmpGathers(); i++)
    {
    if(getCoordinateSystem() == SURVEYSYSTEM)
      fg->getCmpLocBinCenter(i, &xloc , &yloc );
    else
      fg->getCmpGridBinCenter(i, &xloc , &yloc );
    if(xloc >= xmin && xloc <= xmax && yloc >= ymin && yloc <= ymax &&
       xloc != DNIL && yloc != DNIL)
          _num_points++;
    }  

  _minx = xmin;
  _miny = ymin;
  _maxx = xmax;
  _maxy = ymax;
  //make image area limits 10% larger than actual data limits so no points
  //will be excluded by rounding errors
  add_amount = .05 * (_maxx - _minx);
  _minx -= add_amount;
  _maxx += add_amount;
  add_amount = .05 * (_maxy - _miny);
  _miny -= add_amount;
  _maxy += add_amount; 




}


void FgQcBinCenterType::drawColorBox()
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
//====================== When data has changed determine action ==============
//============================================================================
int FgQcBinCenterType::ValuesChanged(FieldGeometry * /*fg*/, long /*ixl*/,
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
//====================== Setup vector grid matrix      =======================
//============================================================================
int FgQcBinCenterType::GridMatrix()
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
  else if(_gvect_data->getNumPts() != _grid_lines->nPoints()){
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
    _GVector = _gvect_ll->add(_gvect_data,"white",line_width, False);
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
void FgQcBinCenterType::ShowGridMatrix(Boolean show)
{

  if(_GVector == NULL) return;

  if(show)
    _GVector->makeVisible();
  else
    _GVector->makeInvisible();    
}



//============================================================================
//====================== Modify vector grid matrix      ======================
//============================================================================
int FgQcBinCenterType::modifyGridMatrix()
{
  return(GridMatrix());
}



Boolean FgQcBinCenterType::modifyTestingGridMatrix()
{

  return(_picker_menu->createVectors());

}
//============================================================================
//====================== Get data for plot             =======================
//============================================================================
int FgQcBinCenterType::plot()
{
SeisPlot *sp = _qcp->sp();
FieldGeometry *fg = _qcp->fg();
SeisColorPop *color_pop = _qcp->getColorPop();
long i, index;
int frame_num = 1, num_frames = 1; //may want movies later
int stat = True;
float xrange, yrange;
double xperpix, yperpix;  
double xloc, yloc;
float *image_data;
float user_left  = _qcp->getUserLeft();
float user_right  = _qcp->getUserRight();
float user_top  = _qcp->getUserTop();
float user_bottom  = _qcp->getUserBottom();
float minx = SU::min(user_left, user_right);
float maxx = SU::max(user_left, user_right);
float miny = SU::min(user_top, user_bottom);
float maxy = SU::max(user_top, user_bottom);
int pixel_width = 1;
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


  xrange = _maxx - _minx;
  yrange = _miny - _maxy; //y is northing by default
  xperpix = xrange / (float)(_qcp->getNumx() - 1.0);
  yperpix = yrange / (float)(_qcp->getNumy() - 1.0);


  for(i=0;i<_qcp->getNumx();i++)
    sp->setHeader(i*sp->numHeaders() +
                    sp->matchHeader()-1, xperpix * i + _minx);



//Fill image array and vector arrays
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
      image_data[i] =  (float)fg->cmpFattestBinette(index);
    else
      image_data[i] = _not_defined;

    _minz = _minz > image_data[i] ? image_data[i] : _minz;
    _maxz = _maxz < image_data[i] ? image_data[i] : _maxz;
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


  _float_grid->setInsertSize (pixel_width, pixel_height);

  _dont_scale = 1;
  _max_hits   = 3;


  _qcp->setReach(_x_reach = 0.0, _y_reach = 0.0);

  sp->setNorm(PlotImage::EXTERNALNORM);
  sp->setExternalAmp(SU::max(_maxz,_minz));

  sp->setSymetricalAnnotation(_minx,_maxx,_miny,_maxy);
  sp->setDrawXlines(False);
  sp->setDrawYlines(False);  
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
    if( _qcp->getFgColorPop()->Texture()== FgSeisColorPop::INSERT_DATA)
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
    //reset background color so rubber bands will show up
    Screen *scr = XtScreen(sp->getWidget());
    sp->setGridColor(PaintsetCollection::white(scr));
    if(_fgmap_anno != NULL) delete _fgmap_anno;
    _fgmap_anno = new FgMapAnno(fg, _qcp->sp(), False);
    _fgmap_anno->setIncrement((int)1);
    _fgmap_anno->show();

    if(!fg->getFixdist()){ GridMatrix(); ShowGridMatrix(False); }
    if(!_gvect_ll->isAdded(_qcp->sp()))
      _gvect_ll->addPlot(_qcp->sp(),True);

//   ehs 06sep00
//  _picker_menu->createPicker();
    }

  if (_do_abort) _do_abort->actionComplete ();
  return(stat);
}


//============================================================================
//====================== Currently just make a new plot ======================
//============================================================================
int FgQcBinCenterType::editData()
{
  return( plot() );
}

