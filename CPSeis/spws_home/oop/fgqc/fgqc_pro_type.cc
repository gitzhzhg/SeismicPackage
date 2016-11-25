//********************************************************
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
//Author Michael L. Sherrill  08/95
//Elevation plot type class
//********************************************************

#include <stdlib.h>
#include "fgqc/fgqc_pro_type.hh"
#include "fgqc/fgqc_plot.hh"
#include "vect/vector.hh"
#include "vect/vect_data.hh"
#include "vect/ll_seis_vect.hh"
#include "fgmap/fg_map_anno.hh"
#include "sl/sl_client_message.hh"
#include "oprim/static_utils.hh"
#include "named_constants.h"

FgQcProgressionType::FgQcProgressionType(FgQcPlot *fgqc_plot) 
                                     : FgQcPlotType(fgqc_plot)
{
  _fgmap_anno   = NULL;
  _xconnect     = NULL;
  _yconnect     = NULL;
  _boxVector    = NULL;
  _boxData      = NULL;
  _activeVector = NULL;
  _activeData   = NULL;
  _vect_ll      = new SeisVectLinkedList();
}


FgQcProgressionType::~FgQcProgressionType()
{
BaseData *data;
Vector *ptr;
Vector *nextptr = (Vector *) NULL;
void *p;


  if(_xconnect != NULL) free(_xconnect);
  if(_yconnect != NULL) free(_yconnect);

  if(_boxData != NULL)
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

  if(_vect_ll != NULL)    delete _vect_ll;

  if(_fgmap_anno != NULL) delete _fgmap_anno;
//
//  new SLClientMessage(_qcp->topWidget(), "junk", 
//                      postDestructorClientMessageFunc, (void *) _fgmap_anno);

}

//This is a temporary fix. If the FgMapAnno is deleted before returning
//to the main loop part of it appears to still exist and X crashes with
//a bad font on an expose.
//void FgQcProgressionType::postDestructorClientMessageFunc(void *obj)/
//{
//FgMapAnno *anno = (FgMapAnno *)obj;
//
//  if(anno) delete anno;
//
//}


//============================================================================
//====================== Find number of data points  =========================
//============================================================================
void FgQcProgressionType::computeNumPoints(float xmin, float xmax, 
                                           float ymin, float ymax)
{
long i;
double xloc, yloc;
FieldGeometry *fg = _qcp->fg();

  _num_points = 0;

    for(i=0; i<fg->numGroups(); i++)
      {
      fg->getSkiddedSourceCoords(i+1, &xloc, &yloc);
          if(xloc >= xmin && xloc <= xmax && yloc >= ymin && yloc <= ymax)
             _num_points++;
      }

}

//============================================================================
//====================== When data has changed determine action ==============
//============================================================================
int FgQcProgressionType::ValuesChanged(FieldGeometry * /*fg*/, long /*ixl*/,
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
    case FG_SHOT:
      if(nrem != nins)
        inform_action = MAKE_NEW_PLOT;
      else
        inform_action = EDIT_PLOT;
      break;
   
    case FG_DIST:
      inform_action = MAKE_NEW_PLOT;
      break;

    default:
      inform_action = NO_ACTION;
      break;
    }

  return(inform_action);
}


//============================================================================
//====================== Update active flag            =======================
//============================================================================
void FgQcProgressionType::postNewActiveFlag(FieldGeometry *fg, long /*ixl*/)
{
int line_width = 2;
int marker_size = 9;
long active_line, active_flag;
long source_index, group_number;
double xloc, yloc;

 if(fg->numSourcesAtActiveFlag()) 
   {
   active_line  = fg->getActiveLineIndex();
   active_flag  = fg->getActiveFlagIndexOnLine(active_line);
   source_index = 0; //may want other sources at active flag later
   group_number = fg->sourceGroupNumber(active_line,active_flag,source_index); 
   fg->getSkiddedSourceCoords(group_number, &xloc, &yloc);
   _activex[0] = xloc;
   _activey[0] = yloc;

   if(_activeData == NULL)
     {
     _activeData = new VectData((int)1,&_activex[0],&_activey[0]);
     _activeVector = _vect_ll->add(_activeData, "Blue", line_width, False,
                                   Vector::NoLine, Vector::SquareMarker,
                                   marker_size, line_width);
     }
   else
     {
     _activeData->replace(0,(int)1 ,(int)1, &_activex[0],&_activey[0]);
     }

   _activeVector->makeVisible();

   }


}



//============================================================================
//====================== Get data for plot             =======================
//============================================================================
int FgQcProgressionType::plot()
{
FgSeisPlot *sp = _qcp->sp();
FieldGeometry *fg = _qcp->fg();
long i, vindex;
int stat = True;
double xloc, yloc;
float user_left   = _qcp->getUserLeft();
float user_right   = _qcp->getUserRight();
float user_top   = _qcp->getUserTop();
float user_bottom   = _qcp->getUserBottom();
float minx = MinimumValue(user_left,user_right);
float maxx = MaximumValue(user_left,user_right);
float miny = MinimumValue(user_top,user_bottom);
float maxy = MaximumValue(user_top,user_bottom);
float user_width  = _qcp->getPlotWidth();
float user_height = _qcp->getPlotHeight();
int line_width = 2, marker_size = 9;
unsigned int arrow_length = 8;
float arrow_percent = .55;
unsigned int min_arrow_distance = 17;
int arrow_line_width = -1;


  if (_do_abort) _do_abort->setNewAction ();
  sp->setPlotType(PlotImage::PlotGRID);
  sp->setGridX1(user_left);
  sp->setGridX2(user_right);
  sp->setGridY1(user_top);
  sp->setGridY2(user_bottom);
  sp->setGridWidth(user_width);
  sp->setGridHeight(user_height);


  _num_points = 0;
  computeNumPoints(minx, maxx, miny, maxy);
  if(!_num_points) {
    _do_abort->actionComplete ();
    return (False);
  }

  
  if(_xconnect == NULL)
       {
       _xconnect = (float *)calloc(1,(unsigned int)(_num_points
                                   *sizeof(float)));
       _yconnect = (float *)calloc(1,(unsigned int)(_num_points
                                   *sizeof(float)));
       }
    else if(_boxData->getNumPts() != _num_points){
       _xconnect  =(float *)realloc(_xconnect,(unsigned int)(_num_points
                                   *sizeof(float)));
       _yconnect  =(float *)realloc(_yconnect,(unsigned int)(_num_points
                                   *sizeof(float)));
       }
    if(_xconnect == NULL || _yconnect == NULL)
      {
      printf("Couldnt allocate vectors for display.");
      _do_abort->actionComplete ();
      return (False);

      }

  vindex = 0;
  for(i=0; i<fg->numGroups(); i++)
    {
    fg->getSkiddedSourceCoords(i+1, &xloc, &yloc);
    if(xloc >= minx && xloc <= maxx && yloc >= miny && yloc <= maxy)
      {
      _xconnect[vindex] = xloc;
      _yconnect[vindex] = yloc;
      _minx = _minx > xloc ? xloc : _minx;
      _maxx = _maxx < xloc ? xloc : _maxx;
      _miny = _miny > yloc ? yloc : _miny;
      _maxy = _maxy < yloc ? yloc : _maxy;           
      ++vindex;
      }
    }

  
  sp->setSymetricalAnnotation(user_left,user_right,user_top,user_bottom);
  sp->setDrawXlines(True);

  
  stat = sp->plot();

  if(stat && _qcp->inApplicationWindow() == False)
    {
    wprocShowMsg( _qcp->helpLine(), "Attribute: Source Progression" );
    }
 
  if(stat)
    {
    sp->backingStore(True);
    _boxData = new VectData((int)_num_points,&_xconnect[0],&_yconnect[0]);
    _boxVector = _vect_ll->add(_boxData, "green", line_width, False,
                               Vector::SolidLine, Vector::SquareMarker,
                               marker_size, line_width);
    _boxVector->arrowsOn(arrow_length, arrow_line_width, "red", 
                         arrow_percent, min_arrow_distance);

    _boxVector->makeVisible();
    postNewActiveFlag(fg, 0);

    if(!_vect_ll->isAdded(_qcp->sp()))
       _vect_ll->addPlot(_qcp->sp(),True);
    _fgmap_anno = new FgMapAnno(fg, sp, False);
    _fgmap_anno->setIncrement((int)1);
    _fgmap_anno->show();
    _qcp->setTitles();
    }

 if (_do_abort) _do_abort->actionComplete ();
 return(stat);
}


//============================================================================
//====================== Edit existing plot            =======================
//============================================================================
int FgQcProgressionType::editData()
{
//Put search for the value changed and update the vectors here...later
  return (plot());
}
