//********************************************
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
//Author Michael L. Sherrill 05/97
//Creates picker to control random line generation
//********************************************

#include <stdlib.h>
#include "sp/seis_plot.hh"
#include "sl/prim_support.hh"
#include "cube/cube.hh"
#include "cube/cube_random_line_pop.hh"
#include "cube/cube_random_line.hh"
#include "cube/cube_random_line_picker.hh"
#include "cube/cube_random_line_xytable.hh"
#include "vect/vector.hh"
#include "vect/vect_data.hh"
#include "vect/ll_seis_vect.hh"
#include "cprim.h"
#include "named_constants.h"


//============================================================================
//=================== Random line picker   ===================================
//============================================================================
static char *picking_mode = "Mode: Pick XY points";
static const char * const help_token = "RANDOM";
static char *randomhelp= "mouse*RANDOM:  RANDOM LINE BTN#1: Select location \
BTN#2: Delete location, BTN#3: None";



CubeRandomLinePicker::CubeRandomLinePicker (Cube              *cube)
              : PickBase(cube->timesliceSP(),
                         picking_mode, 
                         help_token, randomhelp,
                         XC_tcross, allow, True)
{
  _cube = cube;
  _cube_random_line = _cube->randomLine();
  _modifying_segment = 0;   
  _old_numsegs = (int)_cube_random_line->getNumberTraces();
  _old_nummarkers = _cube_random_line->pickerSegments();   
  _first_time = True;
}

//============================================================================
//=================== Destructor           ===================================
//============================================================================
CubeRandomLinePicker::~CubeRandomLinePicker()
{
  
}

//============================================================================
//=================== Button action work   ===================================
//============================================================================
void CubeRandomLinePicker::buttonAny(int ev_x1,int ev_x2,int ev_y1, 
                           int ev_y2, int button, Action action, 
                           Modifier /*modifier*/)
{
SeisPlot *sp;
int previous_index, index, next_index;
float *xdata = _cube_random_line->xarray();
float *ydata = _cube_random_line->yarray();
float *x_xor;
float *y_xor;


  sp = _cube->timesliceSP();
  if(!sp->imageIsDisplayed())return;


  //See if we have a pick within 5 pixels of this one. If so modify it.
  int msm2, psm1;
  if(button == 1 && action == PickBase::press)
    {
    if(_cube_random_line->modifySegment(ev_x1, ev_y1))
      {
      _modifying_segment =
         _cube_random_line->findNearestSegment(getPlot()->xWC(ev_x1),
                                               getPlot()->yWC(ev_y1)) + 1;
      msm2 = _modifying_segment - 2;
      previous_index = MaximumValue(0,msm2);
      psm1 = _cube_random_line->pickerSegments() - 1;
      next_index     = (int)(MinimumValue(_modifying_segment,psm1));
      _cube_random_line->createXORvector();
      x_xor = _cube_random_line->x_xor_array();
      y_xor = _cube_random_line->y_xor_array();
      x_xor[0] = xdata[previous_index];
      x_xor[2] = xdata[next_index];
      y_xor[0] = ydata[previous_index];
      y_xor[2] = ydata[next_index];
      }    
    }
  
  //Dragging a segment
  if(button == 1 && action == PickBase::motion && _modifying_segment &&
     _first_time == False)
    {
    //constrainPick(&ev_x2, &ev_y2);
    x_xor = _cube_random_line->x_xor_array();
    y_xor = _cube_random_line->y_xor_array();
    x_xor[1] = getPlot()->xWC(ev_x2);
    y_xor[1] = getPlot()->yWC(ev_y2);
    index =  _modifying_segment - 1;
    if(_cube_random_line->vectXOR())
       _cube_random_line->vectDataXOR()->replace((int)0,(int)3,(int)3, 
                                                  x_xor, y_xor);
    }


  //Store the segment
  if(button == 1 && action == PickBase::release)
    {
    if(_modifying_segment)
      {
      index = _modifying_segment - 1;
      //constrainPick(&ev_x2, &ev_y2);
      }
    else
      {
      index = (int)_cube_random_line->pickerSegments();
      }
    _xy_table->valueTrap(_xy_table, CubeRandomLineXYTable::XARRAY,
                         index, getPlot()->xWC(ev_x2), 
                         1, "NOACTION");   
    _xy_table->valueTrap(_xy_table, CubeRandomLineXYTable::YARRAY,
                         index, getPlot()->yWC(ev_y2),
                         1, "NOACTION");
    _xy_table->setFocus(CubeRandomLineXYTable::XARRAY,      
            _cube_random_line->findNearestSegment(getPlot()->xWC(ev_x2),
                                                  getPlot()->yWC(ev_y2))); 
    if(_modifying_segment)
      {
      _modifying_segment = 0;
      _cube_random_line->destroyXORvector();
      }
    }


  //Delete a segment
  if(button == 2 && action == PickBase::press && 
     _cube_random_line->pickerSegments())
    {
    _xy_table->valueTrap(_xy_table, CubeRandomLineXYTable::XARRAY,
         _cube_random_line->findNearestSegment(getPlot()->xWC(ev_x2),
                                               getPlot()->yWC(ev_y2)),
                                               0.0, 1, "REMOVE");
    _xy_table->setFocus(CubeRandomLineXYTable::XARRAY,      
         _cube_random_line->findNearestSegment(getPlot()->xWC(ev_x2),
                                               getPlot()->yWC(ev_y2)));
    }

  

  PrimSupport::updateEverything();
}


//============================================================================
//=================== Constrain the picking ==================================
//============================================================================
void CubeRandomLinePicker::constrainPick(int *x, int *y)
{
int previous_index;
int next_index;
int previous_x, previous_y;
int next_x, next_y;
float *xdata = _cube_random_line->xarray();
float *ydata = _cube_random_line->yarray();
Boolean constrain_x = True;
Boolean constrain_y = True;

  int msm2, psm1;
  msm2 = _modifying_segment - 2;
  previous_index = MaximumValue(0,msm2);
  psm1 = _cube_random_line->pickerSegments() - 1;
  next_index     = (int)(MinimumValue(_modifying_segment,psm1));


  previous_x = getPlot()->xPixel(xdata[previous_index]);
  next_x     = getPlot()->xPixel(xdata[next_index]);
  previous_y = getPlot()->yPixel(ydata[previous_index]);
  next_y     = getPlot()->yPixel(ydata[next_index]);

  //See if we should constrain x and y. If the line is almost horizontal
  //we dont want to contrain the y, also if almost vertical dont contrain the x
  //Using 20 pixels arbitrarily here for now.
  if(abs(next_x - previous_x) < 4*CubeRandomLine::MINIMUM_PIXELS) 
    constrain_x = False;
  if(abs(next_y - previous_y) < 4*CubeRandomLine::MINIMUM_PIXELS) 
    constrain_y = False; 
  if(next_index - previous_index < 2)//first or last pick so dont constrain
    constrain_x = constrain_y = False;

  if(constrain_x)
    {
    if(*x >= next_x)     *x = next_x     - CubeRandomLine::MINIMUM_PIXELS;
    if(*x <= previous_x) *x = previous_x + CubeRandomLine::MINIMUM_PIXELS;
    }

  if(constrain_y)
    {
    if(*y >= next_y)     *y = next_y     - CubeRandomLine::MINIMUM_PIXELS;
    if(*y <= previous_y) *y = previous_y + CubeRandomLine::MINIMUM_PIXELS;
    }
}

//============================================================================
//====================== Draw selected line segments =========================
//== Variable names of _vector are for the actual line position draws ========
//== Variable names of _picker are the users actual picks             ========
//============================================================================
void CubeRandomLinePicker::drawVectors(SeisPlot *sp)
{
int vector_segments;
Boolean stat;
int *inline_points;
int *crossline_points;
float *vector_crosslines;
float *vector_inlines;
long i;

  if( !sp->imageIsDisplayed() ) return;
  
  _cube_random_line->sortSegments();

  //Ask the random line class to convert the user coordinates to the
  //closest real cube line points.
  stat = _cube_random_line->findTraces();
  //if(!stat) {_cube_random_line->hideVectors(); return;}
  vector_crosslines = _cube_random_line->vectorCrosslines();
  vector_inlines = _cube_random_line->vectorInlines();  
  crossline_points = _cube_random_line->getCrosslinePoints();
  inline_points = _cube_random_line->getInlinePoints();
  if(vector_crosslines == NULL || vector_inlines == NULL || 
     crossline_points  == NULL || inline_points  == NULL) return;
  

  
  vector_segments = (int)_cube_random_line->getNumberTraces();
  
  for(i=0;i<vector_segments;i++)
    {
    vector_crosslines[i] = _cube_random_line->getWCFromLineIndex(
                                          Cube::CrossLine,crossline_points[i]);
    vector_inlines[i]    = _cube_random_line->getWCFromLineIndex(
                                          Cube::InLine,inline_points[i]);
    }


  //Now draw the vectors

  if(_cube_random_line->vectLines() == NULL)
    {
    _cube_random_line->createVectors();
    }
  else
    {
     _cube_random_line->vectLines()->replace ((int)0,_old_numsegs,
                      (int)_cube_random_line->getNumberTraces(), 
                      _cube_random_line->vectorCrosslines(),
                      _cube_random_line->vectorInlines());
    _cube_random_line->vectMarkers()->replace((int)0,_old_nummarkers,
                      _cube_random_line->pickerSegments(),
                      _cube_random_line->xarray(),_cube_random_line->yarray());
    }


  _first_time = False;
  _cube_random_line->showVectors();
  _old_numsegs = (int)_cube_random_line->getNumberTraces();
  _old_nummarkers = _cube_random_line->pickerSegments();
}

