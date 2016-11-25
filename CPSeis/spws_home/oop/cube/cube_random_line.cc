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
//Creates class to generate random lines
//Used with menu CubeRandomLinePop
//********************************************
#include <stdlib.h>
#include <math.h>
#include "trace_selector.hh"
#include "sp/seis_plot.hh"
#include "sp/seis_cube_slice.hh"
#include "sl/sl_error_pop.hh"
#include "cube/cube.hh"
#include "cube/cube_random_line.hh"
#include "cube/cube_random_line_picker.hh"
#include "cube/cube_random_line_plot.hh"
#include "vect/vector.hh"
#include "vect/vect_data.hh"
#include "vect/ll_seis_vect.hh"

//============================================================================
//====================== Constructor =========================================
//============================================================================
CubeRandomLine::CubeRandomLine(Cube *cube)
{
  _cube                  = cube;
  _picker                = NULL;
  _cube_random_line_plot = NULL;
  _vect_data_lines       = NULL;
  _vect_data_markers     = NULL;
  _vect_data_xor         = NULL;
  _vectXOR               = NULL;
  _vectL                 = NULL;
  _vectM                 = NULL;
  _vect_ll               = new SeisVectLinkedList();
  _xdata = (float *)calloc(1,(unsigned int)
                         ( INITIAL_LOCATIONS * sizeof(float)));
  _ydata = (float *)calloc(1,(unsigned int)
                         ( INITIAL_LOCATIONS * sizeof(float)));
  _vector_inlines = (float *)calloc(1,(unsigned int)
                         ( INITIAL_LOCATIONS * sizeof(float)));
  _vector_crosslines = (float *)calloc(1,(unsigned int)
                         ( INITIAL_LOCATIONS * sizeof(float)));
  _x_xor = (float *)calloc(1,(unsigned int)
                         ( 3 * sizeof(float)));
  _y_xor = (float *)calloc(1,(unsigned int)
                         ( 3 * sizeof(float)));
  _picker_segments = 0;
  _number_traces   = 0;


  _crossline_array = (int *)calloc(1,(unsigned int)( sizeof(int)));
  _inline_array = (int *)calloc(1,(unsigned int)( sizeof(int)));

  
}


//============================================================================
//====================== Destructor                  =========================
//============================================================================
CubeRandomLine::~CubeRandomLine()
{
  if(_crossline_array) free(_crossline_array);
  if(_inline_array) free(_inline_array);
  if(_xdata) free(_xdata);
  if(_ydata) free(_ydata);
  if(_vector_inlines) free(_vector_inlines);
  if(_vector_crosslines) free(_vector_crosslines);
  if(_x_xor) free(_x_xor);
  if(_y_xor) free(_y_xor);
  delete _vect_ll;
  if(_vect_data_lines) delete _vect_data_lines;
  if(_vect_data_markers) delete _vect_data_markers;
  if(_vect_data_xor) delete _vect_data_xor;
  if(_picker) delete _picker;
  if(_cube_random_line_plot)delete _cube_random_line_plot;

}


//============================================================================
//====================== Create a picker         =========================
//============================================================================
CubeRandomLinePicker *CubeRandomLine::getPicker()            
{

  if(_cube->timesliceSP()->imageIsDisplayed() == False)return NULL;

  if(_picker == NULL) 
     _picker = new CubeRandomLinePicker(_cube);
  return _picker;
}

//============================================================================
//====================== Create a picker         =========================
//============================================================================
void CubeRandomLine::removePicker()            
{

  if(_picker)
    {
    delete _picker;
    _picker = NULL;
    hideVectors();
    }
}





//============================================================================
//====================== Get the time slice seisplot        ==================
//============================================================================
SeisPlot *CubeRandomLine::getTimeSliceSP()
{
 return _cube->timesliceSP();
}


//============================================================================
//==================Get world coordinates of cube           ==================
//============================================================================ 
void CubeRandomLine::getCubeWorldCoordinates(float *xmin, float *xmax,
                                             float *ymin, float *ymax)
{
  _cube_world_coordinates = new CubeWorldCoords (_cube);

  *ymin = _cube_world_coordinates->lineMin();  
  *ymax = _cube_world_coordinates->lineMax();
  *xmin = _cube_world_coordinates->crossLineMin();
  *xmax = _cube_world_coordinates->crossLineMax();

  delete _cube_world_coordinates;
}



//============================================================================
//==================Get line index from a world coordinate  ==================
//============================================================================ 
int CubeRandomLine::getLineIndexFromWC(Cube::WhichPlot which, float coord)
{
  return _cube->convertWCToIndex(which, coord);
}


//============================================================================
//==================Get a world coordinate from a line index =================
//============================================================================ 
float CubeRandomLine::getWCFromLineIndex(Cube::WhichPlot which, int index)
{
  return _cube->convertIndexToWC(which, index);
}


//============================================================================
//====================== Method to plot new data       =======================
//============================================================================
int CubeRandomLine::readData()
{
int stat = 0;

//chg TimeSlice to RandomLine when it is defined in cube.hh
  stat = _cube->plot( (Cube::WhichPlot)Cube::TimeSlice );

  return stat;
}

//============================================================================
//====================== Find traces that comprise the line ==================
//== From Chuck C Burch fortran, will complete conversion later    ===========
//== _crossline_array is an output of x trace centers              ===========
//== _inline_array is an output of y trace centers                 ===========
//== vindex is an output of indexes into user's line point         ===========
//== requests not currently used                                   ===========
//============================================================================
Boolean CubeRandomLine::findTraces(Boolean create_read_array)
{
float xloc1, yloc1, xloc2, yloc2;
int i, J;
float diffx, diffy;
int xinc, yinc;
int x1, y1, nx1, ny1;
float c1, d1;
int *vindex, *Y, *X;

  if(!_picker_segments)
   {
   if(create_read_array)
     errorPopUp("No picks found"); 
   return False;
   }

  //allocate arrays
  if(_crossline_array != NULL)
    _crossline_array  =(int *)realloc(_crossline_array,(unsigned int)
             ( _cube->totalLines() *
               _cube->totalCrossLines() * 
               sizeof(int)) );
  else
    _crossline_array = (int *)calloc(1,(unsigned int)(
               _cube->totalLines() *
               _cube->totalCrossLines() * 
               sizeof(int) ));
  if(_inline_array != NULL)
    _inline_array  =(int *)realloc(_inline_array,(unsigned int)(
               _cube->totalLines() *
               _cube->totalCrossLines() * 
               sizeof(int) ));
  else
    _inline_array = (int *)calloc(1,(unsigned int)(
               _cube->totalLines() *
               _cube->totalCrossLines() * 
               sizeof(int) ));
  vindex =  (int *)calloc(_picker_segments,(unsigned int)( sizeof(int))); 
  Y  = (int *)calloc(_picker_segments,(unsigned int)( sizeof(int)));
  X =(int*)calloc(_picker_segments,(unsigned int)( sizeof(int)));
  
  if(_crossline_array == NULL   || _inline_array == NULL || 
     vindex     == NULL   || Y  == NULL ||
     X  == NULL)
    {
    if(_crossline_array != NULL)
                       {free(_crossline_array); _crossline_array = NULL;}
    if(_inline_array != NULL)
                       {free(_inline_array); _inline_array = NULL;}
    if(vindex != NULL)
                       free(vindex);
    if(Y != NULL)
                       free(Y);
    if(X != NULL)
                       free(X);
    errorPopUp("Not enough array memory in CubeRandomLine::findTraces");
    return False;
    }

  //Convert user's xy locations into line indexes
  for(i=0;i<_picker_segments;i++)
    {
    Y[i] = getLineIndexFromWC(Cube::InLine,    _ydata[i]);
    X[i] = getLineIndexFromWC(Cube::CrossLine, _xdata[i]); 
    }

  xloc1 = X[0];
  yloc1 = Y[0];
  _number_traces = 1;
  _crossline_array[0] = (int)xloc1;
  _inline_array[0] = (int)yloc1;
  vindex[0]= 1;
  for(i=2;i<=_picker_segments;i++)
    {
    xloc2 = X[i-1];
    yloc2 = Y[i-1];
    diffx = xloc2 - xloc1;
    diffy = yloc2 - yloc1;
    xinc  = 1;
    if(xloc1 > xloc2) xinc = -1;
    yinc = 1;
    if(yloc1 > yloc2) yinc = -1;

    if(diffx == 0.0)//fixed x ?
      {
      y1 = (int)yloc1 + yinc;
      for(J = y1; yinc < 0 ? J >= yloc2 : J <= yloc2; J+=yinc) 
        {
        _number_traces = _number_traces + 1;
        _crossline_array[_number_traces-1] = (int)xloc1;
        _inline_array[_number_traces-1] = J;
        }
      goto label190;
      }

    if(diffy == 0.0)//fixed y?
      {
      x1 = (int)xloc1 + xinc;
      for(J = x1; xinc < 0 ? J >= xloc2 : J <= xloc2; J+=xinc) 
        {
        _number_traces = _number_traces + 1;
        _crossline_array[_number_traces-1] = J;
        _inline_array[_number_traces-1] = (int)yloc1;
        }
      goto label190;
      }
    
    if(fabs(diffx) >= fabs(diffy))
      {
      c1 = yloc1;
      d1 = xinc * diffy / diffx;
      x1 = (int)xloc1 + xinc;
      for(J = x1; xinc < 0 ? J >= xloc2 : J <= xloc2; J += xinc) 
        {
        _number_traces = _number_traces + 1;
        c1 = c1 + d1;
        _crossline_array[_number_traces-1] = J;
        _inline_array[_number_traces-1] = (int)c1;
        }
      _inline_array[_number_traces-1] = (int)yloc2;
      goto label190;
      }

    c1 = xloc1;//slope > 1
    d1 = yinc * diffx / diffy;
    ny1 = (int)yloc1;
    nx1 = (int)xloc1;

    while(nx1 != xloc2 || ny1 != yloc2)
      {
      ny1 = ny1 + yinc;
      if(yinc > 0) 
        {
        if(ny1 > yloc2) ny1 = (int)yloc2;
        }
      else
        {
        if(ny1 < yloc2) ny1 = (int)yloc2;
        }
      c1 = c1 + d1;
      nx1 = (int)c1;
      if(xinc > 0)
        {
        if(nx1 > xloc2) nx1 = (int)xloc2;
        }
      else
        {
        if(nx1 < xloc2) nx1 = (int)xloc2;
        }
      _number_traces = _number_traces + 1;
      _crossline_array[_number_traces-1] = nx1;
      _inline_array[_number_traces-1] = ny1;
      }//end while

  label190:
    xloc1 = xloc2;//NEXT LINE SEGMENT
    yloc1 = yloc2;
    vindex[i-1] = (int)_number_traces;
    }//end for i


  free(Y);
  free(X);
  free(vindex);

  if(!_number_traces) return False;

  if(_number_traces > INITIAL_LOCATIONS)//realloc arrays for drawing vectors
    {
    _vector_crosslines = (float *) realloc(_vector_crosslines,(unsigned int)
                                          (_number_traces * sizeof(float)));
    _vector_inlines =    (float *) realloc(_vector_inlines,(unsigned int)
                                          (_number_traces * sizeof(float)));
    if(_vector_crosslines == NULL || _vector_inlines == NULL)
      {
      printf("Couldnt allocate vector arrays in CubeRandomLine\n");
      }  
    }

  if(create_read_array)
    return(createReadRequestArray());
  else
    return True;
}



//============================================================================
//====================== Create the read request array that       ============
//====================== will be used in PlotImage                ============
//============================================================================

Boolean CubeRandomLine::createReadRequestArray()
{
int i, numx, numy;
int ok;
long j, frames;

 frames = _cube_random_line_plot->SP()->frames() < 1 ? 1 : 
          _cube_random_line_plot->SP()->frames();

  for(j = 0; j < frames;j++)
    _cube_random_line_plot->SP()->getTraceSelector()->resetNumTraces(j);

  numx = _cube->crosslineSCS()->totalSlices();
  numy = _cube->inlineSCS()->totalSlices();


  for(i=0;i<_number_traces;i++)
    {
    ok = _cube_random_line_plot->SP()->setTraceSelectorTrace(0,
                               _crossline_array[i] + numx * _inline_array[i]);
    if(!ok) return False;
    if(_cube_random_line_plot->SP()->getSelectorNumTraces(0)> numx * numy) 
       return False; 
    }


 return True;
}


//============================================================================
//====================== Create the vectors     ==============================
//============================================================================
void CubeRandomLine::createVectors()
{

  if(_vect_data_lines == NULL)
    {
    _vect_data_lines =   new VectData((int)(_number_traces),_vector_crosslines,
                                  _vector_inlines);
    _vect_data_markers = new VectData((int)(_picker_segments),_xdata,_ydata);
    _vect_data_xor     = new VectData((int)3, _x_xor, _y_xor);
    _vectL =  _vect_ll->add(_vect_data_lines,"white",2);
    _vectM =  _vect_ll->add(_vect_data_markers,"white",1,False,
                            Vector::SolidLine,Vector::XMarker,9,2);
    showVectors();
    }
}



//============================================================================
//====================== Show the vectors       ==============================
//============================================================================
void CubeRandomLine::showVectors()
{
 if(_vectL != NULL) _vectL->makeVisible();
 if(_vectM != NULL) _vectM->makeVisible();
}


//============================================================================
//====================== Hide the vectors       ==============================
//============================================================================
void CubeRandomLine::hideVectors()
{

 if(_vectL != NULL) _vectL->makeInvisible();
 if(_vectM != NULL) _vectM->makeInvisible();
}

//============================================================================
//====================== Create the XOR vector  ==============================
//============================================================================
void CubeRandomLine::createXORvector()
{
 _vectXOR = _vect_ll->add(_vect_data_xor,"red",2,True); 

}

//============================================================================
//====================== Remove the XOR vector  ==============================
//============================================================================
void CubeRandomLine::destroyXORvector()
{
 if(_vectXOR != NULL) _vect_ll->remove(_vectXOR);
}


//============================================================================
//================= A "fortran style" sort of the segments ===================
//============================================================================
void CubeRandomLine::sortSegments()
{
float ytemp, xtemp;
long i,j,k,l,m;

  if(!_enforce_sort)return;

  m = _picker_segments;

  label1:
      m = m / 2;
      if (m == 0) return;
      k = _picker_segments - m;
      for(j = 0; j < k; j++)
        {
        i = j + m;
  label2:
        l = i;
        i = i - m;
        if(_xdata[i] > _xdata[l]  ||
           (_xdata[i] == _xdata[l] && _ydata[i] > _ydata[l]))
          {
          xtemp     = _xdata[i];
          ytemp     = _ydata[i];
          _xdata[i] = _xdata[l];
          _ydata[i] = _ydata[l];
          _xdata[l] = xtemp;
          _ydata[l] = ytemp;
          if(i >= m)goto label2;
          }
        }
      goto label1;


}

//============================================================================
//================= Find closest xy location from a value ====================
//============================================================================
int CubeRandomLine::findNearestSegment(float xloc,float yloc) 
{
int i, imin = -1;
float xdist, ydist, dist, distmin = 0.0;

 for(i = 0; i < _picker_segments; i++)
   {
   xdist = fabs(_xdata[i] - xloc);
   ydist = fabs(_ydata[i] - yloc);
   dist = xdist * xdist + ydist * ydist;
   if(i == 0 || dist < distmin)
     { 
     distmin = dist; 
     imin = i; 
     }
   }

  return imin; 
}


//============================================================================
//====================== Reset size of picker arrays =========================
//============================================================================
void CubeRandomLine::setNumberPickerSegments(int i)
{

  _picker_segments = i;

  //Only reallocate if valid entries exceeds original size
  if(_picker_segments > INITIAL_LOCATIONS)
    {
     _xdata  =(float *)realloc(_xdata,(unsigned int)
                              ( _picker_segments * sizeof(float)));
     _ydata  =(float *)realloc(_ydata,(unsigned int)
                              ( _picker_segments * sizeof(float)));
    }

  if(_xdata == NULL || _ydata == NULL)
     {
     printf("Couldnt allocate x or y arrays in CubeRandomLinePicker\n");
     return;
     }

}


//============================================================================
//================= Find closest xy location from a value ====================
//============================================================================
Boolean CubeRandomLine::modifySegment(int xloc,int yloc) 
{
int i;
int xdist, ydist, dist;
int pixel_x, pixel_y;

 i = findNearestSegment(_picker->getPlot()->xWC(xloc),
                        _picker->getPlot()->yWC(yloc));
 if(i < 0) return (False);

 pixel_x = _picker->getPlot()->xPixel(_xdata[i]);
 pixel_y = _picker->getPlot()->yPixel(_ydata[i]);

 xdist = pixel_x - xloc;
 ydist = pixel_y - yloc;
 dist = xdist * xdist + ydist * ydist;

 return (dist <= MINIMUM_PIXELS);
}





//============================================================================
//====================== Create and popup an error     =======================
//============================================================================
void CubeRandomLine::errorPopUp( char *errstr)
{
SLErrorPop *temp;

  temp = new SLErrorPop(_cube->timesliceSP()->imageGraphic(), "Error", errstr);
}

//============================================================================
//====================== Create and the plot popup     =======================
//============================================================================
CubeRandomLinePlot *CubeRandomLine::cubeRandomLinePlot(Widget p, char *name,
                                                     HelpCtx  hctx,
                                                     CubeRandomLinePop *pop, 
                                                     int numcolors,
                                                     CubeDisplay *cube_display)
{
  if(_cube_random_line_plot==NULL)
    _cube_random_line_plot =  
         new CubeRandomLinePlot(p, name, hctx, pop, numcolors, cube_display);

  return _cube_random_line_plot;
}

CubeRandomLinePlot *CubeRandomLine::cubeRandomLinePlot ()
{
  return _cube_random_line_plot;
}
