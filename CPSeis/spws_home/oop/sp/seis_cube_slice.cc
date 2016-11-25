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
#include <assert.h>
#include "tfio.h"
#include "tfdefs.h"
#include "tf_global.h"
#include "sp/seis_plot.hh"
#include "sp/seis_cube_slice.hh"






SeisCubeSlice::SeisCubeSlice(SeisPlot *sp, AxisType axis_type) :
                          _sp(sp),  _axis_type(axis_type),
                          _current_slice(0), _use_temp_global(False)
{
}



Boolean SeisCubeSlice::setSlice(int slice)
{
  Boolean retval= True;
  
  switch (_axis_type) {

      case InLine :
                    retval= setInlineSlice(slice);
                    break;
      case CrossLine :
                    retval= setCrosslineSlice(slice);
                    break;
      case TimeSlice :
                    retval= setTimeSlice(slice);
                    break;
      default:      assert(0); break;
  } // end switch
  if (retval) _current_slice= slice;

  return retval;
}


int SeisCubeSlice::totalSlices()
{
  int retval= 0;
  int axis;
  switch (_axis_type) {

      case InLine :    axis= 3;   break;
      case CrossLine : axis= 2;   break;
      case TimeSlice : axis= 1;   break;
      default:         assert(0); break;
  } // end switch
  _sp->getCubeTrcio ();
  retval= tf_global_grid_size(&_sp->_temp_G, axis);
  return retval;
}


Boolean SeisCubeSlice::setCrosslineSlice(int slice)
{
  Boolean retval= False;
  if (_sp->_may_plot) {
    _sp->getCubeTrcio ();
    int num_xlines= tf_global_grid_size(&_sp->_temp_G, 2);
    int num_traces_per_slice=  tf_global_grid_size(&_sp->_temp_G, 3);
    if (slice <= num_xlines) {
        //_sp->setNPlt(num_traces_per_slice);
        //_sp->setNdo(1);
        //_sp->setISkp( slice-1 );
        //_sp->setNSkp(num_xlines);
        _sp->set3dSlice(slice);
        _sp->set3dAxis(2);
        retval= True;
    }
    else {
        printf("SeisCubeSlice::setCrosslineSlice: requested slice number %d\n",
                      slice); 
        printf("SeisCubeSlice::setCrosslineSlice: only %d slices available\n",
                      num_xlines); 
    }

  }
  return retval;
} 



Boolean SeisCubeSlice::setInlineSlice(int slice)
{
  Boolean retval= False;
  if (_sp->_may_plot) {
       _sp->getCubeTrcio ();
       int num_inlines= tf_global_grid_size(&_sp->_temp_G, 3);
       int num_traces_per_slice=  tf_global_grid_size(&_sp->_temp_G, 2);
       if (slice <= num_inlines) {
              //_sp->setNPlt(num_traces_per_slice);
              //_sp->setNdo(1);
              //_sp->setISkp( num_traces_per_slice * (slice-1) );
              //_sp->setNSkp(0);
              _sp->set3dSlice(slice);
              _sp->set3dAxis(3);
              retval= True;
       }
       else {
          printf("SeisCubeSlice::setInlineSlice: requested slice number %d\n",
                      slice); 
          printf("SeisCubeSlice::setInlineSlice: only %d slices available\n",
                      num_inlines); 
       }

  }
  return retval;
} 


Boolean SeisCubeSlice::setTimeSlice(int slice)
{
  Boolean retval= False;
  if (_sp->_may_plot) {
       _sp->getCubeTrcio ();
       int num_ts= tf_global_grid_size(&_sp->_temp_G, 1);
       if (slice <= num_ts) {
              _sp->set3dSlice(slice);
              _sp->set3dAxis(1);
              retval= True;
       }
       else {
          printf("SeisCubeSlice::setInlineSlice: requested slice number %d\n",
                      slice); 
          printf("SeisCubeSlice::setInlineSlice: only %d slices available\n",
                      num_ts); 
       }

  }
  return retval;
}



void      SeisCubeSlice::setAxis(AxisType axis_type) { _axis_type= axis_type;}

SeisPlot *SeisCubeSlice::sp()           { return _sp; }
int       SeisCubeSlice::currentSlice() { return _current_slice;}

void SeisCubeSlice::getCoords(float *lmin,  float *lmax, 
                              float *xlmin, float *xlmax,
                              float *tsmin, float *tsmax)
{
 float d1=0.0, d2=0.0, d3=0.0; 
 int   n1=0, n2=0, n3=0; 
 *lmax =   *lmin = 0.0;
 *xlmax = *xlmin = 0.0;
 *tsmax = *tsmin = 0.0;

 if (strlen(_sp->filename()) > 0) {
     _sp->getCubeTrcio ();
     tf_global_grid_orgs (&_sp->_user->_G, tsmin, xlmin, lmin);
     tf_global_grid_delta(&_sp->_user->_G, &d1, &d2, &d3);
     tf_global_grid_sizes(&_sp->_user->_G, &n1, &n2, &n3);

     *lmax =   *lmin  + (n3 - 1) * d3;
     *xlmax = *xlmin  + (n2 - 1) * d2;
     *tsmax = *tsmin  + (n1 - 1) * d1;
 }
}

// Addeded to fix bug in cube.cc when a cube slice is initially set
void SeisCubeSlice::useTempGlobal (Boolean use_temp)
{
  _use_temp_global = use_temp;
}

float SeisCubeSlice::convertIndexToWC(int index)
{
 float orgin= 0.0, delta= 0.0;
 float o1=0.0, o2=0.0, o3=0.0; 
 float d1=0.0, d2=0.0, d3=0.0; 
 float retval;

 if (strlen(_sp->filename()) > 0) {
      _sp->getCubeTrcio ();

      // Addeded to fix bug in cube.cc when a cube slice is initially set
      struct GLBL *global;
      if (_use_temp_global) {
	global = &_sp->_temp_G;
      }
      else {
	global = &_sp->_user->_G;
      }
      // Addeded to fix bug in cube.cc when a cube slice is initially set

      tf_global_grid_orgs (global, &o1, &o2, &o3);
      tf_global_grid_delta(global, &d1, &d2, &d3);

      switch (_axis_type) {
          case InLine:
                        orgin= o3;
                        delta= d3;
                        break;
          case CrossLine:
                        orgin= o2;
                        delta= d2;
                        break;
          case TimeSlice:
                        orgin= o1;
                        delta= d1;
                        break;
          default:      assert(0);
                        break;
      } // end switch
 } // end if

 retval = orgin + ( (float)index * delta );
 return retval;
}


int SeisCubeSlice::convertWCToIndex(float coord)
{
 float orgin= 0.0, delta= 0.0;
 float o1=0.0, o2=0.0, o3=0.0; 
 float d1=0.0, d2=0.0, d3=0.0; 

 if (strlen(_sp->filename()) > 0) {
     _sp->getCubeTrcio ();
     tf_global_grid_orgs (&_sp->_user->_G, &o1, &o2, &o3);
     tf_global_grid_delta(&_sp->_user->_G, &d1, &d2, &d3);

     switch (_axis_type) {
         case InLine:
                       orgin= o3;
                       delta= d3;
                       break;
         case CrossLine:
                       orgin= o2;
                       delta= d2;
                       break;
         case TimeSlice:
                       orgin= o1;
                       delta= d1;
                       break;
         default:      assert(0);
                           break;
     } // end switch
 } //end if

 int slice= (int)((coord - orgin + delta) / delta + .5) - 1;
 return slice;
}

void SeisCubeSlice::getAxisHeaders(int *h1, int *h2)
{
  _sp->getCubeTrcio ();
  *h1= tf_global_hdwrd(&_sp->_user->_G, 2);
  *h2= tf_global_hdwrd(&_sp->_user->_G, 3);

  if ( (*h1 == 0)   || (*h2 == 0) ||
       (*h1 == -99) || (*h2 == -99) ) {

      int axis;
      switch (_axis_type) {
          case InLine:     axis= 3; break;
          case CrossLine:  axis= 2; break;
          case TimeSlice:  axis= 1; break;
      } // end switch
    
      char *desc= tf_global_axis(&_sp->_user->_G, axis);
    
      //*h1= 17;
      //*h2= 18;
      *h1= 7;
      *h2= 8;
      if (desc) {
         printf("desc= %s\n", desc);
         if ( (strcmp(desc, "XGRID") == 0) ||
              (strcmp(desc, "YGRID") == 0) ){
             *h1= 7;
             *h2= 8;
         } // end if
         else if ( (strcmp(desc, "XANNOTATION") == 0) ||
                   (strcmp(desc, "YANNOTATION") == 0) ) {
             *h1= 37;
             *h2= 38;
         } // end if
         else {
             *h1= 17;
             *h2= 18;
         } // end else
      } // end if desc
  } // end if
}
