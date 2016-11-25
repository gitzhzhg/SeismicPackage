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
//========================= COPYRIGHT NOTICE ================================
//====  CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.      ========
//====   PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK       ========
//======================== COPYRIGHT NOTICE =================================


//=========================================================================
//========== Base VA plot class that all images are derived from ==========
//========== Author Michael L. Sherrill 08/97                    ==========
//=========================================================================

// $Id: va_plot.cc,v 1.6 2005/12/13 16:20:48 spws Exp $
// $Name: 12-13-2005 $

#include <math.h>
#include "vaplots/va_plot.hh"
#include "sp/seis_plot.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_utilities.hh"	/* ehs */
#include "vf/vf_dataset.hh"
#include "sl/sl_error_pop.hh"
#include "sl/paintset_collection.hh"
#include "sl/paintset.hh"

#include "cprim.h"
#include "named_constants.h"

//=========================================================================
//======================== Constructor ====================================
//=========================================================================
VaPlot::VaPlot(VfManager *vf_manager, Widget w, char *name)
             : VfInform(vf_manager), SeisInform()
{
  _top_widget = w;
  _vf_manager = vf_manager;

  // this is grabbed so the PaintsetCollection _colorset_naming_policy
  //   won't be altered after this constructor is run
  int old_policy = PaintsetCollection::colorsetNamingPolicy ();

  // this is set so the Paintset _colorset_naming_policy is set 
  //   to NAMES_BY_PLOTS in init_image
  PaintsetCollection::setColorsetNamingPolicy (Paintset::NAMES_BY_PLOTS);

  Paintset *paintset = PaintsetCollection::fetchExistingByColormap (w);
  paintset->setClearPixelFromName ("gray83");

  _sp = new SeisPlot(w, name);

  addSeisPlot(_sp);
  _active_xheader = 7;
  _active_yheader = 8;
  _first_trace = 1;
  _traces_per_group = 1;
  _xlocations = NULL;
  _ylocations = NULL;
  _needs_replotting = True;
  _blank_image = False;
  _tmin = 0.0;
  _tmax = 0.0;
  _velocity_min = 0.0;
  _velocity_max = 0.0;
  _number_locations_in_file = 0;
  _able_to_plot = False;
  _sp->setLeftBorder(5);

  // set the PaintsetCollection _colorset_naming_policy back the way it was
  PaintsetCollection::setColorsetNamingPolicy (old_policy);
}


//=========================================================================
//======================== Destructor  ====================================
//=========================================================================
VaPlot::~VaPlot()
{
  if(_sp) delete _sp;
}



//=========================================================================
//=================== Return active dataset           =====================
//=========================================================================
VfDataset *VaPlot::vfDataset()
{
  return _vf_manager->activeDataset();
}

//=========================================================================
//======================== Return vf object number of functions ===========
//=========================================================================
long VaPlot::numberOfFunctions()
{
VfDataset *vfd = vfDataset();

  return vfd->numVelocityFunctions();
}


//=========================================================================
//================= Allocate arrays that store velocity locations =========
//=========================================================================
Boolean VaPlot::allocateLocationArrays( int size)
{

  if(_xlocations == NULL)
     _xlocations = (float *) calloc(1, (int)(size * sizeof(float)));
  else
     _xlocations = (float *) realloc(_xlocations, (int)(size *sizeof(float)));

  if(_ylocations == NULL)
     _ylocations = (float *) calloc(1, (int)(size * sizeof(float)));
  else
     _ylocations = (float *) realloc(_ylocations, (int)(size *sizeof(float)));


  if(_xlocations == NULL || _ylocations == NULL)
     return False;


  return True;
}


//=========================================================================
//============= Return vf object active x header                ===========
//=========================================================================
int VaPlot::getActiveXheader()
{
VfDataset *vfd = vfDataset();

  return vfd->getNhx();
}


//=========================================================================
//============= Return vf object active y header                ===========
//=========================================================================
int VaPlot::getActiveYheader()
{
VfDataset *vfd = vfDataset();

  return vfd->getNhy();
}

//=========================================================================
//============= Return displayed image's xbin                   ===========
//=========================================================================
float VaPlot::getDisplayedXbin()
{
const float *hd = _sp->firstMemoryHeaderData();

  if(!_sp->imageIsDisplayed()) return FNIL;

  return hd[getActiveXheader() - 1];
}

//=========================================================================
//============= Return displayed image's ybin                   ===========
//=========================================================================
float VaPlot::getDisplayedYbin()
{
const float *hd = _sp->firstMemoryHeaderData();

  if(!_sp->imageIsDisplayed()) return FNIL;

  return hd[getActiveYheader() - 1];
}

//=========================================================================
//============= Return displayed image's panel number index     ===========
//=========================================================================
int VaPlot::getDisplayedPanelIndex()
{
int i;


  if(!_sp->imageIsDisplayed()) return -1;

  for(i=0;i<numberLocationsInFile();i++)
    if(getDisplayedXbin() == _xlocations[i] && 
       getDisplayedYbin() == _ylocations[i])
         return i; 
  return -1;  

}

//=========================================================================
//============= Return displayed panel index from a x and y location ======
//============= One use is to find a panel to movie to               ======
//=========================================================================
int VaPlot::getDisplayedPanelIndexFromXY(float x, float y)
{
int panel;
/* ehs */
// VfDataset *vfd = vfDataset();
VfUtilities *vfu  = _vf_manager->utilities();
float *hd;
long array_stride;

  if(!_sp->imageIsDisplayed()) return -1;
  hd = _sp->getHeaderArrayForUpdate();
  array_stride = _sp->numHeaders() * _sp->originalTraces();
  panel = findArrayMatch(x, vfu->getXwidth(), y, vfu->getYwidth(), 
                         &hd[getActiveXheader() - 1], 
                         &hd[getActiveYheader() - 1],
                         _sp->plottedFrames(), (int)array_stride, True);

  return panel;
}

//=========================================================================
//============= Return panel index from a x and y location      ===========
//============= One use is to find what panel to read from file ===========
//=========================================================================
int VaPlot::getFilePanelIndexFromXY(float x, float y)
{
int panel;
/* ehs */
// VfDataset *vfd = vfDataset();
VfUtilities *vfu  = _vf_manager->utilities();

  if(_xlocations == NULL || _ylocations == NULL) return -1;

  panel = findArrayMatch(x, vfu->getXwidth(), y, vfu->getYwidth(), 
                         &_xlocations[0], &_ylocations[0],
                         numberLocationsInFile(), 1, True);

  return panel;
}

void VaPlot::changePanel(float /*x*/, float /*y*/, 
                         float /*time*/, float /*velocity*/)
{
}


//==========================================================================
//== Method to find the nearest or exact matching location given arrays   ==
//== xarray[n] and yarray[n], and desired values xloc and yloc.           ==
//== Returns nearest matching location (0 thru n-1) if one or more        ==
//== matches are found, or -1 if no match is found.                       ==
//== Arrays xarray[n] and yarray[n] can be any order or disorder.         ==
//== If an yarray is NULL, that array and associated parameters are not   ==
//== used.                                                                ==
//== array_stride is the offset into the arrays for each iteration.       ==
//== match_within_width matches within x and y tolerances specified.      ==
//==========================================================================
int VaPlot::findArrayMatch(double xloc,       double xwidth,
                           double yloc,       double ywidth,
                           float *xarray,     float *yarray,
                           long num_elements, int array_stride,
                           Boolean            match_within_width)
{
int i, imin = -1;
float xdist, ydist, dist, distmin = 0.0;
float xsize, ysize, this_x, this_y;


  if(xwidth == 0.0) xwidth = 1;
  if(ywidth == 0.0) ywidth = 1;

  if(!match_within_width)//Find nearest match
    {
    for(i=0; i<num_elements; i++)
       {
       if(yarray != NULL)
          ydist = fabs(yarray[i*array_stride] - yloc) / ywidth;
       else
          ydist = 0.0;
       if(xarray != NULL)
          xdist = fabs(xarray[i*array_stride] - xloc) / xwidth;
       else
          xdist = 0.0;

       dist = xdist * xdist + ydist * ydist;
       
       if(i == 0 || dist < distmin)
         {
         imin = i;
         distmin = dist;
         }
       }
    }
  else//Find match within tolerances
    {
    xsize = xwidth / 2.0;
    ysize = ywidth / 2.0;
    for(i=0; i<num_elements; i++)
       {
       if(yarray != NULL)
         this_y = yarray[i*array_stride];
       else
         this_y = 0.0;
       if(xarray != NULL)
         this_x = xarray[i*array_stride];
       else
         this_x = 0.0;
       if(this_x - xsize <= xloc && this_x + xsize >= xloc &&
          this_y - ysize <= yloc && this_y + ysize >= yloc    )
         return i;
       }
    }


  return imin;

}


void VaPlot::errorPopup(char *error_msg)
{
 _errorpop = new SLErrorPop(_top_widget, "Warning", error_msg);
}



char *VaPlot::getFileLabel()
{
  return _file_label;
}

void VaPlot::snapPickToLocalMax(float x_snap_min, float x_snap_max,
                                float y_snap_min, float y_snap_max,
                                float &xWC, float &yWC) {

    // Snap a pick to max semblance value within search window, changing
    // the values of xWC and yWC.

    // limit this to just semblance
    if (_plot_type != SEMBLANCE) {
        return;
    }

    int x_snap_max_pix = _sp->xPixel(x_snap_max);
    int x_snap_min_pix = _sp->xPixel(x_snap_min);
    int y_snap_max_pix = _sp->yPixel(y_snap_max);
    int y_snap_min_pix = _sp->yPixel(y_snap_min);

    // debug statements
    /*
    fprintf(stdout," Upper left:\n");
    fprintf(stdout,"   WC: v = %f  t = %f\n",x_snap_min,y_snap_min);
    fprintf(stdout,"   PC: v = %d  t = %d\n",x_snap_min_pix,y_snap_min_pix);
    fprintf(stdout," Upper right:\n");
    fprintf(stdout,"   WC: v = %f  t = %f\n",x_snap_max,y_snap_min);
    fprintf(stdout,"   PC: v = %d  t = %d\n",x_snap_max_pix,y_snap_min_pix);
    fprintf(stdout," Lower right:\n");
    fprintf(stdout,"   WC: v = %f  t = %f\n",x_snap_max,y_snap_max);
    fprintf(stdout,"   PC: v = %d  t = %d\n",x_snap_max_pix,y_snap_max_pix);
    fprintf(stdout," Lower left:\n");
    fprintf(stdout,"   WC: v = %f  t = %f\n",x_snap_min,y_snap_max);
    fprintf(stdout,"   PC: v = %d  t = %d\n",x_snap_min_pix,y_snap_max_pix);
     */


    // find location of max semblance within rectangle
    int xindex, yindex;
    float value;
    int max_x_index = x_snap_min_pix;
    int max_y_index = y_snap_min_pix;
    float maxsemb = _sp->getZvalueFromPixelXY(max_x_index, max_y_index);
    for (xindex = x_snap_min_pix; xindex <= x_snap_max_pix; xindex++) {
        for (yindex = y_snap_min_pix; yindex <= y_snap_max_pix; yindex++) {
            value = _sp->getZvalueFromPixelXY(xindex, yindex);
            if (maxsemb < value) {
                maxsemb = value;
                max_x_index = xindex;
                max_y_index = yindex;
            }
        }
    }

    float xWCorig = xWC;
    float yWCorig = yWC;
    xWC = _sp->xWC(max_x_index);
    yWC = _sp->yWC(max_y_index);
    
    // debug print statements
    /*
    fprintf(stdout," Max semblance = %f at\n",maxsemb);
    fprintf(stdout,"                          WC x = %f y = %f\n",
            xWC,yWC);
    fprintf(stdout,"                       pixel x = %d y = %d\n",
            max_x_index,max_y_index);
    fprintf(stdout,"      Original pick at    WC x = %f y = %f\n",
            xWCorig,yWCorig);
    fprintf(stdout,"                       pixel x = %d y = %d\n",
            _sp->xPixel(xWCorig),_sp->yPixel(yWCorig));
     */
}
