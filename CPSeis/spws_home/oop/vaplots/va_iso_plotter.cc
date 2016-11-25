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
//========== Iso velocity plot class                             ==========
//========== Author Michael L. Sherrill 09/97                    ==========
//=========================================================================

// $Id: va_iso_plotter.cc,v 1.2 2004/04/05 18:12:11 wjdone Exp $
// $Name:  $

#include "vaplots/va_iso_plotter.hh"
#include "vaplots/va_iso_plot.hh"
#include "vaplots/va_iso_gui.hh"
#include "vaplots/va_gvs_plot.hh"
#include "vaplots/va_gvs_gui.hh"
#include "vaplots/va_plot.hh"
#include "vaplots/va_common_params_gui.hh"
#include "sp/seis_plot.hh"
#include "sp/seis_plot_under.hh"
#include "vf/vf_utilities.hh"
#include "vf/vf_dataset.hh"
#include "vf/vf_constants.hh"
#include "vf/vf_manager.hh"
#include "cprim.h"


#define YHDR 7
#define NOHDR 0




VaIsoPlotter::VaIsoPlotter(VfManager      *vf_manager,
                           VaIsoPlot      *iso_plot,
                           VaGvsPlot      *gvs_plot)
                         : VfInform(vf_manager)
{
  _vf_manager         = vf_manager;
  _iso_plot           = iso_plot;
  _gvs_plot           = gvs_plot;
  _iso_gui            = iso_plot->_gui;
  _gvs_gui            = gvs_plot->_gui;
  _iso_plot->_plotter = this;
  _gvs_plot->_plotter = this;
  _image_array        = NULL;
  _headers            = NULL;
  _gvs_plot->_sp_under->shareColorsWith(_iso_plot->_sp); 
  _gvs_plot->_sp_under->shareContourColors(
                                        _iso_plot->_sp->getContourColorPtr());
  _iso_gui->assignIsoPlotter(this);
  _gvs_gui->assignIsoPlotter(this);
}


VaIsoPlotter::~VaIsoPlotter()
{
  if(_image_array != NULL) delete _image_array;
}


void VaIsoPlotter::setColorOptions(int set_which_plot, Boolean turn_on)
{

  if(set_which_plot == VaPlot::ISO)
    _iso_gui->setColorOptions(turn_on);
  else
    _gvs_gui->setColorOptions(turn_on);

}


void VaIsoPlotter::drawLabel(SeisPlot *sp)
{
char label[256];
long i,index = 0;

switch(_iso_gui->getLineType())
 {
 case VaIsoPlot::INLINE:
 case VaIsoPlot::CROSSLINE:    
  for(i=0;i<sp->plottedFrames();i++)
    {
    sprintf(label,"LINE ID %f",_headers[index+_iso_plot->getActiveYheader()-1]);
    sp->drawLabel(label, i);
    index += sp->numHeaders() * sp->getTracesPerPanel(i);
    }
 break;        

 case VaIsoPlot::TIMESLICE:
  for(i=0;i<sp->plottedFrames();i++)
    {
    sprintf(label,"LINE ID %f",_iso_gui->getTimeOfFrame(i));
    sp->drawLabel(label, i);
    }
  break;
  }

}


int VaIsoPlotter::plot(SeisPlot *sp)
{
int stat;

  
  prepareData(sp);

  stat = sp->plot();
  if(stat == PlotImage::PlotSuccess)
    {
    sp->backingStore(True);
    drawLabel(sp);
    if(_iso_gui->underlay())
      { 
      stat = plotUnderlay(_gvs_plot->_sp_under, True);
      if(stat == PlotImage::PlotSuccess)
        _gvs_plot->_sp->backingStore(False);//speeds up movies
      }
    else
      {
      _gvs_plot->_sp_under->cleanup();
      _gvs_plot->_sp->backingStore(True);
      }
    }
  
  return stat;

}

//===========================================================================
//============= Plot underlay images                           ==============
//===========================================================================
int VaIsoPlotter::plotUnderlay(SeisPlotUnder *sp_under, Boolean prepare_data)
{
int stat;
SeisPlot *iso_sp = _iso_plot->_sp;
long *vel_list = _iso_gui->getVelRequestList();
VfDataset   *vfd = _vf_manager->activeDataset(); 
VfUtilities *vfu = _vf_manager->utilities    (); /* ehs */
float y;  
Boolean found = False;
float *gvs_headers;

  sp_under->setContours(iso_sp->contours());
  sp_under->setMinP(iso_sp->minP());
  sp_under->setMaxP(iso_sp->maxP());
  sp_under->setPlotWithContours(iso_sp->plotWithContours());
  sp_under->setContoursOnly(iso_sp->plottingContoursOnly());
  sp_under->setContourIncrement(iso_sp->getContourIncrement());


  sp_under->setTracesPerPanel(0,iso_sp->getTracesPerPanel(0));
  sp_under->setSrval(iso_sp->srval());
  sp_under->setMinColorAmp(iso_sp->minColorAmp());
  sp_under->setMaxColorAmp(iso_sp->maxColorAmp()); 
  sp_under->setGradeHorz((char)iso_sp->gradeHorz());
  sp_under->setGradeVert((char)iso_sp->gradeVert());
  sp_under->setDoAmplitude((int)iso_sp->doamplitude());
  sp_under->setDoPercent((int)iso_sp->dopercent());
  sp_under->setNPlt(iso_sp->originalTraces());
  sp_under->setSamples(iso_sp->displayedSamples());
  sp_under->setFrames(1);
  sp_under->pointToArrayTypeData(iso_sp);
  sp_under->setSeisAnnotation(False);
  sp_under->setPlotType(PlotImage::PlotARRAY);
  sp_under->setGridXYS(iso_sp->plottedGridX1(), iso_sp->plottedGridX2(),
                       iso_sp->plottedGridY1(), iso_sp->plottedGridY2());

  if(prepare_data)  prepareData(sp_under);
                   
  stat = sp_under->plotArrayTypeToSeismic(_iso_plot->getActiveXheader());

  if(stat == PlotImage::PlotSuccess)//Warn user if underlay y does not match
    {
    gvs_headers = _gvs_plot->_sp->getHeaderArrayForUpdate();
    y = vfd->getYloc(vel_list[0]); 
    if(y - vfu->getYwidth() <= gvs_headers[_iso_plot->getActiveYheader()-1] && 
       y + vfu->getYwidth() >= gvs_headers[_iso_plot->getActiveYheader()-1] )
        found = True;
    if(!found)
      _iso_plot->errorPopup("Warning underlay y does not match gvs/cvst/st");
    }

  return stat;

}
//===========================================================================
//============= External method to change the data plotted     ==============
//===========================================================================
Boolean VaIsoPlotter::changePlot(float x1, float x2, float y1, float y2,
                                 Boolean refresh_only)
{
Boolean stat;

  stat = _iso_gui->changePlot(x1, x2, y1, y2, refresh_only);
  return stat;
}
//===========================================================================
//============= Prepare data for plotting                      ==============
//===========================================================================
int VaIsoPlotter::prepareData(SeisPlot *sp)
{
int stat = True;
long i,j,k,m,start,end;
Display *dsp;
int screen;
int nsamp;
float top_y, bottom_y;
VfDataset *vfd = _vf_manager->activeDataset(); 
int index, counter;
long first_frame, num_frames, last_frame;
long frameskip;
long *vel_list = _iso_gui->getVelRequestList();
long *vel_ylist= _iso_gui->getVelYlinesList();
float *fast_location;
float *slow_location;
//float time_array[MAXPICKS];
//float velocity_array[MAXPICKS];
float time_range;
long max_possible_functions_in_frame = 0;


  getFileParameters("");//updates members with latest velocity file changes

  //only need pixels per inch off of one seis plot
  dsp = XtDisplay(sp->imageGraphic());
  screen = DefaultScreen(dsp);

  if(sp->movie())
    {
    if(_iso_gui->getLineType() != VaIsoPlot::TIMESLICE)
      {
      first_frame = (long)_iso_gui->firstFrame();
      num_frames  = _iso_gui->totalFrames();
      frameskip   = (long)_iso_gui->skipFrames();
      last_frame = (num_frames - 1) * (frameskip + 1) + first_frame;
      }
    else
      {
      first_frame = 1;
      num_frames = _iso_gui->totalFrames();
      frameskip = 0;
      last_frame = (num_frames - 1) * (frameskip + 1) + first_frame;
      }
    }
  else
    {
    first_frame = 1;
    num_frames = 1;
    frameskip = 0;
    last_frame = 1;
    }

  for(j=first_frame-1; j < last_frame; j+= frameskip + 1)
     max_possible_functions_in_frame = 
       max(max_possible_functions_in_frame,_iso_gui->getFunctionsInFrame(j));

  switch(_iso_gui->getLineType())
    {
    case VaIsoPlot::INLINE:
      fast_location = _xlocations;
      slow_location = _ylocations;
      top_y    = min(_iso_gui->getTmax(),_iso_gui->getTmin());
      bottom_y = max(_iso_gui->getTmax(),_iso_gui->getTmin());
      break;

    case VaIsoPlot::CROSSLINE:
      fast_location = _ylocations;
      slow_location = _xlocations;
      top_y         = min(_iso_gui->getTmax(),_iso_gui->getTmin());
      bottom_y      = max(_iso_gui->getTmax(),_iso_gui->getTmin());
      break;

    case VaIsoPlot::TIMESLICE:
      fast_location   = _xlocations;
      slow_location   = _ylocations;
      top_y           = _iso_gui->getY1();
      bottom_y        = _iso_gui->getY2();
      max_possible_functions_in_frame = max(_iso_gui->getNumberYlines(),
                                            max_possible_functions_in_frame);
      break;
    }

  time_range = top_y - bottom_y;
  sp->setSrval(  time_range / (_iso_gui->getHeight() *
                 sp->getVerticalPixelsPerInch(dsp,screen)) );


  nsamp   = (int)((top_y - bottom_y) /
                   sp->srval() / sp->tdec() + 1.5);

  

  if(_image_array == NULL)
     _image_array=(float *) calloc(1,((int)(max_possible_functions_in_frame *
                                           nsamp * num_frames*sizeof(float))));
  else
     _image_array=(float *)realloc(_image_array,
                                      (int)((max_possible_functions_in_frame *
                                           nsamp * num_frames*sizeof(float))));
  if(_image_array == NULL) return False;

  stat = sp->initArrayTypeData(1, num_frames,
                                max_possible_functions_in_frame,
                                nsamp, _image_array);
  if(!stat) return stat;

  _headers = sp->getHeaderArrayForUpdate();
  if(_headers == NULL) return False;

  sp->setHeaders(1,2);//use headers one and two for annotation and location

  start = end = 0;
  index = 0;
  counter = 0;
  for(j=first_frame-1, k=0; j < last_frame; j+= frameskip + 1, k++)
    {
    if(_iso_gui->getLineType() != VaIsoPlot::TIMESLICE)
      {
      sp->setTracesPerPanel(k,_iso_gui->getFunctionsInFrame(j));
      start = end = 0;
      for(m = 0; m < j; m++) start += _iso_gui->getFunctionsInFrame(m);
      end = start + _iso_gui->getFunctionsInFrame(j);
      }
    else//should be same number x locations for all time slices
      {
      sp->setTracesPerPanel(k,_iso_gui->getFunctionsInFrame(j));
      start = 0;
      end = _iso_gui->getFunctionsInFrame(j);
      }
    for(i=start;i<end;i++)
      {
      sp->setHeader(counter*sp->numHeaders()+sp->header1()-1,
                     fast_location[vel_list[i]]);
      sp->setHeader(counter*sp->numHeaders()+sp->header2()-1,
                     slow_location[vel_list[i]]);
      if(_iso_gui->getLineType() != VaIsoPlot::TIMESLICE)
        {
        sp->setHeader(counter*sp->numHeaders() + 
                           _iso_plot->getActiveXheader()-1,
                           fast_location[vel_list[i]]);
        sp->setHeader(counter*sp->numHeaders() + 
                           _iso_plot->getActiveYheader()-1,
                           slow_location[vel_list[i]]);
        vfd->velToFloat(vel_list[i], top_y,bottom_y,nsamp,&_image_array[index],
                        _iso_gui->getPlotType(), _iso_gui->gradeVertical());   
        }
      else
        {
        vfd->xbinToFloat (_iso_gui->getY1(), _iso_gui->getY2(),
                         fast_location[vel_list[i]] , _iso_gui->getPlotType(),
                         _iso_gui->getTimeOfFrame(k), nsamp,
                         &_image_array[index],
                         _iso_gui->gradeVertical());
        }
      ++counter;
      index = counter * nsamp;
      }
    }

  //set annotation header
  if(_iso_gui->getLineType() == VaIsoPlot::TIMESLICE)
    { 
     sp->setYannotationHeader(YHDR);
     sp->setNumberYlabels(_iso_gui->getNumberYlines());
     if(_iso_gui->getY2() < _iso_gui->getY1())
       for(i = _iso_gui->getNumberYlines() - 1; i > (-1); i--)
          _headers[i*sp->numHeaders()+YHDR] = 
                                      _iso_plot->_ylocations[vel_ylist[i]];
     else
       for(i = 0; i < _iso_gui->getNumberYlines(); i++)
          _headers[i*sp->numHeaders()+YHDR] = 
                                      _iso_plot->_ylocations[vel_ylist[i]];
    }
  else
    {
    sp->setYannotationHeader(NOHDR);
    } 


  return stat;
}





//===========================================================================
//============= Get velocity file information                  ==============
//===========================================================================
int VaIsoPlotter::getFileParameters(char *filename)
{
long stat = PlotImage::DEFS_OK;
VfDataset *vfd = _vf_manager->activeDataset(); 
int i;

  _number_locations_in_file = vfd->numVelocityFunctions();
  _iso_plot->allocateLocationArrays((int)_number_locations_in_file);
  _xlocations = _iso_plot->_xlocations;
  _ylocations = _iso_plot->_ylocations;
  _tmin = vfd->minimumAbscissa(_iso_gui->getPlotType());
  _tmax = vfd->maximumAbscissa(_iso_gui->getPlotType());
  _velocity_min = vfd->minimumOrdinate(_iso_gui->getPlotType());
  _velocity_max = vfd->maximumOrdinate(_iso_gui->getPlotType());
  _min_xbin = vfd->minimumXloc();
  _max_xbin = vfd->maximumXloc();
  _min_ybin = vfd->minimumYloc();
  _max_ybin = vfd->maximumYloc();


  for(i = 0; i < _number_locations_in_file; i++)
    {
    _xlocations[i] = vfd->getXloc(i);
    _ylocations[i] = vfd->getYloc(i);
    }

  _first_xbin = _xlocations[0];
  _last_xbin  = _xlocations[_number_locations_in_file - 1];
  _first_ybin = _ylocations[0];
  _last_ybin  = _ylocations[_number_locations_in_file - 1];

  //If 3D make sure x locations are the extreme of the data since all
  //lines may not have the same extent.
  if(_first_xbin != _last_xbin && _first_ybin != _last_ybin)
    {
    _first_xbin = _min_xbin;
    _last_xbin  = _max_xbin;
    _first_ybin = _min_ybin;
    _last_ybin  = _max_ybin;
    }

  _iso_plot->initialize();

  if(strlen(filename))
   {
   _iso_plot->updateGui();
   _iso_plot->dataReadByXYheaders(_iso_plot->getActiveXheader(),
                                  _iso_plot->getActiveYheader());
   }
  return stat;  
}



//=========================================================================
//==================== Change movie panel =================================
//=========================================================================
void VaIsoPlotter::changePanel(float x, float y, 
                               float /*time*/, float /*velocity*/,
                               SeisPlot *sp)
{
VfUtilities *vfu = _vf_manager->utilities    (); /* ehs */
float *hd;  
long frame;
float temp;

  if(sp->movie() == False || sp->imageIsDisplayed() == False ||
     _iso_gui->getLineType() == VaIsoPlot::TIMESLICE) return;

  hd = sp->getHeaderArrayForUpdate(); 

  //Flip x and y of the search if displaying crosslines
  if(_iso_gui->getLineType() == VaIsoPlot::CROSSLINE)
    {
    temp = x;
    x = y;
    y = temp;
    }

  //movie new location
  frame = _iso_plot->findArrayMatch(x, vfu->getXwidth(), y, vfu->getYwidth(), 
                                    &hd[_iso_plot->getActiveXheader() - 1], 
                                    &hd[_iso_plot->getActiveYheader() - 1],
                                    sp->originalTraces() * sp->plottedFrames(),
                                    (int)sp->numHeaders(), True);
  if(frame < 0) return;

  frame /= sp->originalTraces();

  if(sp->currentFrame() != frame) sp->movieToFrame((int)frame);

}


//=========================================================================
//==================== Modify panel       =================================
//=========================================================================
long VaIsoPlotter::modifyPanel(float xin,    float yin, 
                               float first_time, float last_time,
                               long  ifun, SeisPlot *sp)
{
VfDataset   *vfd = _vf_manager->activeDataset();
VfUtilities *vfu = _vf_manager->utilities    (); /* ehs */
float *hd;
int pixmap_index;
float temp;
long first_column, last_column;
long first_sample, last_sample;
long stat = 0;
long trace_index, column_index_to_edit;
float *image_array, y1, y2;
long array_start, array_end;
float *new_array;
long first_trace_index, last_trace_index;
long first_column_index_to_edit, last_column_index_to_edit;
float x, y;
long i, j, num_columns;



  if(!_iso_plot->_able_to_plot) return PlotImage::ReadFail;

  if(_iso_gui->getLineType() == VaIsoPlot::TIMESLICE) return stat;

  if(!sp->imageIsDisplayed() && !_gvs_plot->_sp_under->imageIsDisplayed())
     return stat;

  
  //Because iso data can be projected onto the seismic from 
  //locations that are off the seismic line I must compare the projected
  //data to find a change.
  y1           = sp->plottedTmin();
  y2           = sp->plottedTmax();
  hd           = sp->getHeaderArrayForUpdate();
  image_array  = sp->getFloatArrayForUpdate();
  pixmap_index = sp->currentFrame();  
  array_start  = pixmap_index * sp->originalTraces();
  array_end    = pixmap_index * sp->originalTraces() + sp->originalTraces();
  new_array    = new float[sp->samplesPerTrace()];
  if(new_array == NULL) return stat;
  first_trace_index = -1;
  last_trace_index  = -1;    

  
  //find the first
  for(i = array_start; i < array_end; i++)
    {
    x = hd[i * sp->numHeaders() + _iso_plot->getActiveXheader() - 1];
    y = hd[i * sp->numHeaders() + _iso_plot->getActiveYheader() - 1];
    vfd->velToFloat(x, y, y1, y2, sp->samplesPerTrace(), new_array,
                    _iso_gui->getPlotType(),   _iso_gui->gradeVertical());

    for(j = 0; j < sp->samplesPerTrace(); j++)
      {
      if(image_array[i * sp->samplesPerTrace() + j] != new_array[j])
        {
        first_trace_index = max(array_start, i - 1);
        i = array_end;
        j = sp->samplesPerTrace();
        }
      }
    }
  if(first_trace_index < 0)
    {
    delete new_array;
    return stat;
    } 

  //find the last
  for(i = array_end - 1; i >= array_start; i--)
    {
    x = hd[i * sp->numHeaders() + _iso_plot->getActiveXheader() - 1];
    y = hd[i * sp->numHeaders() + _iso_plot->getActiveYheader() - 1];
    vfd->velToFloat(x, y, y1, y2, sp->samplesPerTrace(), new_array,
                    _iso_gui->getPlotType(),   _iso_gui->gradeVertical());
    for(j = 0; j < sp->samplesPerTrace(); j++)
      {
      if(image_array[i * sp->samplesPerTrace() + j] != new_array[j])
        {
        last_trace_index = min(array_end - 1, i + 1);
        i = 0;
        j = sp->samplesPerTrace();
        }
      }
    }
  if(last_trace_index < 0)
    {
    delete new_array;
    return stat;
    }

  if(first_trace_index > last_trace_index)
    {
    long temp         = last_trace_index;
    last_trace_index  = first_trace_index;
    first_trace_index = temp;
    }


  first_column_index_to_edit = first_trace_index 
                             - (pixmap_index*sp->originalTraces());
  last_column_index_to_edit  = last_trace_index 
                             - (pixmap_index*sp->originalTraces());
 
  //work on this later to window the time
  first_column = min(first_column_index_to_edit + 1, sp->originalTraces());
  last_column  = min(last_column_index_to_edit  + 1, sp->originalTraces());
  num_columns  = last_column - first_column + 1;
  first_sample = 0;
  last_sample  = sp->samplesPerTrace() - 1;


  if(num_columns < 1)
    {
    delete new_array; 
    return stat;
    }
  //Now retrieve the new values
  for(i = first_trace_index; i <= last_trace_index; i++)
    {
    x = hd[i * sp->numHeaders() + _iso_plot->getActiveXheader() - 1];
    y = hd[i * sp->numHeaders() + _iso_plot->getActiveYheader() - 1];
    vfd->velToFloat(x, y, y1, y2, last_sample - first_sample + 1,
                    &image_array[i*sp->samplesPerTrace()+first_sample],
                    _iso_gui->getPlotType(),   _iso_gui->gradeVertical());
    }

  //Redraw the modified iso plot image if not doing contours only
  if(!sp->plottingContoursOnly())
    {
    stat = sp->modArrayTypeImage( first_column, last_column, 
                                  first_sample + 1, last_sample + 1,
                                  pixmap_index);
    if(!stat)
      {
      delete new_array;
      return stat;
      }
    }

  //Redraw the modified iso plot contours if any
  if(sp->plotWithContours())
    {
      stat = sp->contourGridderUpdate(
                                      first_column, last_column, 
                                      first_sample + 1, last_sample + 1,
                                      pixmap_index);
     if(!stat)
      {
      delete new_array;
      return stat;
      }
    }                     

  //Redraw the gvs underlay
  if(stat && _gvs_plot->_sp_under->imageIsDisplayed())
    {
    first_sample = _gvs_plot->_sp_under->getSampleIndexFromTime(
                                        _gvs_plot->_sp_under->plottedTmin());
    last_sample  = _gvs_plot->_sp_under->getSampleIndexFromTime(
                                        _gvs_plot->_sp_under->plottedTmax());

    //Redraw the gvs underlay if not plotting contours only
    if(!_gvs_plot->_sp_under->plottingContoursOnly())
      {
       stat = _gvs_plot->_sp_under->modArrayTypeImage( first_column, 
                                 last_column, 
                                 first_sample, last_sample,
                                 pixmap_index, _gvs_plot->_sp);
       if(!stat)
         {
         delete new_array;
         return stat;
         }
      }

    //Redraw the gvs underlay contours if any
    if(_gvs_plot->_sp_under->plotWithContours())
      {
      stat = _gvs_plot->_sp_under->contourGridderUpdate(first_column, 
                                 last_column, 
                                 first_sample, last_sample,
                                 pixmap_index, _gvs_plot->_sp);
      }
    }


  delete new_array;

  return stat;
}

//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//========================================================================
//==================  Informs only after this line  ======================
//========================================================================
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

void VaIsoPlotter::postNewActiveVelocityFunction (VfDataset *vfd)
{
float x,y;


/* ehs */
//  if(vfd != _vf_manager->activeDataset()) return;
  if((vfd != _vf_manager->activeDataset())
  || (vfd->getActiveVelocityFunction() == -1)) return;

  x = vfd->getXloc(vfd->getActiveVelocityFunction());
  y = vfd->getYloc(vfd->getActiveVelocityFunction()); 

  //may want to do a linked list of seisplots later
  changePanel(x,y,0.0,0.0,_iso_plot->_sp);
  changePanel(x,y,0.0,0.0,_gvs_plot->_sp_under);
}


void VaIsoPlotter::postTotalChanges(VfDataset *vfd)
{
  if(vfd != _vf_manager->activeDataset()) return;
  getFileParameters("Newfile");
}


void VaIsoPlotter::postModifyPicks(VfDataset *vfd, long ifun, 
                                int type,          long ipick, 
                                long /*nrem*/,  long /*nins*/)
{
float x,y;
float time, start_time, end_time, velocity;
long first_pick, last_pick, stat;

  if(vfd != _vf_manager->activeDataset()) return;

  long num_picks = vfd->numPicks(ifun);
  if(num_picks == 0) return;
  ipick = vfd->getActivePick(ifun);
  if(ipick < 0) return;

  first_pick = max(0,ipick - 1);
  last_pick =  min(num_picks - 1, ipick + 1);

  if(ipick == 0)//first in funtion so use image tmin
    start_time = _iso_plot->_sp->memTmin();
  else
    start_time = vfd->getAbscissa(ifun, first_pick, type);
  if(ipick == num_picks - 1) //last in function so use image tmax
    end_time = _iso_plot->_sp->memTmax();
  else
    end_time   = vfd->getAbscissa(ifun, last_pick,  type);
  time       = vfd->getAbscissa(ifun, ipick,      type);
  velocity   = vfd->getOrdinate(ifun, ipick,      type);

  //If the user has entered a velocity with out entering a time first when
  //using the table we get a time and velocity for ipick equal to -999.0 so
  //we want to ignore it until he has a time entered correctly. 05/01 MLS
  if(time == -999.0 || velocity == -999.0) return;

  //If the user has entered a velocity with out entering a time first then
  //enters the time we get a condition that makes the start_time and end_time
  //equal to each other. This will cause an assert in VfDataset's
  //velToFloat method. Also if the user has picks that are out of order 
  //(for instance you can get a negative value for depth) we also want to
  // return 05/01 MLS
  if(start_time >= end_time) return;

  x = vfd->getXloc(ifun);
  y = vfd->getYloc(ifun); 

  //may want to do a linked list of seisplots later
  stat = modifyPanel(x,y,start_time,end_time,ifun,_iso_plot->_sp);
  

  //may want to do a linked list of seisplots later
  if(stat) changePanel(x,y,time,velocity,_iso_plot->_sp);

}

void VaIsoPlotter::postRemoveInsertVelocityFunctions(VfDataset *vfd,
                                        long ifun, long /*nrem*/, 
                                        long /*nins*/)
{
float new_x1, new_x2;
float new_y1, new_y2;
float plot_xmin, plot_xmax;
float plot_ymin, plot_ymax;
float temp;
Boolean decreasing_x, decreasing_y;

  if(vfd != _vf_manager->activeDataset() || 
     _iso_plot->_sp->imageIsDisplayed() == False ||
     vfd->numVelocityFunctions() == 0 ) return;

  //If the last function was deleted, we need to decrement the
  //ifun since it will represent the function that is no longer in the data
  ifun = min(ifun, vfd->numVelocityFunctions() - 1);

  //redraw the entire image with the new coordinates
  decreasing_x = (_iso_plot->_sp->plottedGridX2() < 
                  _iso_plot->_sp->plottedGridX1()   );
  decreasing_y = (_iso_plot->_sp->plottedGridY2() < 
                  _iso_plot->_sp->plottedGridY1()   );
  plot_xmin = min(_iso_plot->_sp->plottedGridX1(),
                  _iso_plot->_sp->plottedGridX2());
  plot_xmax = max(_iso_plot->_sp->plottedGridX1(),
                  _iso_plot->_sp->plottedGridX2());
  plot_ymin = min(_iso_plot->_sp->plottedGridY1(),
                  _iso_plot->_sp->plottedGridY2());
  plot_ymax = max(_iso_plot->_sp->plottedGridY1(),
                  _iso_plot->_sp->plottedGridY2());

  switch(_iso_gui->getLineType())
   {
   case VaIsoPlot::INLINE:
     new_x1 = min(plot_xmin, vfd->getXloc(ifun));
     new_x2 = max(plot_xmax, vfd->getXloc(ifun)); 
     new_y1 = vfd->getYloc(ifun);
     new_y2 = vfd->getYloc(ifun);
     if(decreasing_x)
       {
       temp = new_x1;
       new_x1 = new_x2;
       new_x2 = temp;
       }
     break; 

   case VaIsoPlot::CROSSLINE:   
     new_y1 = min(plot_xmin, vfd->getYloc(ifun));
     new_y2 = max(plot_xmax, vfd->getYloc(ifun)); 
     new_x1 = vfd->getXloc(ifun);
     new_x2 = vfd->getXloc(ifun);
     if(decreasing_x)
       {
       temp = new_y1;
       new_y1 = new_y2;
       new_y2 = temp;
       }
     break; 

   case VaIsoPlot::TIMESLICE:
     new_x1 = vfd->minimumXloc();
     new_x2 = vfd->maximumXloc();
     new_y1 = vfd->minimumYloc();
     new_y2 = vfd->maximumYloc();
     if(decreasing_x)
       {
       temp = new_x1;
       new_x1 = new_x2;
       new_x2 = temp;
       }
     if(decreasing_y)
       {
       temp = new_x1;
       new_x1 = new_x2;
       new_x2 = temp;
       }
     break;

   }

  changePlot(new_x1, new_x2, new_y1, new_y2);

}
