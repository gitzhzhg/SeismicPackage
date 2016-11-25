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



//===========================================================================
//========== Gvs plot class                                      ============
//========== Author Michael L. Sherrill 09/97                    ============
//===========================================================================

// $Id: va_gvs_plot.cc,v 1.3 2005/12/13 16:18:37 spws Exp $
// $Name: 12-13-2005 $

#include <stdlib.h>
#include "vaplots/va_plot.hh"
#include "vaplots/va_gvs_plot.hh"
#include "vaplots/va_gvs_gui.hh"
#include "vaplots/va_vect_colors.hh"
#include "vaplots/va_gvs_picks.hh"
#include "vaplots/va_common_params.hh"
#include "vf/vf_dataset.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_utilities.hh"
///#include "vf/vf_read_save.hh"    // removed 1/9/02 by TRS.
#include "sp/seis_plot.hh"
#include "sp/seis_plot_under.hh"
#include "sp/seis_winman.hh"
#include "sp/seis_scrwin.hh"
#include "sl/paintset_collection.hh"
#include "sl/paintset.hh"
#include "oprim/history.hh"
#include "trslib.h"
#include "tfdefs.h"
#include "str.h"

#define   UNDEFINED_LOC  -9873228.2F


VaGvsPlot::VaGvsPlot(VfManager          *vf_manager, 
                     class VfHorizons   *horizons,
                     Widget             w,
                     char               *name,
                     HelpCtx            hctx,
                     VaPlotCommonParams *common_params,
                     VaVectColors       *vect_colors,
                     long               numcolors)
                   : VaPlot(vf_manager, w, name)
{
  _numcolors = numcolors;
  _va_picks = new VaGvsPicks(vf_manager, horizons, this, _sp, vect_colors);
  setCommonParams(common_params);

  // this is grabbed so the PaintsetCollection _colorset_naming_policy
  //   won't be altered after this constructor is run
  int old_policy = PaintsetCollection::colorsetNamingPolicy ();

  // this is set so the Paintset _colorset_naming_policy is set 
  //   to NAMES_BY_PLOTS in init_image
  PaintsetCollection::setColorsetNamingPolicy (Paintset::NAMES_BY_PLOTS);

  _sp_under = new SeisPlotUnder(_sp);
  _sp_under->setLeftBorder(5);
  _gui = new VaGvsGui(w,"Gvs Gui", hctx, this);
  if(_numcolors) _sp->setLoadableColors((int)_numcolors);
  _plot_type        = GVS;
  _ref_times        = NULL;
  _ref_velocities   = NULL;
  _trace_velocities = NULL;
  _trace_velocities_xloc = UNDEFINED_LOC;
  _trace_velocities_yloc = UNDEFINED_LOC;
  _is_gvs = False;
  _gvs_dataset = NULL;
  _history = NULL;
  _reference_file_option = 0;
  _num_ref_picks = 0;

  // set the PaintsetCollection _colorset_naming_policy back the way it was
  PaintsetCollection::setColorsetNamingPolicy (old_policy);
}

VaGvsPlot::~VaGvsPlot()
{
  delete _va_picks;
  if(_ref_times        != NULL) delete _ref_times;
  if(_ref_velocities   != NULL) delete _ref_velocities;
  if(_trace_velocities != NULL) delete _trace_velocities;
  if(_gvs_dataset      != NULL) delete _gvs_dataset;
}

//=========================================================================
//================== New seis plot in window  =============================
//=========================================================================
void VaGvsPlot::notCurrentInWindow(SeisPlot *sp)
{
  addSeisPlot(sp->currentSPInWindow());
  _sp = sp->currentSPInWindow();
  _gui->seisPlotChanged();
  //may need to notify picker via _va_picks here
}

//===========================================================================
//============= Return ptr to the dialog                       ==============
//===========================================================================
SLShellContainer *VaGvsPlot::getDialog()
{
  return _gui;
}

void VaGvsPlot::manageGui()
{
  _gui->makeAndManage();
}


void VaGvsPlot::updateGui(Boolean update_file_limits)
{
  _gui->updateParams(update_file_limits);
}

int VaGvsPlot::plot()
{
int stat;
VfDataset *vfd = _vf_manager->activeDataset();

  if(!needsReplotting()) return PlotImage::PlotSuccess;  
  if(!_able_to_plot) return PlotImage::ReadFail; 

  setPlotParameters();
  stat = _sp->plot();
  if(stat == PlotImage::PlotSuccess)
    {
    _sp->backingStore(True);
    if(_is_gvs)initializeGvsVelocities();
    _common_params->setGvsReplot(False);
    if(vfd->getActiveVelocityFunction() >= 0)
      centerPanel(vfd->getXloc(vfd->getActiveVelocityFunction()),
                  vfd->getYloc(vfd->getActiveVelocityFunction()));
    }
  return stat;

}




//===========================================================================
//============= Get trace file information                     ==============
//===========================================================================
int VaGvsPlot::getFileParameters(char *filename)
{
long stat = PlotImage::DEFS_FAIL;
long xloc_hdr = getActiveXheader();
long yloc_hdr = getActiveYheader();
long max_traces = 0;
int user_aborted = 0;

  allocateLocationArrays(MAX_LOCATIONS);

  _sp->allocateVelocityArray(MAX_LOCATIONS);

  _sp->setFilename(filename);

  
  //Not checking for variable trace groups
  if(!_sp->usingSelector())
    {
    stat = _sp->getByteDefaults(_xlocations, _ylocations, xloc_hdr, 
                                yloc_hdr, &_number_locations_in_file);  
    if(stat != PlotImage::DEFS_OK) return stat;
    _traces_per_group = _sp->totalTraces() / _number_locations_in_file;
    }
  else
    {
    stat= _sp->findTracesByPattern(_sp->filename(),PlotImage::GRPHDR+1, yloc_hdr,
                                 _sp->totalTraces(),&_number_locations_in_file, 
                                 &user_aborted, _xlocations, _ylocations);
    if(!stat) 
       return PlotImage::DEFS_FAIL;
    else
       stat = PlotImage::DEFS_OK;

    for(int i = 0; i < _number_locations_in_file; i++)
       if(max_traces < _sp->getSelectorNumTraces(i))
          max_traces = _sp->getSelectorNumTraces(i);
    
    _traces_per_group = max_traces;

    }


  dataReadByXYheaders(getActiveXheader(),getActiveYheader());

  _tmin = _sp->minTmin();
  _tmax = _sp->maxTmax();

  _sp->setMatchHeader(getActiveXheader());
  _sp_under->setMatchHeader(getActiveXheader());
  determineDataType();

  updateGui();

  return stat;  
}

//==========================================================================
//=========== Query history class to determine gvs or cvs types ============
//==========================================================================
void VaGvsPlot::determineDataType()
{
char *err_msg, *vmod_type;
char *vel_path;
char vf_msg[222];//using this size because Tom does in vf class
int stat;
///char returned_keyword[222];//using this size because Tom does in vf class
char global_value[512];
char look_for_global[64];



  strcpy(global_value, "");
///  strcpy(returned_keyword, "");
  strcpy(look_for_global, "");

  if(_ref_times != NULL)
    {free(_ref_times); _ref_times = NULL;}

  if(_ref_velocities != NULL)
    {free(_ref_velocities); _ref_velocities = NULL;}

  if(_trace_velocities != NULL)
    {free(_trace_velocities);_trace_velocities=NULL;}

  //Migrated panels is a temporary new option to support picking
  //migrated panels that have not been run thru the gvs process.
  //The current code will need to be revised to support the new CPS
  //way of generating these panels when the new CPS has that option
  if(_reference_file_option)
    {
    _is_gvs = True;
    return;
    }

  if(_sp->isByteData())
    {
    _history = new History(_sp->filename(), "GVS");
    if( _history->constructedOK(&err_msg) == 0)
      {
       if( (stat = _history->getStr("VMODTYPE", &vmod_type) ) < 0 &&
           (stat = _history->getStr("MODIFIER_VEL", &vmod_type) ) < 0 )
         {
         errorPopup("Error in gvs global file");
         return;
         }
       if( strstr(vmod_type,"ADDITIVE") )
         _vmod_type = ADDITIVE;
       else
         _vmod_type = MULTIPLICATIVE;

       //New byte file format
       if( (stat = _history->getStr("PATHNAME_VEL", &vmod_type) ) == 0)
         {
         if( (stat = _history->getStr("PATH_VEL", &vel_path) ) < 0)
             {
             errorPopup("Error in gvs global file");
             return;
             }      

           if(_gvs_dataset != NULL)
           delete _gvs_dataset;

           _gvs_dataset = new  VfDataset (0, _vf_manager->informer(),
                                             _vf_manager->utilities()  );
         
           /// VfReadSave *vfrs = new VfReadSave(_vf_manager->informer(),
           ///                                   _vf_manager->utilities()  );
           ///
           /// stat = _gvs_dataset->readVelocityFile(vel_path, vf_msg,
           ///                                   vfrs, NULL);
           /// the above replaced by the following line 1/9/02 by TRS.

           stat = _gvs_dataset->readVelocityFile(vel_path, vf_msg, NULL);

           if(stat)
             {
             delete _gvs_dataset; _gvs_dataset = NULL;
             errorPopup(vf_msg);
             return;
             }
         }
       else//try old style byte file format
         {
         if((stat = _history->getPairedFloatArrays("REFERENCE FUNCTION", 
                                                 &_num_ref_picks,
                                                 &_ref_times,
                                                 &_ref_velocities))       < 0)
           {
           errorPopup("Error in gvs global file");
           return;
           }
         }

       _is_gvs = True;
      }
    else
      {
      _is_gvs = False;
      }

    delete _history;

    }//End of byte data checking
  else//See if we have a gvs history in a trot type file
    {
     strcpy(look_for_global, "MODIFIER_VEL");
     stat =  get_global_keyword_(_sp->filename(), 
                                 look_for_global,
                                 global_value);
     if(stat == 0)//Gvs is in the global record so see if it is CVST OR GVS mode
       {
       if(strstr(global_value,"CONSTANT"))//CONSTANT is used for cvst
         {
         _is_gvs = False;
         return;
         }

       _is_gvs = True;
       if(strstr(global_value,"MULTIPLICATIVE"))
         _vmod_type = MULTIPLICATIVE;
       else
         _vmod_type = ADDITIVE;
       //Get velocity file used in gvs
       strcpy(look_for_global, "PATH_VEL"); 
       stat =  get_global_keyword_(_sp->filename(), 
                                   look_for_global,
                                   global_value);
       if(stat != 0)
          {
          errorPopup("Could not find the path to the vel file used in gvs"); 
          _is_gvs = False;
          return;
          }

       //Create the dataset from the trot file's velocity info
       if(_gvs_dataset != NULL)
         delete _gvs_dataset;

       _gvs_dataset = new  VfDataset (0, _vf_manager->informer(),
                                         _vf_manager->utilities()  );
         
       /// VfReadSave *vfrs = new VfReadSave(_vf_manager->informer(),
       ///                                   _vf_manager->utilities()  );
       ///
       /// //Tom's class expects an array of 222 elements
       /// strncpy(returned_keyword,global_value,222);
       ///
       /// stat = _gvs_dataset->readVelocityFile(returned_keyword, vf_msg,
       ///                                       vfrs, NULL);
       /// the above replaced by the following line 1/9/02 by TRS.

       //Remove the trailing blanks.
       str_remove_trailing_blanks (global_value, global_value);

       stat = _gvs_dataset->readVelocityFile(global_value, vf_msg, NULL);

       if(stat)
         {
         delete _gvs_dataset; _gvs_dataset = NULL;
         errorPopup(vf_msg);
         }
       }//stat == 0 (ok) on a trot type file
     else//Who knows what kind of file this is
       {
       _is_gvs = False;
       }
     
    }

 

}



void VaGvsPlot::setPlotParameters()
{

  _gui->setParameters();

}

//=========================================================================
//============= Store gvs type velocities in a trace array      ===========
//=========================================================================
void VaGvsPlot::initializeGvsVelocities() 
{
float tmin;
float tmax;
long nsamp;

  if(_trace_velocities == NULL)
    {
    _trace_velocities = (float *) 
              calloc(1,(unsigned int)_sp->samplesPerTrace() * sizeof(float)); 
    }
  else
    {
    _trace_velocities = (float *)
            realloc(_trace_velocities,(unsigned int)_sp->samplesPerTrace() *
                    sizeof(float));
    }

  if(_trace_velocities == NULL)
    {
    errorPopup(
          "Not enough memory for reference array in initializeGvs method\n");
    return;
    }

  tmin  = _sp->memTmin();
  tmax  = _sp->memTmax();
  nsamp = _sp->samplesPerTrace();


  if(!_is_gvs)
    terpfill(&_ref_times[0], &_ref_velocities[0], &_num_ref_picks,
             &tmin, &tmax, &nsamp, &_trace_velocities[0]);

}


//=========================================================================
//============= Get velocity from a panel at a specified time   ===========
//=========================================================================
float VaGvsPlot::getVelocityFromPanel(long panel_index, float time, 
                                      long trace_number) 
{
float *hd = _sp->getHeaderArrayForUpdate();
long sample;
float reference_velocity;
int error;

 if(hd == NULL) return 0.0F;

  if(!_is_gvs)
    {
    return hd[panel_index*_sp->originalTraces()*_sp->numHeaders()+VELHEADER]; 
    }

  if(_reference_file_option)
    {
    error = populateReferenceFileVelocity(
      hd[(trace_number - 1) * _sp->numHeaders() + getActiveXheader() - 1],
      hd[(trace_number - 1) * _sp->numHeaders() + getActiveYheader() - 1]  );
    }
  else
    {
    error = populateGvsFileVelocity(
      hd[(trace_number - 1) * _sp->numHeaders() + getActiveXheader() - 1],
      hd[(trace_number - 1) * _sp->numHeaders() + getActiveYheader() - 1]  );
    }

  if(error)
    return 0.0F;

  sample = (long) ((time - _sp->memTmin()) / _sp->srval() + 1);
  reference_velocity = _trace_velocities[sample];  
  if(_vmod_type == ADDITIVE)
    return reference_velocity + hd[panel_index * _sp->numHeaders() * 
                                   _sp->originalTraces()+VELHEADER]; 
  else
    return reference_velocity * hd[panel_index *_sp->numHeaders() * 
                                   _sp->originalTraces()+VELHEADER];
}

//=========================================================================
//============= Center a panel around the active function       ===========
//=========================================================================
void VaGvsPlot::centerPanel(float x, float y)
{
long trace_index, trace;
VfUtilities *vfu = _vf_manager->utilities();
float *hd = _sp->getHeaderArrayForUpdate(); 
long x1;

   if(!_sp->isPlotDisplayed()) return;

   trace_index = findArrayMatch(x, vfu->getXwidth(), y, vfu->getYwidth(), 
                               &hd[getActiveXheader() - 1], 
                               &hd[getActiveYheader() - 1],
                               _sp->originalTraces(), (int)_sp->numHeaders(),
                               True);
   if(trace_index < 0) return;
   trace = trace_index + 1;

   if( !_sp->rToL())
     {
     x1 = trace * _sp->traceWidth() + _sp->leftBorder();
     }
  else /*right to left display*/
    {
    x1 = (_sp->displayedTraces() + 1 - trace) * _sp->traceWidth() 
            + _sp->leftBorder();
    }

  _sp->getSeisWinMan()->scrolledWindow()->centerOnX( (unsigned int) x1 );

}

//=========================================================================
//============= Change movie panel or read from file            ===========
//=========================================================================

void VaGvsPlot::changePanel(float x, float y, 
                            float time, float velocity)
{
float *hd = _sp->getHeaderArrayForUpdate();  
/* ehs */
// VfDataset *vfd = _vf_manager->activeDataset();
VfUtilities *vfu = _vf_manager->utilities();
long i;
float diff_current, diff_next;
long trace_index;
long found, sample;
float reference_velocity;
float temp_vel, current_vel, next_vel;
double minimum_vel, maximum_vel;
long min_panel_id, max_panel_id;
int error;

  if(!_sp->isPlotDisplayed()) return;

  //Currently we will not try to read a panel, only movie to one
  if(!_sp->movie()) return;
  
  //Make sure location is within the panel's data range
  trace_index = findArrayMatch(x, vfu->getXwidth(), y, vfu->getYwidth(), 
                               &hd[getActiveXheader() - 1], 
                               &hd[getActiveYheader() - 1],
                               _sp->originalTraces(), (int)_sp->numHeaders(),
                               True);
  if(trace_index < 0) return;


  if(!_is_gvs)//Constant velocity stack in use
    {
    found = findArrayMatch(velocity, 1.0, 0.0, 1.0,
                           &hd[VELHEADER], NULL, 
                           _sp->plottedFrames(), (int)_sp->numHeaders() * 
                           (int)_sp->originalTraces(), False);
    }
  else//Graded velocity stack in use
    {
    if(_reference_file_option)
      error = populateReferenceFileVelocity(x, y);
    else
      error = populateGvsFileVelocity(x, y);
    if(error)
        return;

    found = -1;
    sample = (long) ((time - _sp->memTmin()) / _sp->srval());
    reference_velocity = _trace_velocities[sample];

    //Find minimum and maximum velocity in gvs at the time that was found
    //and set image to appropriate panel if the velocity is less than
    //or greater than that in data.
    maximum_vel = 0.0;
    minimum_vel = 99999999.0;
    for(i = 0; i < _sp->plottedFrames(); i++)
      {
      if(_vmod_type == ADDITIVE)
        current_vel = reference_velocity + hd[i*_sp->numHeaders() * 
                                            _sp->originalTraces()+VELHEADER]; 
      else
        current_vel = reference_velocity * hd[i*_sp->numHeaders() * 
                                            _sp->originalTraces()+VELHEADER];
      if(minimum_vel > current_vel)
        { 
        minimum_vel = current_vel;
        min_panel_id = i;
        }
      if(maximum_vel < current_vel) 
        {
        maximum_vel = current_vel;
        max_panel_id = i;
        }
      }
    if(minimum_vel > velocity) found = min_panel_id;
    if(maximum_vel < velocity) found = max_panel_id;


    //If not found in above loop velocity is within the data set
    if(found < 0)
      {
      for(i = 0; i < _sp->plottedFrames(); i++)
        {
        if(_vmod_type == ADDITIVE)
          {
          current_vel = reference_velocity + hd[i*_sp->numHeaders() * 
                                              _sp->originalTraces()+VELHEADER];
          if(i < _sp->plottedFrames() - 1)
            next_vel =  reference_velocity + hd[(i+1)*_sp->numHeaders() * 
                                              _sp->originalTraces()+VELHEADER];
          else
            next_vel = current_vel;
          }
        else
          {
          current_vel = reference_velocity * hd[i* _sp->numHeaders() * 
                                              _sp->originalTraces()+VELHEADER];
          if(i < _sp->plottedFrames() - 1)
            next_vel =  reference_velocity * hd[(i+1)*_sp->numHeaders() * 
                                             _sp->originalTraces()+VELHEADER];
          else
            next_vel = current_vel;
          }
        if(next_vel < current_vel)
          {
          temp_vel = current_vel;
          current_vel = next_vel;
          next_vel = temp_vel;
          }
        if(velocity >= current_vel && velocity <= next_vel)
          {
          diff_current = fabs(velocity - current_vel);
          diff_next    = fabs(velocity - next_vel);
          if(diff_current < diff_next)
            found = i;
          else
            found = i + 1;
          i = _sp->plottedFrames();
          }
        }
      }
    if(found < 0) return;
    }//End graded velocity loop



  //copy in nearest panel
  if(_sp->currentFrame() != found) _sp->movieToFrame((int)found);

}


//============================================================================
//== This is a temporary way to set va to use migrated panels. It is called
//== from the option pull down in va_application.cc
//============================================================================
void VaGvsPlot::setGvsReferenceFileOption(int set)
{
  _reference_file_option = set;
  if(set)
   _is_gvs = True;
}

//============================================================================
//== This is a temporary and SLOW way to get the velocity for migrated panels.
//== This will be replaced when the new CPS handles the velocities for us.
//============================================================================
int VaGvsPlot::populateReferenceFileVelocity(float x, float y)
{
int error = 0;
VfDataset *refset;
float *hd = _sp->getHeaderArrayForUpdate();


 if(hd == NULL) return (error = 1);

  refset = (VfDataset *)_vf_manager->referenceDataset();
  if(refset == NULL || refset == _vf_manager->activeDataset())
    {
    errorPopup("You must read in a reference velocity file\n");
    return (error = 1);
    }

  if(_trace_velocities == NULL)
    {
    _trace_velocities = (float *) 
              calloc(1,(unsigned int)_sp->samplesPerTrace() * sizeof(float));
    }

  //Dont call velToFloat any more than we have to since it is slow
  if(x != _trace_velocities_xloc || y != _trace_velocities_yloc)
    {
    _trace_velocities_xloc = x;
    _trace_velocities_yloc = y;
    refset->velToFloat(x, y, _sp->memTmin(),   _sp->memTmax(),
                       _sp->samplesPerTrace(), _trace_velocities,  
                       VTNM, True);

    //This is dangerous, but we will do it until the new CPS provides this info
    if(hd[VELHEADER] <  10.0F)
      _vmod_type = MULTIPLICATIVE;
    else
      _vmod_type = ADDITIVE;
    }

  return (error = 0);
}


//============================================================================
//== Get velocity from a user's gvs file found in the trot file
//============================================================================
int VaGvsPlot::populateGvsFileVelocity(float x, float y)
{
int error = 0;
float *hd = _sp->getHeaderArrayForUpdate();


  if(_gvs_dataset == NULL)
    {
    errorPopup("Error getting velocities from gvs file\n");
    return (error = 1);
    }

  if(_trace_velocities == NULL)
    {
    _trace_velocities = (float *) 
              calloc(1,(unsigned int)_sp->samplesPerTrace() * sizeof(float));
    }

  //Dont call velToFloat any more than we have to since it is slow
  if(x != _trace_velocities_xloc || y != _trace_velocities_yloc)
    {
    _trace_velocities_xloc = x;
    _trace_velocities_yloc = y;
    _gvs_dataset->velToFloat(x, y, _sp->memTmin(),   _sp->memTmax(),
                       _sp->samplesPerTrace(), _trace_velocities,  
                       VTNM, True);
    }

  return (error = 0);
}













//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//========================================================================
//==================  Informs only after this line  ======================
//========================================================================
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

void VaGvsPlot::postModifyPicks(VfDataset *vfd, long ifun, 
                                int type,       long ipick, 
                                long /*nrem*/,  long /*nins*/)
{
float x,y;
float time,velocity;

  if(!_able_to_plot) return;

  if(vfd != _vf_manager->activeDataset()) return;

  ipick = vfd->getActivePick(ifun);
  //The following changed by MLS 09/99. If picking cvst loops
  //and the first pick is made at a new location the VaGvsPickData class
  //will call changePanel appropriately but then this method gets called
  //because a pick is automatically made on new locations at zero time.
  //That will cause the call here to changePanel to put the panel back
  //to the first panel which we dont want.
  //if(ipick < 0) return;
  if(ipick < 1) return;

  time = vfd->getAbscissa(ifun, ipick, type);
  velocity = vfd->getOrdinate(ifun, ipick, type);
  x = vfd->getXloc(ifun);
  y = vfd->getYloc(ifun); 

  changePanel(x,y,time,velocity);
}



void VaGvsPlot::postNewActiveVelocityFunction (VfDataset *vfd)
{
float x,y;

/* ehs */
//  if(vfd != _vf_manager->activeDataset()) return;

  if(!_able_to_plot) return;

  if((vfd != _vf_manager->activeDataset())    || 
     (vfd->getActiveVelocityFunction() == -1) ||
     (!_sp->isPlotDisplayed())   ) return;

  x = vfd->getXloc(vfd->getActiveVelocityFunction());
  y = vfd->getYloc(vfd->getActiveVelocityFunction()); 

  centerPanel(x,y);
}

