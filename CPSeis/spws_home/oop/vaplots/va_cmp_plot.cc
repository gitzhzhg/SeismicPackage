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
//========== Cmp plot class                                      ============
//========== Author Michael L. Sherrill 09/97                    ============
//===========================================================================

// $Id: va_cmp_plot.cc,v 1.2 2004/04/05 18:12:11 wjdone Exp $
// $Name:  $

#include "vaplots/va_plot.hh"
#include "vaplots/va_cmp_plot.hh"
#include "vaplots/va_cmp_gui.hh"
#include "vaplots/va_eta_plot.hh"
#include "vaplots/va_vect_colors.hh"
#include "vaplots/va_cmp_picks.hh"
#include "vaplots/va_common_params.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "vf/vf_moveout.hh"
#include "sp/seis_plot.hh"



VaCmpPlot::VaCmpPlot(VfManager          *vf_manager, 
                     class VfHorizons   *horizons,
                     Widget w,          char *name,
                     HelpCtx            hctx,
                     VaPlotCommonParams *common_params,
                     VaVectColors       *vect_colors,
                     long               numcolors)
                   : VaPlot(vf_manager, w, name)
{
  _numcolors = numcolors;
  _va_picks = new VaCmpPicks(vf_manager, horizons, this, _sp, vect_colors);
  setCommonParams(common_params);
  _gui = new VaCmpGui(w,"Cmp Gui", hctx, this);
  if(_numcolors) _sp->setLoadableColors((int)_numcolors);
  _doppler = 1.7;
  _vf_moveout = new VfMoveout();
  for(int i = 0; i < MAX_PIXMAP; i++)_nmc_applied[i] = False;
  _nmc_mode = FORWARD_NMC;
  _plot_type = CMP;
  _modify_nmc_frame = -1;
}

VaCmpPlot::~VaCmpPlot()
{
 delete _va_picks; 
 delete _vf_moveout;
}

//=========================================================================
//================== New seis plot in window  =============================
//=========================================================================
void VaCmpPlot::notCurrentInWindow(SeisPlot *sp)
{
  addSeisPlot(sp->currentSPInWindow());
  _sp = sp->currentSPInWindow();
  _gui->seisPlotChanged();
  //may need to notify picker via _va_picks here
}


//===========================================================================
//============= Return ptr to the dialog                       ==============
//===========================================================================
SLShellContainer *VaCmpPlot::getDialog()
{
  return _gui;
}


//===========================================================================
//============= Manage the gui                                 ==============
//===========================================================================
void VaCmpPlot::manageGui()
{
  _gui->makeAndManage();
}


//===========================================================================
//============= Update the gui                                 ==============
//===========================================================================
void VaCmpPlot::updateGui(Boolean update_file_limits)
{
  _gui->updateParams(update_file_limits);
}


//===========================================================================
//============= Plot the data                                  ==============
//===========================================================================
int VaCmpPlot::plot()
{
int i, stat;
Boolean reread_data = False;

  if(!needsReplotting()) return PlotImage::PlotSuccess;  
  if(!_able_to_plot) return PlotImage::ReadFail;

  setPlotParameters();

  //If nmc has been applied make sure the data is reread
  for(i = 0; i < MAX_PIXMAP; i++) 
    {
    if(_nmc_applied[i])
      {
      reread_data = True;
      i = MAX_PIXMAP;
      }
    }

  if(reread_data) _sp->forceRereadOfData();

  for(i = 0; i < MAX_PIXMAP; i++) _nmc_applied[i] = False;

  stat = _sp->plot();
  if(stat == PlotImage::PlotSuccess)
    {
    _sp->backingStore(True);
    _common_params->setCmpReplot(False);
    if(_gui->applyAutomaticNmc())
      {
      _nmc_mode = FORWARD_NMC;
      applyMoveout();
      }
    }


  return stat;

}


//===========================================================================
//============= Set parameters on the gui                      ==============
//===========================================================================
void VaCmpPlot::setPlotParameters()
{

  _gui->setParameters();

}


//===========================================================================
//=============== Public method to determine nmc state         ==============
//===========================================================================
Boolean VaCmpPlot::nmcApplied()
{
  return _nmc_applied[_sp->currentFrame()];
}

//===========================================================================
//============= Apply forward moveout                          ==============
//===========================================================================
void VaCmpPlot::applyForwardMoveout()
{

  _nmc_mode = FORWARD_NMC;
  applyMoveout();

}

//===========================================================================
//============= Apply reverse moveout                          ==============
//===========================================================================
void VaCmpPlot::applyReverseMoveout()
{

  _nmc_mode = REVERSE_NMC;
  applyMoveout();

}

//===========================================================================
//============= Apply moveout                                  ==============
//===========================================================================
void VaCmpPlot::applyMoveout()
{
long stat;
VfDataset *vfd = _vf_manager->activeDataset(); 
VfDataset *etavfd; 
int order;
int previous_nmc_mode = _nmc_mode;
float dop = ((VaCmpPicks*)_va_picks)->getDopplerMuteParameter();
long func_index;
long aryoffset;
unsigned char *image_data;
float *image_float_data;

  if(!_sp->imageIsDisplayed()) return;

  if(_eta_plot->isActivated())
    {
      etavfd = _eta_manager->activeDataset();
      order = VfMoveout::ORDER_2_PLUS_ETA;
/**********************************************
      ///// other possibilities:
      order = VfMoveout::ORDER_ETA;
      order = VfMoveout::ORDER_ETA4;
      order = VfMoveout::ORDER_2_PLUS_ETA4;
      order = VfMoveout::ORDER_2_PLUS_4;
**********************************************/
    }
  else
    {
      etavfd = NULL;
      order = VfMoveout::ORDER_2_OR_4;   // uses nhosign and nhoexp to decide.
/**********************************************
      ///// other possibilities:
      order = VfMoveout::ORDER_2;
      order = VfMoveout::ORDER_4;
**********************************************/
    }

  func_index = vfd->findMatchingVelfun(getDisplayedXbin(), getDisplayedYbin());

  if(func_index < 0 || vfd->numPicks(func_index) == 0) return;

  if(_nmc_applied[_sp->currentFrame()] == True && _nmc_mode != REVERSE_NMC)
     _nmc_mode = REAPPLY_NMC;

  if(_nmc_mode == REVERSE_NMC && _nmc_applied[_sp->currentFrame()] == False)
    return;


  if( _sp->isByteData() )
    {
    image_data = _sp->firstMemoryByteTraceDataForUpdate();
    aryoffset  = _sp->currentFrame() * _sp->originalTraces() *
                 _sp->samplesPerTrace();
    stat = _vf_moveout->doMoveout(vfd, func_index, _nmc_mode, dop,
                        &image_data[aryoffset], 
            /////       _sp->firstMemoryHeaderData(),
                        _sp->firstMemoryHeaderDataForUpdate(),
                        _sp->samplesPerTrace(), 
                        _sp->numHeaders(), 
                        _sp->displayedTraces(_sp->getCurrentPanel()),
                        _sp->memTmin(),  _sp->srval(), etavfd, order);
    }
  else//float data
    {
    image_float_data = _sp->firstMemoryFloatTraceDataForUpdate();
    aryoffset  = _sp->currentFrame() * _sp->originalTraces() *
                 _sp->samplesPerTrace();
    stat = _vf_moveout->doMoveout(vfd, func_index, _nmc_mode, dop,
                        &image_float_data[aryoffset], 
            /////       _sp->firstMemoryHeaderData(),
                        _sp->firstMemoryHeaderDataForUpdate(),
                        _sp->samplesPerTrace(), 
                        _sp->numHeaders(),
                        _sp->displayedTraces(_sp->getCurrentPanel()),
                        _sp->memTmin(),  _sp->srval(), etavfd, order);
    }



  if(stat)//nmc failed
    {
    _nmc_mode = previous_nmc_mode;
    return;
    }

  stat = _sp->redrawImage(_sp->currentFrame());

  if(stat != PlotImage::PlotSuccess) return;

  if(_nmc_mode >= REAPPLY_NMC) 
    {
    _nmc_applied[_sp->currentFrame()] = True;
    }
  else
    {
    _nmc_applied[_sp->currentFrame()] = False;
    }

}


long VaCmpPlot::modifyMoveout(int direction, long func_index,
                              long frame, Boolean redraw)
{
unsigned char *image_data;
float *image_float_data;
float *header_data;
long aryoffset, hdroffset;
long stat;
float dop = ((VaCmpPicks*)_va_picks)->getDopplerMuteParameter();
VfDataset *vfd = _vf_manager->activeDataset();
VfDataset *etavfd; 
int order;

   

  header_data = _sp->getHeaderArrayForUpdate();

  aryoffset = frame * _sp->originalTraces() * _sp->samplesPerTrace();
  hdroffset = frame * _sp->originalTraces() * _sp->numHeaders();


  if(_eta_plot->isActivated())
    {
      etavfd = _eta_manager->activeDataset();
      order = VfMoveout::ORDER_2_PLUS_ETA;
/**********************************************
      ///// other possibilities:
      order = VfMoveout::ORDER_ETA;
      order = VfMoveout::ORDER_ETA4;
      order = VfMoveout::ORDER_2_PLUS_ETA4;
      order = VfMoveout::ORDER_2_PLUS_4;
**********************************************/
    }
  else
    {
      etavfd = NULL;
      order = VfMoveout::ORDER_2_OR_4;   // uses nhosign and nhoexp to decide.
/**********************************************
      ///// other possibilities:
      order = VfMoveout::ORDER_2;
      order = VfMoveout::ORDER_4;
**********************************************/
    }

  if(_sp->isByteData())
    {
    image_data = _sp->firstMemoryByteTraceDataForUpdate();
    stat = _vf_moveout->doMoveout(vfd, func_index, direction, dop,
                        &image_data[aryoffset], 
                        &header_data[hdroffset], _sp->samplesPerTrace(), 
                        _sp->numHeaders(),
                        _sp->displayedTraces(_sp->getCurrentPanel()),
                        _sp->memTmin(),  _sp->srval(), etavfd, order);
    }
  else//float data
    {
    image_float_data = _sp->firstMemoryFloatTraceDataForUpdate();
    stat = _vf_moveout->doMoveout(vfd, func_index, direction, dop,
                        &image_float_data[aryoffset], 
                        &header_data[hdroffset], _sp->samplesPerTrace(), 
                        _sp->numHeaders(), 
                        _sp->displayedTraces(_sp->getCurrentPanel()),
                        _sp->memTmin(),  _sp->srval(), etavfd, order);
    }


  if(redraw)
    stat = _sp->redrawImage(frame);
  else
    stat = PlotImage::PlotSuccess;

  return stat;
}

//===========================================================================
//============= Get trace file information                     ==============
//===========================================================================
int VaCmpPlot::getFileParameters(char *filename)
{
long stat = PlotImage::DEFS_FAIL;
long xloc_hdr = getActiveXheader();
long yloc_hdr = getActiveYheader();
int user_aborted = 0;
long max_traces = 0;


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
    stat= _sp->findTracesByPattern(_sp->filename(), xloc_hdr, yloc_hdr,
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


  _tmin = _sp->minTmin();
  _tmax = _sp->maxTmax();

  //Next line added by MLS 05/29/01. When we have a panel with nmc
  //applied then read in a new file the preChangeMovieoutOrder method
  //is called and it tries to apply reverse nmc on cmp data that is no
  //longer valid. 
  for(int i = 0; i < MAX_PIXMAP; i++)_nmc_applied[i] = False;

  updateGui();

  dataReadByXYheaders(getActiveXheader(),getActiveYheader());


  return stat;  
}


//=========================================================================
//============= Change movie panel or read from file            ===========
//=========================================================================
void VaCmpPlot::changePanel(float x, float y, 
                            float /*time*/, float /*velocity*/)
{
int stat = PlotImage::PlotSuccess;
int frame;

      if(!_able_to_plot) return;

      if(!_sp->isPlotDisplayed()) return;
  
      //see if we can movie to new location
      if( (frame = getDisplayedPanelIndexFromXY(x, y))  >= 0 && 
          _blank_image == False)
        {
        if(_gui->applyAutomaticNmc() && nmcApplied() == False)
          {
          _nmc_mode = FORWARD_NMC;
          applyMoveout();        
          }
        if(_sp->currentFrame() != frame) 
          _sp->movieToFrame(frame);
        return;
        }

      //Movie didnt match so see if we can read from file
      if( (frame = getFilePanelIndexFromXY(x, y)) >= 0)
        {
        _first_trace = frame * _sp->originalTraces() + 1;

        updateGui(False);

        stat = plot();

        if(stat == PlotImage::PlotSuccess && _sp->movie())
          if( (frame = getDisplayedPanelIndexFromXY(x, y))  >= 0)
            if(_sp->currentFrame() != frame) _sp->movieToFrame(frame);

        _blank_image = False;

        if(_gui->applyAutomaticNmc())
           applyMoveout();

        return;
        }

      //Location is not in file so blank the image out
      _sp->blankImage(x,y);
      _blank_image = True;
}


//=========================================================================
//============= Get number of cmp gathers in the file     =================
//=========================================================================
long VaCmpPlot::getNumberPanelsInFile()
{
  return _number_locations_in_file;
}

//=========================================================================
//============= Get xlocation of a gather in the file     =================
//=========================================================================
const float VaCmpPlot::getPanelXlocation(long i)
{
  return getXlocation(i);
}

//=========================================================================
//============= Get xlocation of a gather in the file     =================
//=========================================================================
const float VaCmpPlot::getPanelYlocation(long i)
{
  return getYlocation(i);
}
//=========================================================================
//============= Get number movie panels displayed               ===========
//=========================================================================
long VaCmpPlot::getMovieTotalPanels()
{
  return _sp->plottedFrames();
}

//=========================================================================
//============= Get first movie panel displayed                 ===========
//=========================================================================
long VaCmpPlot::getMovieFirstPanel()
{
  return _gui->getFirstMoviePanel();
}

//=========================================================================
//============= Get skipped panels                              ===========
//=========================================================================
long VaCmpPlot::getMovieSkipPanels()
{
  return _gui->getSkipMoviePanels();
}

//=========================================================================
//============= Set first panel to plot                         ===========
//=========================================================================
void VaCmpPlot::setFirstPanelToPlot(long first_panel)
{
  _gui->setFirstPanelToPlot(first_panel);
}


//=========================================================================
//============= Externally call methods to modify the moveout   ===========
//=========================================================================
void VaCmpPlot::externalModifyForwardMoveout()
{
VfDataset *vfd = _vf_manager->activeDataset(); 

  if(!_able_to_plot) return;
 
  if(!_sp->isPlotDisplayed()) return;

  long ifun = vfd->getActiveVelocityFunction();

  long modify_nmc_frame = 
       getDisplayedPanelIndexFromXY(vfd->getXloc(ifun),vfd->getYloc(ifun));
  if(modify_nmc_frame < 0) return;

  modifyMoveout(FORWARD_NMC, ifun, modify_nmc_frame, True);
  _nmc_applied[modify_nmc_frame] = True;
}


//=========================================================================
//============= Externally call methods to modify the moveout   ===========
//=========================================================================
void VaCmpPlot::externalModifyReverseMoveout()
{
VfDataset *vfd = _vf_manager->activeDataset(); 
long ifun = vfd->getActiveVelocityFunction();
float xloc = vfd->getXloc(ifun);
float yloc = vfd->getYloc(ifun);

  if(!_able_to_plot) return; 

  if(!_sp->isPlotDisplayed()) return;

  long modify_nmc_frame = 
       getDisplayedPanelIndexFromXY(vfd->getXloc(ifun),vfd->getYloc(ifun));
  if(modify_nmc_frame < 0) return;

  if(_nmc_applied[modify_nmc_frame])
    { 
      modifyMoveout(REVERSE_NMC, ifun, modify_nmc_frame, False);
      _nmc_applied[modify_nmc_frame] = False;
    }
}




//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//===========================================================================
//============  Informs only after this point                ================
//===========================================================================
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

void VaCmpPlot::postNewActiveVelocityFunction (VfDataset *vfd)
{
float x,y;

  if(!_sp->isPlotDisplayed()) return;

  if(!_able_to_plot) return;

  if((vfd != _vf_manager->activeDataset())
  || (vfd->getActiveVelocityFunction() == -1)) return;

  x = vfd->getXloc(vfd->getActiveVelocityFunction());
  y = vfd->getYloc(vfd->getActiveVelocityFunction()); 

  changePanel(x,y);
}


void VaCmpPlot::preModifyPicks(VfDataset *vfd, long ifun,
                               int /*type*/, long /*ipick*/, long /*nrem*/)
{
float xloc = vfd->getXloc(ifun);
float yloc = vfd->getYloc(ifun);

  if(!_able_to_plot) return; 

  if(!_sp->isPlotDisplayed()) return;

  if(vfd != _vf_manager->activeDataset()) return;

  _modify_nmc_frame = 
       getDisplayedPanelIndexFromXY(vfd->getXloc(ifun),vfd->getYloc(ifun));
  if(_modify_nmc_frame < 0) return;

  if(_nmc_applied[_modify_nmc_frame]) 
     modifyMoveout(REVERSE_NMC, ifun, _modify_nmc_frame, False);
  else
     _modify_nmc_frame = -1;

}


void VaCmpPlot::postModifyPicks(VfDataset *vfd, long ifun, 
                                int /*type*/,   long /*ipick*/,
                                long /*nrem*/,  long /*nins*/)
{
  if(!_able_to_plot) return;
 
  if(!_sp->isPlotDisplayed()) return;

  if(vfd != _vf_manager->activeDataset()) return;

  if(_modify_nmc_frame >= 0)
    {
    modifyMoveout(FORWARD_NMC, ifun, _modify_nmc_frame, True);
    _modify_nmc_frame = -1;
    }
}


void VaCmpPlot::preChangeCoords(VfDataset *vfd, long ifun, long /*nchng*/)
{
  if(!_able_to_plot) return;

  if(!_sp->isPlotDisplayed()) return;

  if(vfd != _vf_manager->activeDataset()) return;

  _modify_nmc_frame = 
       getDisplayedPanelIndexFromXY(vfd->getXloc(ifun),vfd->getYloc(ifun));
  if(_modify_nmc_frame < 0) return;

  if(_nmc_applied[_modify_nmc_frame]) 
    {
    modifyMoveout(REVERSE_NMC, ifun, _modify_nmc_frame, True);
    _nmc_applied[_modify_nmc_frame] = False;
    }
  _modify_nmc_frame = -1;
}

void VaCmpPlot::postChangeCoords(VfDataset *vfd, long ifun, long /*nchng*/)
{
  if(!_able_to_plot) return; 

  if(!_sp->isPlotDisplayed()) return;

  if(vfd != _vf_manager->activeDataset()) return;

  changePanel(vfd->getXloc(ifun),vfd->getYloc(ifun));
}

void VaCmpPlot::preRemoveInsertVelocityFunctions (VfDataset *vfd,
                                        long ifun, long nrem, long nins)
{
  if(!_able_to_plot) return;

  if(!_sp->isPlotDisplayed()) return;

  if(vfd != _vf_manager->activeDataset()) return;
  if(nrem < 1 && nins > 0) return; //inserting only

  _modify_nmc_frame = 
       getDisplayedPanelIndexFromXY(vfd->getXloc(ifun),vfd->getYloc(ifun));
  if(_modify_nmc_frame < 0) return;

  if(_nmc_applied[_modify_nmc_frame]) 
    {
    modifyMoveout(REVERSE_NMC, ifun, _modify_nmc_frame, True); 
    _nmc_applied[_modify_nmc_frame] = False;
    }

  _modify_nmc_frame = -1;
}



void VaCmpPlot::preChangeMoveoutOrder(VfDataset *vfd)
{
long i,j;
long func_index;

  if(!_able_to_plot) return;

  if(!_sp->isPlotDisplayed()) return;

  if(vfd != _vf_manager->activeDataset()) return;

  j = _gui->getFirstMoviePanel() - 1;
  for(i = 0; i < _sp->plottedFrames(); i++)
    {
    if(_nmc_applied[i])
      {
      func_index = vfd->findMatchingVelfun(_xlocations[j], _ylocations[j]);
      if(func_index >= 0)
        {
        modifyMoveout(REVERSE_NMC, func_index, i, True);
        _nmc_applied[i] = False;
        }
      }
    j += _gui->getSkipMoviePanels() + 1;
    }
  
}


void VaCmpPlot::preTotalChanges(VfDataset *vfd)
{
long i,j;
long func_index;

  if(!_able_to_plot) return;

  if(!_sp->isPlotDisplayed()) return;

  if(vfd != _vf_manager->activeDataset()) return;

  j = _gui->getFirstMoviePanel() - 1;
  for(i = 0; i < _sp->plottedFrames(); i++)
    {
    if(_nmc_applied[i])
      {
      func_index = vfd->findMatchingVelfun(_xlocations[j], _ylocations[j]);
      if(func_index >= 0) 
        {
        modifyMoveout(REVERSE_NMC, func_index, i, True);
        _nmc_applied[i] = False;
        }
      }
    j += _gui->getSkipMoviePanels() + 1;
    }

}


void VaCmpPlot::preNewActiveDataset()
{
long i,j;
long func_index;
VfDataset *vfd = _vf_manager->activeDataset();

  if(!_able_to_plot) return;

  if(!_sp->isPlotDisplayed()) return;

  j = _gui->getFirstMoviePanel() - 1;
  for(i = 0; i < _sp->plottedFrames(); i++)
    {
    if(_nmc_applied[i])
      {
      func_index = vfd->findMatchingVelfun(_xlocations[j], _ylocations[j]);
      if(func_index >= 0)
        {
        modifyMoveout(REVERSE_NMC, func_index, i, True);
        _nmc_applied[i] = False;
        }
      }
    j +=_gui->getSkipMoviePanels() + 1;
    }
}

void VaCmpPlot::postChangeBinTolerances()
{
VfDataset *vfd = _vf_manager->activeDataset();
long ifun = vfd->getActiveVelocityFunction();
  
  if(!_able_to_plot) return;

  if(!_sp->isPlotDisplayed()) return;

  if(ifun == -1) return;

  changePanel(vfd->getXloc(ifun),vfd->getYloc(ifun));
}
