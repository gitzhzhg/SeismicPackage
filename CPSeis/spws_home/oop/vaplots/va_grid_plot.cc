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
//========== Grid plot class                                ==========
//========== Author Michael L. Sherrill 09/97                    ==========
//=========================================================================

// $Id: va_grid_plot.cc,v 1.2 2004/04/05 18:12:11 wjdone Exp $
// $Name:  $

#include "vaplots/va_plot.hh"
#include "vaplots/va_grid_plot.hh"
#include "vaplots/va_grid_gui.hh"
#include "vaplots/va_vect_colors.hh"
#include "vaplots/va_grid_picks.hh"
#include "sp/seis_plot.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_utilities.hh"
#include "vf/vf_dataset.hh"
#include "vf/vf_constants.hh"

VaGridPlot::VaGridPlot(VfManager          *vf_manager, 
                       class VfHorizons   *horizons,
                       Widget             w, 
                       char               *name,
                       HelpCtx            hctx,
                       VaVectColors       *vect_colors)
                     : VaPlot(vf_manager, w, name)
{
  _va_picks = new VaGridPicks(vf_manager, horizons, this, _sp, vect_colors);
  _gui     = new VaGridGui(w, "Grid Gui", hctx, this);
  _plot_type = VGRID;
  _number_locations_in_file = 0;
  _sp->setLeftBorder(91);
}

VaGridPlot::~VaGridPlot()
{
  delete _va_picks;
}

//=========================================================================
//================== New seis plot in window  =============================
//=========================================================================
void VaGridPlot::notCurrentInWindow(SeisPlot *sp)
{
  addSeisPlot(sp->currentSPInWindow());
  _sp = sp->currentSPInWindow();
  _gui->seisPlotChanged();
 //may need to notify picker via _va_picks here 
}

//===========================================================================
//============= Return ptr to the dialog                       ==============
//===========================================================================
SLShellContainer *VaGridPlot::getDialog()
{
  return _gui;
}

void VaGridPlot::manageGui()
{
  _gui->makeAndManage();
}



void VaGridPlot::updateGui(Boolean update_file_limits)
{
  _gui->updateParams(update_file_limits);
}



int VaGridPlot::plot()
{
int stat;

  if(!needsReplotting()) return PlotImage::PlotSuccess;  
  //if(!_able_to_plot) return PlotImage::ReadFail;

  setPlotParameters();
  stat = _sp->plot();
  if(stat == PlotImage::PlotSuccess)
    {
    _sp->backingStore(True); 
    }
  return stat;

}


//===========================================================================
//============= Get velocity file information                  ==============
//===========================================================================
int VaGridPlot::getFileParameters(char * /*filename*/)
{
long stat = PlotImage::DEFS_FAIL;
VfDataset *vfd = _vf_manager->activeDataset(); 
int i;

  _sp->allocateVelocityArray(MAX_LOCATIONS);

  _number_locations_in_file = vfd->numVelocityFunctions();

  allocateLocationArrays((int)_number_locations_in_file);

  _min_xbin = vfd->minimumXloc();
  _max_xbin = vfd->maximumXloc();
  _min_ybin = vfd->minimumYloc();
  _max_ybin = vfd->maximumYloc();
  
  for(i = 0; i < _number_locations_in_file; i++)
    {
    _xlocations[i] = vfd->getXloc(i);
    _ylocations[i] = vfd->getYloc(i);
    }

  updateGui();

  dataReadByXYheaders(getActiveXheader(),getActiveYheader());

  return stat;  
}




void VaGridPlot::setPlotParameters()
{

  _gui->setParameters();

}


//===========================================================================
//===================== INFORMS AREA ========================================
//===========================================================================
void VaGridPlot::postNewActiveDataset()
{
  getFileParameters("");
  if(_sp->imageIsDisplayed()) plot();
}

void VaGridPlot::postChangeCoords(VfDataset *, long, long )
{
  getFileParameters("");
  plot();
}

void VaGridPlot::postTotalChanges(VfDataset *)
{
  //not used yet. this call usually happens on new files coming in
  //and the application will make the plot if requested
  // getFileParameters(""); 
  //plot(); 
}

void VaGridPlot::postRemoveInsertVelocityFunctions(VfDataset *vfd,
                                                   long /*ifun*/, long, long)
{
  if(vfd != _vf_manager->activeDataset()) return;
  if( _number_locations_in_file   == 0 ||
      vfd->numVelocityFunctions() == 0 ) return;

  //Replot only if the new coordinates are outside of the existing plot area
  if(vfd->minimumXloc() < _min_xbin ||
     vfd->maximumXloc() > _max_xbin ||
     vfd->minimumYloc() < _min_ybin ||
     vfd->maximumYloc() > _max_ybin   )
    {
    getFileParameters("");
    if (_sp->imageIsDisplayed()) plot();
    }
}
