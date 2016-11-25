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
//========== Crossplot plot class                                ==========
//========== Author Michael L. Sherrill 09/97                    ==========
//=========================================================================

// $Id: va_crossplot_plot.cc,v 1.2 2004/04/05 18:12:11 wjdone Exp $
// $Name:  $

#include "vaplots/va_plot.hh"
#include "vaplots/va_crossplot_plot.hh"
#include "vaplots/va_crossplot_gui.hh"
#include "vaplots/va_vect_colors.hh"
#include "vaplots/va_crossplot_picks.hh"
#include "vaplots/va_common_params.hh"
#include "sp/seis_plot.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_utilities.hh"
#include "vf/vf_dataset.hh"
#include "vf/vf_constants.hh"


VaCrossplotPlot::VaCrossplotPlot(VfManager          *vf_manager, 
                                 class VfHorizons   *horizons,
                                 Widget             w, 
                                 char               *name,
                                 HelpCtx            hctx,
                                 VaPlotCommonParams *common_params,
                                 VaVectColors       *vect_colors)
                               : VaPlot(vf_manager, w, name)
{
  _va_picks  = new VaCrossplotPicks(vf_manager, horizons, this, _sp,
                                    vect_colors);
  setCommonParams(common_params);
  _gui       = new VaCrossplotGui(w, "Crossplot Gui", hctx, this);
  _plot_type = CROSSPLOT;
}

VaCrossplotPlot::~VaCrossplotPlot()
{
  if(_va_picks) delete _va_picks;
}

//=========================================================================
//================== New seis plot in window  =============================
//=========================================================================
void VaCrossplotPlot::notCurrentInWindow(SeisPlot *sp)
{
  addSeisPlot(sp->currentSPInWindow());
  _sp = sp->currentSPInWindow();
  _gui->seisPlotChanged();
 //may need to notify picker via _va_picks here 
}

//===========================================================================
//============= Return ptr to the dialog                       ==============
//===========================================================================
SLShellContainer *VaCrossplotPlot::getDialog()
{
  return _gui;
}

void VaCrossplotPlot::manageGui()
{
  _gui->makeAndManage();
}



void VaCrossplotPlot::updateGui(Boolean update_file_limits)
{
  _gui->updateParams(update_file_limits);
}



int VaCrossplotPlot::plot()
{
int stat;

  if(!needsReplotting()) return PlotImage::PlotSuccess;  
  if(!_able_to_plot) return PlotImage::ReadFail;

  setPlotParameters();
  stat = _sp->plot();
  if(stat == PlotImage::PlotSuccess)
    {
    _sp->backingStore(True);
    _common_params->setCrossplotReplot(False); 
    }
  return stat;

}


//===========================================================================
//============= Get velocity file information                  ==============
//===========================================================================
int VaCrossplotPlot::getFileParameters(char * /*filename*/)
{
long stat = PlotImage::DEFS_FAIL;
VfDataset *vfd = _vf_manager->activeDataset(); 
int i;

//most of this will probably not be used

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




void VaCrossplotPlot::setPlotParameters()
{

  _gui->setParameters();

}
