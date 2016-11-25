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

// $Id: va_iso_plot.cc,v 1.2 2004/04/05 18:12:11 wjdone Exp $
// $Name:  $

#include "vaplots/va_plot.hh"
#include "vaplots/va_plot_control.hh"
#include "vaplots/va_iso_plot.hh"
#include "vaplots/va_iso_gui.hh"
#include "vaplots/va_vect_colors.hh"
#include "vaplots/va_iso_picks.hh"
#include "vaplots/va_iso_plotter.hh"
#include "sp/seis_plot.hh"
#include "vf/vf_utilities.hh"
#include "vf/vf_dataset.hh"
#include "vf/vf_constants.hh"
#include "vf/vf_manager.hh"

#include "cprim.h"




VaIsoPlot::VaIsoPlot(VfManager        *vf_manager, 
                     class VfHorizons *horizons,
                     Widget           w, 
                     char             *name, 
                     HelpCtx          hctx,
                     VaVectColors     *vect_colors,
                     long             numcolors)
                     : VaPlot(vf_manager, w, name)
{
  _numcolors = numcolors;
  _va_picks = new VaIsoPicks(vf_manager, horizons, this, _sp, vect_colors);
  _gui = new VaIsoGui(w, "Iso Gui", hctx, this);
  if(_numcolors) _sp->setLoadableColors((int)_numcolors);
  _sp->setContourColors(VaPlot::NUM_CONTOUR_COLORS);
  _sp->allocateVelocityArray(MAX_LOCATIONS);   
  _sp->setLeftBorder(91);
  _plot_type = ISO;
  _plotted_line_type = VaIsoPlot::INLINE;
}

VaIsoPlot::~VaIsoPlot()
{
  delete _va_picks;
}

//=========================================================================
//================== New seis plot in window  =============================
//=========================================================================
void VaIsoPlot::notCurrentInWindow(SeisPlot *sp)
{
  addSeisPlot(sp->currentSPInWindow());
  _sp = sp->currentSPInWindow();
  _gui->seisPlotChanged();
  //may need to notify picker via _va_picks here
}

//===========================================================================
//============= Return ptr to the dialog                       ==============
//===========================================================================
SLShellContainer *VaIsoPlot::getDialog()
{
  return _gui;
}

void VaIsoPlot::manageGui()
{
  _gui->makeAndManage();
}


void VaIsoPlot::updateGui(Boolean update_file_limits)
{
  _gui->updateParams(update_file_limits);
}


int VaIsoPlot::plot()
{
int stat;
int oldlinetype;


  if(!needsReplotting()) return PlotImage::PlotSuccess;  

  if(!_able_to_plot) return PlotImage::ReadFail;

  setPlotParameters();

  oldlinetype = _plotted_line_type;

  stat = _plotter->plot(_sp);
  if(stat)
    _plotted_line_type = _gui->getLineType(); 
  else
    _plotted_line_type = oldlinetype;

  return stat;

}


//Public method to externally change the displayed plots
Boolean VaIsoPlot::changePlot(float x1, float x2, float y1, float y2,
                              Boolean refresh_only)
{
Boolean stat;

  stat = _plotter->changePlot(x1,x2,y1,y2, refresh_only);

  if(!stat) errorPopup("Error in generating iso plot\n");

  return stat;
}



int VaIsoPlot::getLineType()
{
  return _gui->getLineType();
}

int VaIsoPlot::getPlottedLineType()
{
  return _plotted_line_type;
}

int VaIsoPlot::getPlottedVelocityType()
{
  return _gui->getPlottedVelType();
}

float VaIsoPlot::getTimeOfFrame(long frame_index)
{
  assert(getPlottedLineType() == VaIsoPlot::TIMESLICE);
  return _gui->getTimeOfFrame(frame_index);
}

//===========================================================================
//============= Get velocity file information                  ==============
//============= Should only be called by user file changes     ==============
//===========================================================================
int VaIsoPlot::getFileParameters(char *filename)
{
long stat;

  strcpy(_filename,filename);

  stat = _plotter->getFileParameters(filename);
  
  return stat;
}
  

void VaIsoPlot::initialize()
{
  _number_locations_in_file = _plotter->getNumberLocationsInFile();
  _tmin                     = _plotter->getTmin();
  _tmax                     = _plotter->getTmax();
  _velocity_min             = _plotter->getVelMin();
  _velocity_max             = _plotter->getVelMax();
  _min_xbin                 = _plotter->getFirstXbin();
  _max_xbin                 = _plotter->getLastXbin();
  _min_ybin                 = _plotter->getFirstYbin();
  _max_ybin                 = _plotter->getLastYbin();
}


void VaIsoPlot::setPlotParameters()
{

  _gui->setParameters();

}


void VaIsoPlot::setDontPlotYet(Boolean dont_plot_yet)
{

  _gui->setDontPlotYet(dont_plot_yet);

}


void VaIsoPlot::showIsoWindow(Boolean show)
{
   _plot_control->showIsoWindow(show);
}


//===========================================================================
//=========================  INFORMS ========================================
//===========================================================================
void VaIsoPlot::postNewActiveDataset()
{
  if(_sp->imageIsDisplayed())
    {
    getFileParameters("Filename");
    plot();
    }
}
