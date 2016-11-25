// fgqc_compute_grid_pop.cc: implementation file for FgQcComputeGridPop class
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

#include "fgqc/fgqc_compute_grid_pop.hh"
#include "fgqc/fgqc_compute_grid.hh"
#include "fgqc/fgqc_pop.hh"
#include "fgqc/fgqc_plot.hh"
#include "fgqc/fgqc_plot_type.hh"
#include "sp/seis_plot.hh"
#include "sl/sl_error_pop.hh"

FgQcComputeGridPop::FgQcComputeGridPop (Widget p, char *name,
  FgQcComputeGrid *fcg, FgQcPlot *qcp, HelpCtx hctx) :
  ComputeGridPop (p, name, (ComputeGrid *)fcg, hctx),
  SeisInform (),
  _fcg              (fcg),
  _qcp              (qcp),
  _pickers          (0),
  _seis_plots       (0),
  _error_status     (FCGP_SUCCESSFUL)
{
}

FgQcComputeGridPop::~FgQcComputeGridPop ()
{
  if (_pickers) {
    int k2;
    for (k2 = 0; k2 < _picker_count; k2++) {
      if (_pickers[k2]) delete _pickers[k2];
    }
    delete _pickers;
  }
  if (_seis_plots) delete _seis_plots;
}

int FgQcComputeGridPop::selectGrid (SeisPlot *sp)
{
// using the PlotList of FgQcPlots found in the FgQcPop, find the FgQcPlot
//   associated with this FgQcComputeGridPop, find out which particular
//   FgQcPlot is associated with the given SeisPlot.
  FgQcPlot *found_lfp = 0, *lfp = 0;
  PlotList *plist = &(_qcp->_pop->_plist);
  int found_sp = 0;
  for (lfp = plist->top(); lfp && !found_sp; lfp = plist->next()) {
    if ((SeisPlot *)lfp->sp() == sp) {
      found_sp = 1;
      found_lfp = lfp;
    }
  }

  if (found_sp) {
    if (_qcp->_plot_class->getImageCoordinateSystem() !=
        found_lfp->_plot_class->getImageCoordinateSystem()) {
// the coordinate system is not consistent, tell user this SeisPlot is rejected
      new SLErrorPop (topWidget(), "Compute Grid Error",
        "The coordinate system must be consistent for all selected plots");
      return 0;
    }

// note:  _select_inputs_button is 1-relative, it should be used as 0-relative
    if ((FgQcPlot *)_fcg->getInputGrid(_select_inputs_button-1) == found_lfp) {
// case where the selection did not change, but there is no longer a need to
//   highlite a input selection button
      _select_inputs_button = -1;
      return 1;
    }
    else if (_fcg->getSeisPlot(_select_inputs_button-1))
// case where the selection obsoleted a previous selection
      delSeisPlot (_fcg->getSeisPlot(_select_inputs_button-1));

// store the corresponding (FgQcPlot *) at the currently selected inputs
//   button
    _fcg->setInputGrid (found_lfp, _select_inputs_button-1);
    addSeisPlot (sp);  // add this SeisPlot to the inform list

// no longer a need to highlite a input selection button
    _select_inputs_button = -1;
    return 1;
  }
  else
    return 0;
}

void FgQcComputeGridPop::installPicking ()
{
// using the PlotList of FgQcPlot's found in the FgQcPop, install a picker
//   on each associated SeisPlot.
  FgQcPlot *lfp = 0;
  PlotList *plist = &(_qcp->_pop->_plist);

// check for old array of picker pointers
  if (_pickers) {
    int k2;
    for (k2 = 0; k2 < _picker_count; k2++)
      if (_pickers[k2]) delete _pickers[k2], _pickers[k2] = 0;
    delete _pickers, _pickers = 0;
  }
  if (_seis_plots) delete _seis_plots, _seis_plots = 0;

// count the number of FgQcPlot's excluding this one
  _picker_count = 0;
  for (lfp = plist->top(); lfp; lfp = plist->next())
    if (lfp != _qcp) _picker_count++;

  if (_picker_count == 0) return;
  _pickers = new ComputeGridPicker *[_picker_count];
  if (!_pickers) {
    _error_status = FCGP_MEMORY_ALLOCATION_ERROR;
    return;
  }

  _seis_plots = new SeisPlot *[_picker_count];
  if (!_seis_plots) {
    _error_status = FCGP_MEMORY_ALLOCATION_ERROR;
    return;
  }

// install a picker on each SeisPlot excluding the one associated with this one
  int count = 0;
  for (lfp = plist->top(); lfp; lfp = plist->next()) {
    if (lfp != _qcp) {
      _pickers[count] = new ComputeGridPicker ((SeisPlot *)lfp->sp(),
        (ComputeGridPop *)this);
      if (!_pickers[count]) {
        _error_status = FCGP_MEMORY_ALLOCATION_ERROR;
        return;
      }
      _seis_plots[count] = (SeisPlot *)lfp->sp ();
      count++;
    }
  }
}

void FgQcComputeGridPop::removePicking ()
{
// delete old array of picker pointers including the pickers
  if (_pickers) {
    int k2;
    for (k2 = 0; k2 < _picker_count; k2++)
      if (_pickers[k2]) delete _pickers[k2], _pickers[k2] = 0;
    delete _pickers, _pickers = 0;
  }
  if (_seis_plots) delete _seis_plots, _seis_plots = 0;
  _picker_count = 0;
}

char *FgQcComputeGridPop::gridname (long which_variable)
{
  FgQcPlot *plot = (FgQcPlot *)_fcg->getInputGrid (which_variable);
  return plot->getTitle ();
}

int FgQcComputeGridPop::failed ()
{
  return (int)(_error_status != FCGP_SUCCESSFUL || ComputeGridPop::failed());
}

GridErrorCodes FgQcComputeGridPop::errorStatus ()
{
  if (ComputeGridPop::failed())
    return ComputeGridPop::errorStatus ();
  else
    return _error_status;
}

// called so as to inform that a SeisPlot has been destroyed
void FgQcComputeGridPop::destroyed (SeisPlot *sp)
{
// using the _seis_plots array, find out which picker (if any) should be gone
  int k2;
  if (_picker_count && _pickers && _seis_plots) {
    for (k2 = 0; k2 < _picker_count; k2++) {
      if (_seis_plots[k2] == sp) {
        if (_pickers[k2]) delete _pickers[k2], _pickers[k2] = 0;
        _seis_plots[k2] = 0;
      }
    }
  }

// based on the object pointers stored in FgQcComputeGrid, find out which
//   FgQcPLot's (if any) should be gone
  int count = _fcg->numberOfInputs ();
  for (k2 = 0; k2 < count; k2++)
    if (_fcg->getSeisPlot((long)k2) == sp) _fcg->setInputGrid (0, (long)k2);

// update the pop up menu shown to the user if necessary
    setDisplay ();
}

Boolean FgQcComputeGridPop::compressIsOK ()
{
  return (int)(_qcp->getNumColors() == _qcp->_pop->numberInPrivateColorMap());
}
