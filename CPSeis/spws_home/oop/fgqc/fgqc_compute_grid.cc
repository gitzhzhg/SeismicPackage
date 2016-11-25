// fgqc_compute_grid.cc:  implementation file for field geomtry QC compute
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
//   grid class

#include "fgqc/fgqc_compute_grid.hh"
#include "fgqc/fgqc_plot.hh"
#include "fgqc/fgqc_plot_type.hh"
#include "sp/seis_plot.hh"
#include "dp/color_lut.hh"


#define EPSILON 1e-31

FgQcComputeGrid::FgQcComputeGrid (FgQcPlot *qcp) :
  ComputeGrid ((int)NUMBER_OF_INPUTS),
  _qcp            (qcp),
  _color_lut      (0),
  _do_lut_init    (0),
  _undo_lut       (0),
  _input_plots    (0),
  _seis_plots     (0),
  _error_status   (FCG_SUCCESSFUL)
{
  _color_lut = new ColorLUT ();
  if (!_color_lut) {
    _error_status = FCG_MEMORY_ALLOCATION_ERROR;
    return;
  }
  else if (_color_lut->failed()) {
    _error_status = _color_lut->errorStatus ();
    return;
  }

  _input_plots = new FgQcPlot *[NUMBER_OF_INPUTS];
  if (!_input_plots) {
    _error_status = FCG_MEMORY_ALLOCATION_ERROR;
    return;
  }

  _seis_plots = new SeisPlot *[NUMBER_OF_INPUTS];
  if (!_seis_plots) {
    _error_status = FCG_MEMORY_ALLOCATION_ERROR;
    return;
  }

  int k2;
  for (k2 = 0; k2 < NUMBER_OF_INPUTS; k2++) {
    _input_plots[k2] = 0;
     _seis_plots[k2] = 0;
  }
}

FgQcComputeGrid::~FgQcComputeGrid ()
{
  if (_undo_lut) {
    _color_lut->resetSeisPlotLUT();
    _qcp->getFgColorPop()->assignColorLUT (0);
  }
  if (_color_lut) delete _color_lut;
}

void FgQcComputeGrid::setInputGrid (void *data, long which_input)
{
  FgQcPlot *plot = (FgQcPlot *)data;
  _input_plots[validateIndex((int)which_input)] = plot;
  if (!plot)
    _seis_plots[validateIndex((int)which_input)] = 0;
  else
    _seis_plots[validateIndex((int)which_input)] = (SeisPlot *)plot->sp ();

// inform the FgQcComputeGrid that the inputs have changed
  inputsChanged ();
}

void *FgQcComputeGrid::getInputGrid (long which_input)
{
  return (void *)_input_plots[validateIndex((int)which_input)];
}

SeisPlot *FgQcComputeGrid::getSeisPlot (long which_input)
{
  return _seis_plots[validateIndex((int)which_input)];
}

int FgQcComputeGrid::initializeDisplay ()
{
  if (!_color_lut->setSeisPlot (_qcp->sp())) {
    _error_status = _color_lut->errorStatus ();
    return 0;
  }
  _do_lut_init = 1;
  return 1;
}

int FgQcComputeGrid::gridValid (long which_input)
{
  if (!_input_plots[validateIndex((int)which_input)]) return (int)0;
  if (!getInputFloatGrid((int)which_input)) return (int)0;
  if (_input_plots[validateIndex((int)which_input)]->getFgColorPop()->
    colorLUTIsSet()) return (int)0;
  return 1;
}

int FgQcComputeGrid::failed ()
{
  return (int)(_error_status != FCG_SUCCESSFUL || ComputeGrid::failed());
}

GridErrorCodes FgQcComputeGrid::errorStatus ()
{
  if (ComputeGrid::failed())
    return ComputeGrid::errorStatus ();
  else
    return _error_status;
}

// display the result through the use of LUTs
int FgQcComputeGrid::displayLUT ()
{
  if (_do_lut_init) {
// initialize LUT operations
//   get colors currently being used
    if (!_color_lut->getSeisPlotLUT()) {
      _error_status = _color_lut->errorStatus ();
      return 0;
    }
// establish the resultant decoder with the colors used
    if (!_color_lut->setResultLUT(_result_LUT, (Decoder *)_ma)) {
      _error_status = _color_lut->errorStatus ();
      return 0;
    }
//   tell the color pop what LUT to use
    _qcp->getFgColorPop()->assignColorLUT (_color_lut);
  }
  else {
// subsequent LUT operations
//   store the new result LUT in the color LUT attribute array
    if (!_color_lut->reloadResultLUT()) {
      _error_status = _color_lut->errorStatus ();
      return 0;
    }
  }

// assign the RGB's to the display
  if (!_color_lut->setResult()) {
    _error_status = _color_lut->errorStatus ();
    return 0;
  }

  if (_do_lut_init) {
// first time through display the pixels
    if (!_qcp->_plot_class->postPlot()) {
      _error_status = FCG_PLOTTING_ERROR;
      return 0;
    }
    _do_lut_init = 0;
    _undo_lut = 1;
    _qcp->sp()->drawColorBarOnHardCopy(SeisPlot::Off);
  }

  return 1;
}

// display the result using the input grids themselves
int FgQcComputeGrid::displayGrid ()
{
  if (_undo_lut) {
    _color_lut->resetSeisPlotLUT();
    _qcp->getFgColorPop()->assignColorLUT (0);
    _qcp->sp()->drawColorBarOnHardCopy(SeisPlot::On);
  }

  if (!_qcp->_plot_class->postPlot()) {
    _error_status = FCG_PLOTTING_ERROR;
    return 0;
  }

  _undo_lut = 0;
  _do_lut_init = 1;
  return 1;
}

// this call assumes that validation was already done on _input_plots[]
FloatGrid *FgQcComputeGrid::getInputFloatGrid (int index)
{
  if (!_input_plots[validateIndex(index)]) return 0;
  return _input_plots[validateIndex(index)]->_plot_class->getFloatGrid ();
}

FloatGrid *FgQcComputeGrid::getResultFloatGrid ()
{
  return _qcp->_plot_class->getFloatGrid ();
}

int FgQcComputeGrid::getNumColors ()
{
  return _qcp->getNumColors ();
}

float FgQcComputeGrid::resultMinAmp ()
{
  return _qcp->sp()->minColorAmp ();
}

float FgQcComputeGrid::resultMaxAmp ()
{
  return _qcp->sp()->maxColorAmp ();
}

// this call assumes that validation was already done on _input_plots[]
float FgQcComputeGrid::inputX0 (int index)
{
  if (!_input_plots[validateIndex(index)]) return 0;
  return _input_plots[validateIndex(index)]->getUserLeft();
}

// this call assumes that validation was already done on _input_plots[]
float FgQcComputeGrid::inputX1 (int index)
{
  if (!_input_plots[validateIndex(index)]) return 0;
  return _input_plots[validateIndex(index)]->getUserRight();
}

// this call assumes that validation was already done on _input_plots[]
float FgQcComputeGrid::inputY0 (int index)
{
  if (!_input_plots[validateIndex(index)]) return 0;
  return _input_plots[validateIndex(index)]->getUserTop();
}

// this call assumes that validation was already done on _input_plots[]
float FgQcComputeGrid::inputY1 (int index)
{
  if (!_input_plots[validateIndex(index)]) return 0;
  return _input_plots[validateIndex(index)]->getUserBottom();
}

float FgQcComputeGrid::resultX0 ()
{
  return _qcp->getUserLeft();
}

float FgQcComputeGrid::resultX1 ()
{
  return _qcp->getUserRight();
}

float FgQcComputeGrid::resultY0 ()
{
  return _qcp->getUserTop();
}

float FgQcComputeGrid::resultY1 ()
{
  return _qcp->getUserBottom();
}
