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
// Field geometry QC compute grid class

#ifndef FGQC_COMPUTE_GRID_HH
#define FGQC_COMPUTE_GRID_HH

#include "fgqc/fgqc_compute_grid.hh"
#include "dp/compute_grid.hh"
#include "dp/grid_error_codes.hh"

#define NUMBER_OF_INPUTS 2

class FgQcPlot;
class SeisPlot;
class ColorLUT;

class FgQcComputeGrid : public ComputeGrid {

public:
  FgQcComputeGrid				// constructor
    (FgQcPlot *qcp);				//   fg qc plot

  virtual ~FgQcComputeGrid ();			// destructor

  virtual void setInputGrid			// set a new input grid
    (void *data,				//   object where grid is found
     long which_input);				//   which input is being set

  virtual void *getInputGrid			// get objct where inpt grid is
    (long which_input);				//   which input is being rtnd

  SeisPlot *getSeisPlot				// get SeisPlot obj assoc w/inp
    (long which_input);				//   which input where sp is

  int initializeDisplay ();			// initialize color LUT

  virtual int gridValid				// rtn 1 if grid is valid at
    (long which_input);				//   given index

  int failed ();				// return 1 if unsuccessful

  GridErrorCodes errorStatus ();		// return error status flag

protected:
  virtual int displayLUT ();			// Displays the resultant LUT

  virtual int displayGrid ();			// Displays the resultant grid

  virtual FloatGrid *getInputFloatGrid		// get input float grid object
    (int index);				//   at given index

  virtual FloatGrid *getResultFloatGrid ();	// get result float grid object

  virtual int getNumColors ();			// get number of colors allowed

  virtual float resultMinAmp ();		// get minimum amplitude of res

  virtual float resultMaxAmp ();		// get maximum amplitude of res

  virtual float inputX0				// get first input X-coordinate
    (int index);				//   at given index

  virtual float inputX1				// get second input X-coordinat
    (int index);				//   at given index

  virtual float inputY0				// get first input Y-coordinate
    (int index);				//   at given index

  virtual float inputY1				// get second input Y-coordinat
    (int index);				//   at given index

  virtual float resultX0 ();			// get first result X-coordinat

  virtual float resultX1 ();			// get second result X-coordina

  virtual float resultY0 ();			// get first result Y-coordinat

  virtual float resultY1 ();			// get second result Y-coordina

private:
  FgQcPlot
    *_qcp,					// FgQcPlot obj to show result
    **_input_plots;				// array of plot pointers

  SeisPlot
    **_seis_plots;				// array of SeisPlot pointers

  ColorLUT
    *_color_lut;				// color look-up-table

  int
    _do_lut_init,				// if T(!=0), need to init LUT
    _undo_lut;					// if T(!=0) the LUT is enabled

  GridErrorCodes
    _error_status;				// error status flag

};

#endif
