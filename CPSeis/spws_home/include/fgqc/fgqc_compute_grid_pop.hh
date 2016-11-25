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
// Field geometry QC compute grid popup class

#ifndef FGQC_COMPUTE_GRID_POP_HH
#define FGQC_COMPUTE_GRID_POP_HH

#include "dp/compute_grid_pop.hh"
#include "sp/seis_inform.hh"
#include "dp/grid_error_codes.hh"

class SeisPlot;
class FgQcComputeGrid;
class FgQcPlot;
class ComputeGridPicker;

class FgQcComputeGridPop : public ComputeGridPop, public SeisInform {

public:
  FgQcComputeGridPop				// constructor
    (Widget p,					//   parent widget
     char *name,				//   name of popup
     FgQcComputeGrid *fcg,			//   fg qc compute grid
     FgQcPlot *qcp,				//   fg qc plot
     HelpCtx hctx);				//   context sensitive help

  virtual ~FgQcComputeGridPop ();			// destructor

  virtual int selectGrid			// select a grid knowing the sp
    (SeisPlot *sp);				//   given SeisPlot

  virtual void installPicking ();		// install pickers on all sp's

  virtual void removePicking ();		// remove pickers from all sp's

  virtual char *gridname			// returns input data name
    (long which_variable);			//   indicator of which input

  virtual void destroyed			// is called by SeisInform
    (SeisPlot *sp);				//   SeisPlot just destroyed

  virtual Boolean compressIsOK ();		// return True if OK to compres

  int failed ();				// return 1 if unsuccessful

  GridErrorCodes errorStatus ();		// returns error status flag

private:
  FgQcComputeGrid
    *_fcg;					// inpt FgQcComputeGrid obj ptr

  FgQcPlot
    *_qcp;					// input FgQcPlot object pointr

  ComputeGridPicker
    **_pickers;					// array of picker pointers

  SeisPlot
    **_seis_plots;				// array of SeisPlot pointers

  int
    _picker_count;				// # of pickers in array

  GridErrorCodes
    _error_status;				// error status flag

};

#endif
