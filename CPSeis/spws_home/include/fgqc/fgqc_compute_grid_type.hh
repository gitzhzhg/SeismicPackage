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
// fgqc_compute_grid_type.hh: user interface file for FgQcComputeGridType class

#ifndef FGQC_COMPUTE_GRID_TYPE_HH
#define FGQC_COMPUTE_GRID_TYPE_HH

#include "fgqc/fgqc_plot_type.hh"

class FgQcComputeGrid;
class FgQcComputeGridPop;

class FgQcComputeGridType : public FgQcPlotType {
  friend class FgQcComputeGrid;

  public:
    FgQcComputeGridType				// constructor
      (FgQcPlot *fgqc_plot);

    virtual ~FgQcComputeGridType ();		// destructor

    int ValuesChanged				// called when fg values chng
      (FieldGeometry *fg,			//   FieldGeometry object ptr
       long ixl,				//   seismic line
       int ident,				//   what changed
       long index,				//   index identified
       long nrem,				//   # to remove
       long nins);				//   # to insert

    int plot ();				// plot the data (begin)

    virtual int postPlot ();			// call after plot (finish)

    int editData ();				// edit the data plotted

  private:
    Boolean checkGridStatus			// check grid obj error status
      (long check_type,				//   identity of object to chk
       int dont_post=0);			//   enter 1 to not post error

    FgQcComputeGrid
      *_compute_grid;				// compute grid object pointer

    FgQcComputeGridPop
      *_compute_grid_pop;			// compute grid popup obj ptr 
};

#endif
