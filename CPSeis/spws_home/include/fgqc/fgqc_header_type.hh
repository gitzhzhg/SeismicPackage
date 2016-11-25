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
// fgqc_header_type.hh

#ifndef FGQC_HEADER_TYPE_HH
#define FGQC_HEADER_TYPE_HH

#include "fgqc/fgqc_plot_type.hh"

class FgQcHeaderType : public FgQcPlotType {

public:
  FgQcHeaderType				// Constructor
    (FgQcPlot *fgqc_plot);			//   FgQcPlot ptr

  virtual ~FgQcHeaderType ();			// Destructor

  int ValuesChanged				// what action when values chng
    (FieldGeometry *fg,				//   field geometry pointer
     long ixl,					//   line index?
     int what_changed,				//   identity of what chngd
     long index,				//   index of item changed?
     long nrem,					//   number that remain?
     long nins);				//   number input?

  int plot ();					// get data for plot

  int editData ();				// edit exiting plot

  static long computeNumPoints			// compute the number of points
    (float xmin,				//   X-minimum range for plot
     float xmax,				//   X-maximum range for plot
     float ymin,				//   Y-minimum range for plot
     float ymax,				//   Y-maximum range for plot
     FieldGeometry *fg,				//   field geometry pointer
     long coord_system,				//   coordinate sys for plot
     long data_loc,				//   where to plot trace hdr
     DoAbort *do_abort);			//   obj to allow user abort

private:

protected:

};

#endif
