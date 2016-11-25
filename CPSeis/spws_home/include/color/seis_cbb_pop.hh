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
// class that creates the SeisPlot color bar builder menu
#ifndef SEIS_CBB_POP_HH
#define SEIS_CBB_POP_HH

#include "color/color_bar_builder_pop.hh"
#include "sp/seis_inform.hh"

class SeisPlot;
class SPList;
class CBBRGBSet;
class SeisColorFileIO;
class SeisColor;

class SeisCBBPop :  public ColorBarBuilderPop, public SeisInform {

public:
  SeisCBBPop					// constructor
    (Widget parent,				//   parent widget
     char *name,				//   name
     HelpCtx hctx,				//   context sensitive help
     SeisPlot *sp,				//   seis plot
     int max_levels,				//   maximum color levels
     SeisColorFileIO *fio,			//   seismic color file I/O
     SeisColor *scr);				//   seisColor object

  SeisCBBPop					// constructor
    (SLDelay *container,			//   container
     char *name,				//   name
     HelpCtx hctx,				//   context sensitive help
     SeisPlot *sp,				//   seis plot
     int max_levels,				//   maximum color levels
     SeisColorFileIO *fio,			//   seismic color file I/O
     SeisColor *scr);				//   seisColor object

  virtual ~SeisCBBPop ();			// destructor
  
  virtual Widget make				// make function
    (Widget parent);				//   parent widget

  SeisPlot *SP ()				// return local SeisPlot obj
    { return _sp; }

  virtual void addSP				// add a SeisPlot to the list
    (SeisPlot *sp);				//   given SeisPlot

  virtual void notCurrentInWindow		// mrk SP not currnt in window
    (SeisPlot *sp);				//   given SeisPlot

  virtual void destroyed			// mark SeisPlot destroyed
    (SeisPlot *sp);				//   given SeisPlot

  virtual void newPlot				// called when new plot made
    (SeisPlot *sp);				//   given SeisPlot

  virtual float *getLUT ();			// rtn the SeisPlot LUT

  virtual int getLUTSize ();			// get the SeisPlot LUT size

  virtual Boolean LUTChanged			// True if color LUT chnged
    (CBBRGBSet *rgb);				//   givn the obj to put it in

  virtual Boolean LUTNotEqualToFile		// True if user chnged input
    (char *filename,				//   given the file to chk
     CBBRGBSet *rgb);				//   givn the LUT to check

  virtual void getAmps ();			// get the SeisPlot amplitudes

  virtual Boolean ampsChanged ();		// True if amplitudes chnged

protected:
  virtual void init ();				// constructor helper

  SeisPlot
    *_sp;					// current SeisPlot

  SeisColor
    *_scr;					// SeisColor object

  SPList
    *_list;					// linked list of SeisPlot's

};

#endif
