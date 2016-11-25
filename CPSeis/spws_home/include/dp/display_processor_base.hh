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
// display_processor_base.hh:  User I/F file for DisplayProcessorBase class
#ifndef DISPLAY_PROCESSOR_BASE_HH
#define DISPLAY_PROCESSOR_BASE_HH

#include "dp/rgbz_generator.hh"
#include "dp/decoder.hh"
#include "dp/n_tree.hh"
#include "dp/grid_error_codes.hh"

class NTree;

class DisplayProcessorBase : public RGBZGenerator, public Decoder {

public:
  virtual ~DisplayProcessorBase ();		// destructor

// virtual function
  virtual unsigned char *getArray ()		// returns unsigned char array*
    { return (unsigned char *)0; }

// pure virtual function
  virtual int prepareUse () = 0;		// prepare the Z-LUT for use

// pure virtual function
 virtual void getOneCode			// returns one RGBZ decoder elm
    (int index,					//   RGBZ decoder index to get
     float *red,				//   red decoder value
     float *green,				//   green decoder value
     float *blue,				//   blue decoder value
     float *attribute) = 0;			//   attribute decoder value

// pure virtual function
  virtual void getNextCode			// returns nxt RGBZ decoder elm
    (float *red,				//   red decoder value
     float *green,				//   green decoder value
     float *blue,				//   blue decoder value
     float *attribute) = 0;			//   attribute decoder value

// virtual function
  virtual int execute				// work on region of grid
    (int /* first_column */,			//   first column to modify
     int /* first_row */,			//   first row to modify
     int /* column_count */,			//   # of columns to modify
     int /* row_count */)			//   # of rows to modify
    { return (int)0; }

  int failed ();				// return 1 if unsuccessful

  GridErrorCodes errorStatus ();		// return error status flag

protected:
  DisplayProcessorBase				// constructor
    (int coding_levels);			//   number of levels for codin

  virtual void reset ();			// initialize object

// pure virtual function
  virtual void computeElement			// compute a decoder element
    (float *red,				//   address of red value
     float *green,				//   address of green value
     float *blue,				//   address of blue value
     float *attribute) = 0;			//   address of attribute value

// virtual function
  virtual int preModify				// prep chng subregion of grid
    (int /* first_column */,			//   first column to modify
     int /* first_row */,			//   first row to modify
     int /* column_count */,			//   # of columns to modify
     int /* row_count */)			//   # of rows to modify
    { return (int)0; }

// virtual function
  virtual int modify ()				// change subregion of grid
    { return (int)0; }

  NTree
    *_n_tree;					// N tree compress distribution

  int
    _needs_initializing;			// initializing switchh (T=1)

  unsigned char
    *_temp_array;				// temp array to hold DS result

  GridErrorCodes
    _error_status;				// error status flag

private:
  void reclaimMemory ();			// delete arrays and objects

};

#endif
