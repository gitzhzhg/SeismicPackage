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
// fgqc_trace_counter.hh

#ifndef FGQC_TRACE_COUNTER_HH
#define FGQC_TRACE_COUNTER_HH

class FgQcTraceCounter {
  private:
    long
      _data_loc,				// DataLocation indicator
      _coord_system;				// plot coord system indicator

    class FieldGeometry *_fg;			// ptr to FieldGeometry object

    float
      _xmin,					// plot X-coordinate minimum
      _xmax,					// plot X-coordinate maximum
      _ymin,					// plot Y-coordinate minimum
      _ymax;					// plot Y-coordinate maximum

  public:
    FgQcTraceCounter				// constructor
      (long data_loc,				//   DataLocation indicator
       class FgQcPop *pop);			//   pntr to FgQcPop object

    ~FgQcTraceCounter ();

    char *questionText ();			// returns question box string

    long computation ();			// rtrns number of plot traces

};

#endif
