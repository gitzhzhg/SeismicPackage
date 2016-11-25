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
#ifndef GRID_ERROR_HANDLER_HH
#define GRID_ERROR_HANDLER_HH

#include "sl/sl_error_pop.hh"
#include "dp/grid_error_codes.hh"

class GridErrorHandler : public SLErrorPop
{

friend char *gridErrorMessage (GridErrorCodes error_status);

public:
  GridErrorHandler				// constructor
    (SLDelay *slparent,				// parent SLDelay object ptr
     char *name,				// name of error handler
     GridErrorCodes error_status);		// grid error status flag

  GridErrorHandler
    (Widget wparent,				// parent widget ptr
     char *name,				// name of error handler
     GridErrorCodes error_status);		// grid error status flag

  virtual ~GridErrorHandler ();			// destructor

private:

};

#endif
